package flame

import java.util.concurrent.atomic as juca

import soundness.*
import perihelion.*
import perihelion.given

import doms.html.whatwg.*
import Control.*

import anticipation.abstractables.durationAbstractable
import charEncoders.utf8Encoder
import classloaders.threadContextClassloader
import formatting.compactJsonFormatting
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.virtualThreading
import webserverErrorPages.minimalErrorPage

// A web-based front-end for the Flame REPL that mirrors the terminal client's live
// editing: every keystroke is sent to the server, which tokenizes the line with the same
// fast lexer the CLI uses (`Repl.tokenize`, no compiler) and returns the Harlequin accent
// of each token; the browser re-paints the line accordingly. Submitting a line runs it
// through the same typechecked engine (`Repl.react`) as the socket client, and the result
// is appended to a scrollback log. The editor cannot be a `<textarea>` (those can't carry
// styled spans), so it is a contenteditable `<code>` element driven by `replScript`.

// Messages exchanged with the browser as JSON over the WebSocket. Flat case classes (not
// enums) so the JSON shape — `{"kind":…,"seq":…,…}` — is predictable for the JavaScript.
case class WebRequest(kind: Text, seq: Int, code: Text, offset: Int)
case class WebToken(text: Text, accent: Text, role: Text = t"")
case class WebCompletion(name: Text, kind: Text, signature: Text)

case class WebReply
   ( kind:        Text,
     seq:         Int,
     value:       Text,
     tpe:         Text,
     output:      Text,
     diagnostics: Text,
     tokens:      List[WebToken],
     completions: List[WebCompletion] = Nil,
     incomplete:  Boolean = false,
     name:        Text = t"" )

// The browser-side editor and styling. No `$` (it would interpolate) and `\n` is written
// `\\n` so the served script carries a real newline escape. Selects its two controls by
// tag — the page has exactly one `<pre>` (the log) and one `<code>` (the editor).
val replScript: Text = t"""
(function() {
  var css = [
    // JetBrains Mono, loaded from Google Fonts. An `@import` must be the first rule in the sheet.
    "@import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&display=swap');",
    "body { font-family: 'JetBrains Mono', ui-monospace, SFMono-Regular, Menlo, monospace; margin: 1rem;",
    "  background: #1e1e1e; color: #d4d4d4; }",
    "h1 { font-size: 1.1rem; font-weight: normal; color: #d7ba7d; }",
    // `pre`/`code` carry the UA's own monospace font, so make them inherit `body`'s JetBrains Mono.
    "pre { font-family: inherit; white-space: pre-wrap; word-break: break-word; margin: 0 0 0.5rem; }",
    "code { font-family: inherit; display: block; white-space: pre-wrap; word-break: break-word; outline: none;",
    "  border: 1px solid #3a3a3a; border-radius: 4px; padding: 0.4rem 0.6rem;",
    "  min-height: 1.4em; background: #252526; caret-color: #d4d4d4; }",
    "code::after { content: attr(data-ghost); color: #5c6370; }",
    ".prompt { color: #808080; }",
    ".result { color: #b5cea8; }",
    ".error { color: #f48771; white-space: pre-wrap; }",
    ".notice { color: #e5c07b; font-weight: bold; white-space: pre-wrap; }",
    ".pending { color: #808080; font-style: italic; }",
    ".completions { position: fixed; z-index: 20; background: #252526; border: 1px solid #454545;",
    "  max-height: 14em; overflow-y: auto; box-shadow: 0 2px 10px rgba(0,0,0,0.5); }",
    ".completions div { padding: 1px 10px 1px 8px; white-space: pre; cursor: default; }",
    ".completions div.sel { background: #094771; }",
    ".citem-sig { color: #808080; margin-left: 1.5em; }",
    ".status { position: fixed; top: 8px; right: 10px; z-index: 30; font-size: 0.8em;",
    "  padding: 2px 9px; border-radius: 10px; border: 1px solid transparent; }",
    ".status.ok { background: #14321a; color: #7ec77e; border-color: #2c5c34; }",
    ".status.warn { background: #3a341d; color: #e5c07b; border-color: #6b5e30; }",
    "code.offline { border-color: #7a3a3a; opacity: 0.65; }",
    ".tok-keyword { color: #569cd6; }",
    ".tok-modifier { color: #569cd6; }",
    ".tok-string { color: #ce9178; }",
    ".tok-number { color: #b5cea8; }",
    ".tok-typal { color: #4ec9b0; }",
    ".tok-term { color: #9cdcfe; }",
    ".tok-symbol { color: #d4d4d4; }",
    ".tok-parens { color: #ffd700; }",
    ".tok-error { color: #f48771; text-decoration: underline; }",
    ".tok-unparsed { color: #6a9955; }",
    // A `/`-command line is highlighted specially (see `makeSpans`): the command word in gold, and its
    // parameters in a fainter, more-yellow colour so they recede behind the command.
    ".tok-command { color: #d7ba7d; }",
    ".tok-command-param { color: #a1843c; }",
    // A term/type token's role is a second class: italicise bindings (a `val`/`def`/param or
    // pattern name, a class/type definition, or a type parameter) on top of the accent's colour.
    ".binding { font-style: italic; }"
  ].join("\\n");
  var styleEl = document.createElement("style");
  styleEl.textContent = css;
  document.head.appendChild(styleEl);

  var log = document.querySelector("pre");
  var editor = document.querySelector("code");
  // The page is pretty-printed, so the empty elements arrive holding indentation
  // whitespace; clear it so the editor and log start genuinely empty.
  log.innerHTML = "";
  editor.innerHTML = "";
  editor.contentEditable = "true";
  editor.spellcheck = false;

  // A live connection-state indicator, fixed in the corner.
  var indicator = document.createElement("div");
  document.body.appendChild(indicator);

  var socket;
  var seq = 0;
  var serverInstance = null;
  var sessionName = null;
  var connected = false;
  var everConnected = false;
  var reconnectTimer = null;

  // Shell-style input history. Each entry caches the submitted line's highlighted markup
  // (`html`) so recall is instant and needs no re-tokenize, plus its plain `text` for
  // de-duplication. `histIdx === history.length` means "the current draft" (not in history);
  // `draft` holds the markup of what was being typed before navigating up.
  var history = [];
  var histIdx = 0;
  var draft = "";

  function setStatus(state) {
    if (state === "connected") {
      indicator.className = "status ok";
      indicator.textContent = "connected";
      editor.classList.remove("offline");
    } else {
      indicator.className = "status warn";
      indicator.textContent = everConnected ? "reconnecting..." : "connecting...";
      editor.classList.add("offline");
    }
  }

  function send(kind, code, offset) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      var s = ++seq;
      if (kind === "tokenize") tokenizeCode[s] = code;
      socket.send(JSON.stringify({ kind: kind, seq: s, code: code, offset: offset || 0 }));
      return s;
    }
    return -1;
  }

  // Inline autosuggestion ("ghost text", fish-style): the remainder of a unique completion,
  // or the common stem of several (then suffixed "..."), shown faint after the cursor via the
  // `code::after` rule reading `data-ghost`. It is never part of `textContent`, so it never
  // reaches `tokenize`/`submit`. `ghostSeq`/`popupSeq` tell a ghost reply from a Tab reply;
  // `prevText` lets `input` tell an appended character from any other edit.
  var ghost = "";
  var ghostSeq = -1;
  var popupSeq = -1;
  var ghostTimer = null;
  var prevText = "";

  // The engine's latest "is this an incomplete prefix?" verdict and the text it was for,
  // plus the text each in-flight `tokenize` seq was sent for, so Enter can decide instantly.
  var incomplete = false;
  var incompleteText = null;
  var tokenizeCode = {};

  // Bracket-balance fallback used only until the engine's verdict for the current text lands:
  // non-empty with every bracket closed (ignores strings/comments, hence "looks").
  function looksComplete(text) {
    if (!text.trim()) return false;
    var depth = 0;
    for (var i = 0; i < text.length; i++) {
      var c = text.charAt(i);
      if (c === "(" || c === "[" || c === "{") depth++;
      else if (c === ")" || c === "]" || c === "}") depth--;
    }
    return depth <= 0;
  }

  function setGhost(text) {
    ghost = text || "";
    if (ghost) editor.setAttribute("data-ghost", ghost);
    else editor.removeAttribute("data-ghost");
  }

  function atEnd() { return getCaret() === editor.textContent.length && editor.textContent.length > 0; }

  function applyGhost(candidates) {
    if (!atEnd() || !candidates || !candidates.length) { setGhost(""); return; }
    var isSlash = candidates[0].name.charAt(0) === "/";
    var stem = currentStem(isSlash);
    if (candidates.length === 1) {
      var name = candidates[0].name;
      setGhost(name.length > stem.length && name.indexOf(stem) === 0 ? name.slice(stem.length) : "");
    } else {
      var common = longestCommonPrefix(candidates.map(function(c) { return c.name; }));
      setGhost(common.length > stem.length && common.indexOf(stem) === 0 ? common.slice(stem.length) + "..." : "");
    }
  }

  function scheduleGhost() {
    if (ghostTimer) clearTimeout(ghostTimer);
    ghostTimer = setTimeout(function() {
      ghostTimer = null;
      if (atEnd()) ghostSeq = send("complete", editor.textContent, getCaret());
      else setGhost("");
    }, 200);
  }

  function getCaret() {
    var sel = window.getSelection();
    if (!sel.rangeCount) return 0;
    var range = sel.getRangeAt(0);
    var pre = range.cloneRange();
    pre.selectNodeContents(editor);
    pre.setEnd(range.endContainer, range.endOffset);
    return pre.toString().length;
  }

  function setCaret(offset) {
    var walker = document.createTreeWalker(editor, NodeFilter.SHOW_TEXT, null);
    var node, remaining = offset, target = null, targetOffset = 0;
    while ((node = walker.nextNode())) {
      var len = node.textContent.length;
      if (remaining <= len) { target = node; targetOffset = remaining; break; }
      remaining -= len;
    }
    var sel = window.getSelection();
    var range = document.createRange();
    if (target) range.setStart(target, targetOffset);
    else { range.selectNodeContents(editor); range.collapse(false); }
    range.collapse(true);
    sel.removeAllRanges();
    sel.addRange(range);
  }

  function makeSpans(parent, tokens) {
    // A `/`-command line is a REPL command, not Scala, so it is coloured specially rather than by the
    // Scala accents: the command word (before the first space) reads as a command, and its parameters
    // (everything after) are shown fainter and more yellow. `sp` is the first space; a token is a
    // parameter when it begins at or after it.
    var full = "";
    for (var i = 0; i < tokens.length; i++) full += tokens[i].text;
    var slash = full.charAt(0) === "/";
    var sp = slash ? full.indexOf(" ") : -1;
    var off = 0;
    for (var i = 0; i < tokens.length; i++) {
      var span = document.createElement("span");
      if (slash) span.className = (sp !== -1 && off >= sp) ? "tok-command-param" : "tok-command";
      // The accent is the colour class; a term/type token's role (binding/usage) is a second
      // class, so the stylesheet can e.g. italicise .binding on top of the accent's colour.
      else span.className = "tok-" + tokens[i].accent + (tokens[i].role ? " " + tokens[i].role : "");
      span.textContent = tokens[i].text;
      parent.appendChild(span);
      off += tokens[i].text.length;
    }
  }

  function highlight(tokens) {
    var text = "";
    for (var i = 0; i < tokens.length; i++) text += tokens[i].text;
    // Only re-paint when the tokens describe the line as it stands now; a newer keystroke
    // already has a fresh tokenize in flight, so stale tokens are dropped (and fast typing
    // is naturally debounced — the paint lands once the typist pauses).
    if (text !== editor.textContent) return;
    var caret = getCaret();
    editor.innerHTML = "";
    makeSpans(editor, tokens);
    setCaret(caret);
  }

  function logBlock(cls, text) {
    if (!text) return;
    var div = document.createElement("div");
    if (cls) div.className = cls;
    div.textContent = text;
    log.appendChild(div);
  }

  // A result value is server-rendered HTML (the engine's HTML typeclass cascade), so it is inserted
  // as innerHTML, not text. The value itself is already HTML-safe (honeycomb-escaped server-side).
  function logHtml(cls, html) {
    if (!html) return;
    var div = document.createElement("div");
    if (cls) div.className = cls;
    div.innerHTML = html;
    log.appendChild(div);
  }

  function escapeHtml(s) {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }

  // Renders a result/error reply INTO `target` — output, then the value line (`name = value : type`,
  // the value already server-rendered HTML, the rest escaped), then diagnostics. Shared by the normal
  // result path and the async fill so a placeholder fills identically to an inline result.
  function fillResult(target, msg) {
    var html = "";
    var out = msg.output;
    if (out) { if (out.charAt(out.length - 1) === "\\n") out = out.slice(0, -1); }
    if (out) html += "<div>" + escapeHtml(out) + "</div>";
    if (msg.value) {
      var line = msg.value;
      if (msg.name) line = escapeHtml(msg.name) + " = " + line;
      if (msg.tpe) line += " : " + escapeHtml(msg.tpe);
      html += "<div class='result'>" + line + "</div>";
    }
    if (msg.diagnostics) html += "<div class='error'>" + escapeHtml(msg.diagnostics) + "</div>";
    target.innerHTML = html;
  }

  function submit() {
    // Refuse to send while disconnected: don't echo or clear the editor, so the user keeps
    // their input until the connection is back (the indicator shows why nothing happened).
    if (!connected) return;
    var code = editor.textContent;
    if (code.trim() === "") return;
    var line = document.createElement("div");
    var prompt = document.createElement("span");
    prompt.className = "prompt";
    prompt.textContent = "> ";
    line.appendChild(prompt);
    var node = editor.firstChild;
    while (node) { line.appendChild(node.cloneNode(true)); node = node.nextSibling; }
    log.appendChild(line);
    // Cache the submitted line (with its highlighting) for history recall, skipping a
    // consecutive duplicate; reset the cursor back to the newest position.
    if (!history.length || history[history.length - 1].text !== code)
      history.push({ html: editor.innerHTML, text: code });
    histIdx = history.length;
    draft = "";
    send("submit", code);
    editor.innerHTML = "";
    setGhost("");
    prevText = "";
    log.scrollIntoView(false);
  }

  // Replace the editor with cached markup (a history entry or the saved draft) and put the
  // caret at the end. Setting innerHTML directly fires no `input`, so the cached highlighting
  // stands until the user edits (which re-tokenizes as usual).
  function showHistory(html) {
    editor.innerHTML = html;
    var r = document.createRange();
    r.selectNodeContents(editor);
    r.collapse(false);
    var s = window.getSelection();
    s.removeAllRanges();
    s.addRange(r);
    setGhost("");
    prevText = editor.textContent;
  }

  function historyUp() {
    if (!history.length) return;
    if (histIdx === history.length) draft = editor.innerHTML;
    if (histIdx > 0) { histIdx--; showHistory(history[histIdx].html); }
  }

  function historyDown() {
    if (histIdx >= history.length) return;
    histIdx++;
    showHistory(histIdx === history.length ? draft : history[histIdx].html);
  }

  function insertNewline() {
    var sel = window.getSelection();
    if (!sel.rangeCount) return;
    // Auto-indent: when the caret is at the end of the current line (nothing to its right on that
    // line, so the newline isn't splitting text), start the new line with the same leading whitespace
    // as the line being left — matching the CLI front-end.
    var text = editor.textContent, caret = getCaret();
    var after = text.slice(caret);
    var indent = "";
    if (after === "" || after.charAt(0) === "\\n") {
      var before = text.slice(0, caret);
      var line = before.slice(before.lastIndexOf("\\n") + 1);
      var m = line.match(/^[ \\t]*/);
      indent = m ? m[0] : "";
    }
    var range = sel.getRangeAt(0);
    range.deleteContents();
    var nl = document.createTextNode("\\n" + indent);
    range.insertNode(nl);
    range.setStartAfter(nl);
    range.collapse(true);
    sel.removeAllRanges();
    sel.addRange(range);
  }

  // --- Tab completion (asks the server, shows a popup, inserts the chosen name) ---
  var popup = null;
  var items = [];
  var selIdx = 0;

  function isIdentChar(c) {
    return (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9") || c === "_";
  }

  function hideCompletions() {
    if (popup) { popup.remove(); popup = null; }
    items = [];
  }

  function insertCompletion(name) {
    var next, caretPos;
    if (name.charAt(0) === "/") {
      // A /-command candidate is the whole command; replace the entire line.
      next = name;
      caretPos = name.length;
    } else {
      // A code candidate replaces the partial identifier ending at the caret.
      var text = editor.textContent;
      var caret = getCaret();
      var start = caret;
      while (start > 0 && isIdentChar(text.charAt(start - 1))) start--;
      next = text.slice(0, start) + name + text.slice(caret);
      caretPos = start + name.length;
    }
    editor.textContent = next;
    setCaret(caretPos);
    setGhost("");
    prevText = next;
    send("tokenize", next);
  }

  function paintSelection() {
    for (var i = 0; i < popup.children.length; i++)
      popup.children[i].className = (i === selIdx) ? "sel" : "";
    if (popup.children[selIdx]) popup.children[selIdx].scrollIntoView({ block: "nearest" });
  }

  function acceptCompletion() {
    if (items.length) insertCompletion(items[selIdx].name);
    hideCompletions();
  }

  function moveCompletion(delta) {
    if (!items.length) return;
    selIdx = (selIdx + delta + items.length) % items.length;
    paintSelection();
  }

  function longestCommonPrefix(names) {
    if (!names.length) return "";
    var prefix = names[0];
    for (var i = 1; i < names.length; i++) {
      var s = names[i], j = 0;
      while (j < prefix.length && j < s.length && prefix.charAt(j) === s.charAt(j)) j++;
      prefix = prefix.slice(0, j);
      if (!prefix) break;
    }
    return prefix;
  }

  // The stem already typed (so we only advance, never shorten): the whole line for a
  // /-command, otherwise the partial identifier ending at the caret.
  function currentStem(isSlash) {
    var text = editor.textContent;
    if (isSlash) return text;
    var caret = getCaret(), start = caret;
    while (start > 0 && isIdentChar(text.charAt(start - 1))) start--;
    return text.slice(start, caret);
  }

  function showCompletions(list) {
    if (!list || !list.length) { hideCompletions(); return; }
    if (list.length === 1) { insertCompletion(list[0].name); hideCompletions(); return; }
    // Several candidates: first extend the stem to the longest prefix common to them all
    // (e.g. "pri" -> "print" when the options are print/println), then show the popup.
    var isSlash = list[0].name.charAt(0) === "/";
    var common = longestCommonPrefix(list.map(function(c) { return c.name; }));
    var stem = currentStem(isSlash);
    if (common.length > stem.length && common.indexOf(stem) === 0) insertCompletion(common);
    hideCompletions();
    items = list;
    selIdx = 0;
    popup = document.createElement("div");
    popup.className = "completions";
    for (var i = 0; i < items.length; i++) {
      var row = document.createElement("div");
      var nm = document.createElement("span");
      nm.textContent = items[i].name;
      row.appendChild(nm);
      if (items[i].signature) {
        var sg = document.createElement("span");
        sg.className = "citem-sig";
        sg.textContent = items[i].signature;
        row.appendChild(sg);
      }
      (function(idx) {
        row.addEventListener("mousedown", function(e) { e.preventDefault(); selIdx = idx; acceptCompletion(); });
      })(i);
      popup.appendChild(row);
    }
    document.body.appendChild(popup);
    var rect = null;
    var s = window.getSelection();
    if (s.rangeCount) rect = s.getRangeAt(0).getBoundingClientRect();
    if (!rect || (rect.left === 0 && rect.top === 0)) rect = editor.getBoundingClientRect();
    popup.style.left = rect.left + "px";
    popup.style.top = (rect.bottom + 2) + "px";
    paintSelection();
  }

  editor.addEventListener("input", function() {
    hideCompletions();
    var text = editor.textContent;
    // Keep the ghost only if the user typed exactly its next character at the end of the line;
    // any other edit drops it (the debounced request below recomputes a fresh one).
    if (ghost && text.length === prevText.length + 1 && text.slice(0, prevText.length) === prevText
        && text.charAt(text.length - 1) === ghost.charAt(0)) {
      var rest = ghost.slice(1);
      setGhost((rest === "" || rest === "...") ? "" : rest);
    } else {
      setGhost("");
    }
    prevText = text;
    send("tokenize", text);
    scheduleGhost();
  });
  editor.addEventListener("blur", function() { setTimeout(hideCompletions, 150); });

  editor.addEventListener("keydown", function(e) {
    if (popup) {
      if (e.key === "ArrowDown") { e.preventDefault(); moveCompletion(1); return; }
      if (e.key === "ArrowUp") { e.preventDefault(); moveCompletion(-1); return; }
      if (e.key === "Enter" || e.key === "Tab") { e.preventDefault(); acceptCompletion(); return; }
      if (e.key === "Escape") { e.preventDefault(); hideCompletions(); return; }
      hideCompletions();
    }
    // Right at the end of the line accepts the ghost suggestion (its common stem, dropping a
    // trailing "..."), like fish; elsewhere it just moves the cursor.
    if (e.key === "ArrowRight" && ghost && atEnd()) {
      e.preventDefault();
      var accept = (ghost.slice(-3) === "...") ? ghost.slice(0, -3) : ghost;
      var text = editor.textContent + accept;
      editor.textContent = text;
      setCaret(text.length);
      setGhost("");
      prevText = text;
      send("tokenize", text);
      scheduleGhost();
      return;
    }
    if (e.key === "Tab") {
      e.preventDefault();
      // With a ghost showing, Tab accepts it: insert its text (the common stem, dropping a
      // trailing "...") and remove it. If several candidates shared that stem, then open the
      // popup to choose among them; a unique completion just lands. With no ghost, ask the
      // server and show the popup as before.
      if (ghost && atEnd()) {
        var multiple = ghost.slice(-3) === "...";
        var accept = multiple ? ghost.slice(0, -3) : ghost;
        var text = editor.textContent + accept;
        editor.textContent = text;
        setCaret(text.length);
        setGhost("");
        prevText = text;
        send("tokenize", text);
        if (multiple) popupSeq = send("complete", text, getCaret());
        else scheduleGhost();
      } else {
        popupSeq = send("complete", editor.textContent, getCaret());
      }
      return;
    }
    // Enter submits a complete (or malformed) line and inserts a continuation newline for an
    // incomplete one (per the engine's parser, cached as `incomplete`/`incompleteText`; a
    // bracket-balance fallback covers the gap before the verdict for the current text lands).
    // Shift+Enter always submits, matching the CLI front-end.
    if (e.key === "Enter" && e.shiftKey) { e.preventDefault(); submit(); return; }
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      var text = editor.textContent;
      var inc = (incompleteText === text) ? incomplete : !looksComplete(text);
      if (inc) { insertNewline(); send("tokenize", editor.textContent); }
      else submit();
      return;
    }
    // Up/Down cycle through history, but only from the first/last line so multi-line entries
    // still move the cursor between their lines.
    if (e.key === "ArrowUp") {
      var before = editor.textContent.slice(0, getCaret());
      if (before.indexOf("\\n") === -1) { e.preventDefault(); setGhost(""); historyUp(); return; }
    }
    if (e.key === "ArrowDown") {
      var after = editor.textContent.slice(getCaret());
      if (after.indexOf("\\n") === -1) { e.preventDefault(); setGhost(""); historyDown(); return; }
    }
    // Any other caret movement drops the ghost (it is only valid at the end of the line).
    if (e.key === "ArrowLeft" || e.key === "Home" || e.key === "End" || e.key === "Escape") setGhost("");
  });

  editor.addEventListener("mousedown", function() { setGhost(""); });

  function onMessage(ev) {
    var msg = JSON.parse(ev.data);
    if (msg.kind === "pong") return;
    if (msg.kind === "hello") {
      if (serverInstance === null) serverInstance = msg.value;
      else if (msg.value !== serverInstance) {
        serverInstance = msg.value;
        logBlock("notice", "${Repl.messages.serverRestarted}");
        log.scrollIntoView(false);
      }
      // Show this connection's auto-started session name (and any later switch). The banner text
      // (`msg.output`) is built in core, so it reads identically to the CLI's startup line.
      if (msg.name && msg.name !== sessionName) {
        sessionName = msg.name;
        logBlock("notice", msg.output);
        log.scrollIntoView(false);
      }
      return;
    }
    if (msg.kind === "completions") {
      if (msg.seq === popupSeq) showCompletions(msg.completions);
      else if (msg.seq === ghostSeq) applyGhost(msg.completions);
      return;
    }
    if (msg.kind === "tokens") {
      highlight(msg.tokens);
      if (msg.seq in tokenizeCode) { incompleteText = tokenizeCode[msg.seq]; incomplete = msg.incomplete; delete tokenizeCode[msg.seq]; }
      return;
    }
    // Async mode: a "pending" ack appends an empty placeholder block tagged with the submission's seq;
    // the later "async" message (same seq) fills that block in place, so the result lands where the
    // submission was, even though the editor has moved on.
    if (msg.kind === "pending") {
      var ph = document.createElement("div");
      ph.className = "pending";
      ph.setAttribute("data-id", msg.seq);
      ph.textContent = "⋯ evaluating…";
      log.appendChild(ph);
      log.scrollIntoView(false);
      return;
    }
    if (msg.kind === "async") {
      var target = log.querySelector('[data-id="' + msg.seq + '"]');
      if (!target) { target = document.createElement("div"); log.appendChild(target); }
      target.removeAttribute("data-id");
      target.className = "";
      fillResult(target, msg);
      log.scrollIntoView(false);
      return;
    }
    // A normal (synchronous) result or error: one block rendered by the shared helper.
    var block = document.createElement("div");
    fillResult(block, msg);
    log.appendChild(block);
    log.scrollIntoView(false);
  }

  function scheduleReconnect() {
    if (reconnectTimer) return;
    reconnectTimer = setTimeout(function() { reconnectTimer = null; connect(); }, 1000);
  }

  // The server closes an idle WebSocket after ~30s; a foreground tab is kept warm by the
  // ping below, but a backgrounded tab has its timers throttled, so the socket may still
  // drop. We reconnect automatically (the REPL session lives on the server, shared across
  // connections, so a fresh socket resumes the same state) and surface the state through
  // the indicator. Guard against opening a second socket while one is already live.
  function connect() {
    if (socket && (socket.readyState === WebSocket.CONNECTING || socket.readyState === WebSocket.OPEN)) return;
    setStatus("connecting");
    socket = new WebSocket("ws://" + location.host + "/socket");
    socket.onmessage = onMessage;
    socket.onopen = function() {
      connected = true;
      everConnected = true;
      setStatus("connected");
      send("hello", "");
      editor.focus();
      if (editor.textContent) send("tokenize", editor.textContent);
    };
    socket.onclose = function() {
      connected = false;
      hideCompletions();
      setGhost("");
      setStatus("connecting");
      scheduleReconnect();
    };
    socket.onerror = function() { try { socket.close(); } catch (e) {} };
  }

  // Timers are throttled in a hidden tab, so reconnect promptly when the tab is shown or
  // refocused rather than waiting for the (possibly throttled) retry timer.
  function reconnectNow() {
    if (socket && (socket.readyState === WebSocket.CONNECTING || socket.readyState === WebSocket.OPEN)) return;
    if (reconnectTimer) { clearTimeout(reconnectTimer); reconnectTimer = null; }
    connect();
  }
  document.addEventListener("visibilitychange", function() { if (!document.hidden) reconnectNow(); });
  window.addEventListener("focus", reconnectNow);

  connect();
  setInterval(function() { send("ping", ""); }, 20000);
})();
"""

// The REPL page: a heading, a scrollback log (`<pre>`), and the live editor (`<code>`,
// made contenteditable by the script). Styling and behaviour come entirely from the
// script, so the page itself stays minimal.
class ReplPage() extends Archetype:
  def content: Html of (? <: Flow) =
    Fragment[Flow](H1(t"Flame REPL"), Pre(), Code(), Script(replScript))

private def webTokens(tokens: List[Repl.Token]): List[WebToken] =
  tokens.map { token => WebToken(token.text, token.accent, token.role.or(t"")) }

private def webCompletions(items: List[Repl.CompletionItem]): List[WebCompletion] =
  items.map { item => WebCompletion(item.name, item.kind, item.signature) }

// Maps a typed `Repl.Reply` (from `react`) to the flat reply the browser renders.
private def resultReply(seq: Int, reply: Repl.Reply): WebReply = reply match
  case Repl.Reply.Ran(_, value, output, tpe, name, diagnostics, highlight) =>
    WebReply
     ( t"result", seq, value.or(t""), tpe.or(t""), output, diagnostics, webTokens(highlight),
       name = name.or(t"") )

  case Repl.Reply.Rejected(_, diagnostics, highlight) =>
    WebReply(t"error", seq, t"", t"", t"", diagnostics, webTokens(highlight))

  case Repl.Reply.Threw(_, output, diagnostics, highlight) =>
    WebReply(t"error", seq, t"", t"", output, diagnostics, webTokens(highlight))

  case Repl.Reply.Crashed(_, diagnostics, highlight) =>
    WebReply(t"error", seq, t"", t"", t"", diagnostics, webTokens(highlight))

  case Repl.Reply.Failed(_, message) =>
    WebReply(t"error", seq, t"", t"", t"", message, Nil)

  case _ =>
    WebReply(t"error", seq, t"", t"", t"", t"", Nil)

// The out-of-band fill for an async submission: the same content `resultReply` produces, but tagged
// `async` so the browser fills the matching placeholder div (located by `seq`) rather than appending a
// new block. The JS `async` handler renders value/output/diagnostics exactly like a normal result.
private def asyncReply(seq: Int, reply: Repl.Reply): WebReply =
  resultReply(seq, reply).copy(kind = t"async")

// Serves the web REPL on `port`, blocking until the process is interrupted. The embedded
// engine's compile classpath comes from the supplied `Classloader`, so each caller passes
// the loader matching how it was launched: the standalone `web` main uses the
// thread-context loader (correct when run from the fat assembly), while the CLI client
// passes its `serverClassloader` (correct under the Burdock/Ethereal launcher).
// Blocks until `quit` is fulfilled (the CLI completes it on Ctrl+C; the standalone `web`
// main passes one that is never fulfilled and relies on the JVM's own signal handling).
def serveHttp(port: Int, quit: Promise[Unit])(using Monitor, System, Probate, Classloader): Unit =
  given Scalac[3.8] = Scalac(Nil)

  // Multiple named sessions — each browser connection auto-starts a fresh randomly-named session and
  // may `/session`-switch to any other. Result values render as HTML (via `flame.HtmlRender`'s
  // Renderable→Showable→toString cascade), unlike the CLI's teletype `Inspect` rendering.
  val sessions = Sessions(Repl.Rendering.Html)

  // A token unique to this server process. The client remembers it and, on reconnecting,
  // compares: a different token means it reached a fresh process (e.g. after a restart),
  // whose `repl` has none of the previous session's definitions, imports, or settings —
  // so the client can warn that the session was lost.
  val instance: Text = java.util.UUID.randomUUID.nn.toString.nn.tt

  // Translate one JSON request from the browser into a JSON reply, dispatched on this connection's
  // CURRENT session (`current`): `tokenize` (the fast, compiler-free lexer) is stateless; `submit`/
  // `complete` run on the current session (with `/session` intercepted to switch it); `hello` reports
  // the session name for the startup banner.
  // `push` sends an unsolicited frame to THIS browser (via the connection's WebSocket channel), used
  // to deliver an async submission's result out-of-band once it is ready.
  def respondJson(current: juca.AtomicReference[Text], push: Text => Unit, payload: Text): Text =
    def session: Optional[Repl[3.8]] = sessions.session(current.get.nn)

    safely(payload.read[Json].as[WebRequest]).let: request =>
      request.kind match
        case t"tokenize" =>
          WebReply
           ( t"tokens", request.seq, t"", t"", t"", t"", webTokens(Repl.tokenize(request.code)), Nil,
             Repl.incomplete(request.code) )

        case t"submit" =>
          val code = request.code
          if code == t"/session" || code.starts(t"/session ") then
            val name = code.skip(t"/session".length).trim
            val message: Text =
              if name == t"" then Repl.messages.sessionList(current.get.nn, sessions.names)
              else if sessions.session(name).let(_ => true).or(false) then
                current.set(name)
                Repl.messages.switched(name)
              else Repl.messages.noSession(name)

            WebReply(t"result", request.seq, t"", t"", message, t"", Nil)

          // An unrecognised `/`-command gets the same message as the CLI, rather than being compiled
          // as Scala (`Repl.isCommand` and the message text are shared with the CLI, via core).
          else if code.starts(t"/") && !Repl.isCommand(code) then
            WebReply(t"result", request.seq, t"", t"", Repl.messages.unknownCommand(code), t"", Nil)
          else
            session.lay(WebReply(t"error", request.seq, t"", t"", t"", t"flame: no active session", Nil)): repl =>
              if !repl.asyncEnabled then resultReply(request.seq, repl.react(request.seq, code))
              else
                // Async mode: run the submission on a worker. If it finishes within the grace window,
                // reply with the full result inline; otherwise acknowledge with a `pending` placeholder
                // now and `push` the real reply (tagged `async`, correlated by `seq`) when it completes.
                val promise: Promise[Repl.Reply] = Promise()

                async:
                  promise.offer:
                    safely(repl.react(request.seq, code)).or
                     (Repl.Reply.Failed(request.seq, t"the submission could not be processed"))

                promise.attend(Sessions.graceMillis)

                promise().let(resultReply(request.seq, _)).or:
                  async:
                    promise.attend()
                    promise().let { reply => push(asyncReply(request.seq, reply).json.show) }

                  WebReply(t"pending", request.seq, t"", t"", t"", t"", Nil)

        case t"complete" =>
          if request.code.starts(t"/session ") then
            val partial = request.code.skip(t"/session ".length)
            val items = sessions.names.filter(_.starts(partial)).map: name =>
              WebCompletion(t"/session $name", t"command", t"")

            WebReply(t"completions", request.seq, t"", t"", t"", t"", Nil, items)
          else
            val completions = session.lay(Nil)(_.completionsAt(request.code, request.offset))
            WebReply(t"completions", request.seq, t"", t"", t"", t"", Nil, webCompletions(completions))

        case t"ping" =>
          // The client's keep-alive heartbeat; the reply is ignored, but answering keeps
          // both directions active so the connection never crosses the idle timeout.
          WebReply(t"pong", request.seq, t"", t"", t"", t"", Nil)

        case t"hello" =>
          // Sent on every (re)connect; the instance token lets the client tell a resumed connection
          // from a new one, `name` carries this connection's session (for the switch-detection), and
          // `output` is the ready-to-show startup banner (built in core, identical to the CLI's).
          WebReply
           ( t"hello", request.seq, instance, t"", Repl.messages.session(current.get.nn), t"", Nil,
             name = current.get.nn )

        case _ =>
          WebReply(t"error", request.seq, t"", t"", t"", t"flame: unknown request", Nil)

    . lay(t"")(_.json.show)

  val service = SocketServer(port).handle:
    request.target match
      case t"/" | t"/index.html" =>
        Http.Response(ReplPage())

      case t"/socket" =>
        // Per-connection: a fresh session, switchable with `/session`.
        val current: juca.AtomicReference[Text] = juca.AtomicReference(sessions.create())

        Http.Response:
          // Bind the socket to a `lazy val` so the handler can reach `ws.channel` (only dereferenced
          // when a message arrives, by which point `ws` is initialised) to push async results out-of-
          // band. `push` sends one Text frame — wire-identical to a normal reply.
          lazy val ws: perihelion.Websocket[perihelion.Message, Unit] =
            webSocket(): (message: perihelion.Message) =>
              def push(text: Text): Unit = ws.channel.send(perihelion.Message.Text(text))

              message match
                case perihelion.Message.Text(payload) =>
                  Reply(perihelion.Message.Text(respondJson(current, push, payload)), ())

                case perihelion.Message.Binary(_) => Continue(())

          ws

      case _ =>
        Http.Response(Http.NotFound)(t"not found")

  java.lang.System.out.nn.println("flame: serving the web REPL (press Ctrl+C to stop):")
  java.lang.System.out.nn.println(s"  http://localhost:$port/")
  quit.attend()
  safely(service.cancel())

@main
def web(): Unit =
  given Classloader = threadContextClassloader
  supervise(serveHttp(8080, Promise[Unit]()))
