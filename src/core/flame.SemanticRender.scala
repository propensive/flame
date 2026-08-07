                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                       ╭────────╮╌──╮                                                             ┃
┃                       │   ╭────╯   │                                                             ┃
┃                       │   │    │   │                                                             ┃
┃                       │   ╰──╮ │   │╭─────────╮╭───╮╌────╮╌────╮╭────────╮                       ┃
┃                       │   ╭──╯ │   ││   ╭─╮   ││   ╭─╮   ╭─╮   ││   ╭─╮  │                       ┃
┃                       │   │    │   ││   │ │   ││   │ │   │ │   ││   ╰─╯  │                       ┃
┃                       │   │    │   ││   │ │   ││   │ │   │ │   ││   ╭────╯                       ┃
┃                       │   │    │   ││   ╰─╯   ││   │ │   │ │   ││   ╰────╮                       ┃
┃                       ╰───╯    ╰───╯╰────╌╰───╯╰───╯ ╰───╯ ╰───╯╰────────╯                       ┃
┃                                                                                                  ┃
┃    Flame, version 0.1.0.                                                                         ┃
┃    © Copyright 2026 Jon Pretty, Propensive OÜ.                                                   ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://propensive.dev/flame/                                                             ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package flame

import scala.collection.immutable as sci

import anticipation.*
import anthology.*
import escapade.*, termcapDefinitions.xtermTrueColorTermcap
import gossamer.*
import harlequin.*
import hieroglyph.*, textMetrics.uniformMetric
import iridescence.*
import prepositional.*
import rudiments.*
import stenography.*
import vacuous.*

import delicious.*  // Markup, SemanticMessage, Reifier, `semantic` on Notice, `teletype` on SemanticMessage

// Renders compiler notices for a REPL reply, re-rendering the TYPES embedded in each error message
// through stenography and syntax-highlighting both them and any embedded CODE SAMPLES with harlequin.
// Under `-Xsemantic-diagnostics` the compiler wraps every interpolated type of a message in in-band
// markers carrying its pickled TASTy, and interpolated trees as `code` nodes; `delicious` parses that
// into a `Markup` tree and its `Reifier` turns each type marker's TASTy back into a
// `stenography.Syntax`. For the CLI (`Inspect`) the rendering is `delicious.ansi`'s `teletype` —
// harlequin-highlighted code and types as coloured ANSI; for the web (`Html`) the same tree is walked
// here, emitting harlequin tokens as `tok-*` spans (the classes the web page already styles). Both
// fall back to the compiler's own printed text for anything that cannot be reified, and to the plain
// message when there is no semantic markup at all.
object SemanticRender:
  private def hex(rgb: Int): Color in Srgb =
    Srgb(((rgb >> 16) & 0xff)/255.0, ((rgb >> 8) & 0xff)/255.0, (rgb & 0xff)/255.0)

  // The syntax palette, matched to the user's Zed theme "Propensive" — the single source of truth,
  // shared with the CLI front-end (whose editor `given` delegates here), so a type or code sample in
  // an error message is coloured exactly as it would be in the editor. (See the palette notes in
  // `flame_client.scala`: an accent is a colour category; `scalaSymbol`/`scalaParenthesis` map to
  // Zed's punctuation.bracket/operator respectively.)
  val palette: ScalaSyntaxPalette = new Palette:
    type Form = Srgb
    def background:       Color in Srgb = hex(0x000000)  // editor.background
    def foreground:       Color in Srgb = hex(0xd4be98)  // editor.foreground
    def scalaError:       Color in Srgb = hex(0xea6962)  // deleted (a clear red)
    def scalaNumber:      Color in Srgb = hex(0xcc3366)  // number
    def scalaString:      Color in Srgb = hex(0x99ffff)  // string
    def scalaTerm:        Color in Srgb = hex(0xffcc99)  // variable — every term (binding or usage)
    def scalaType:        Color in Srgb = hex(0x00cc99)  // type
    def scalaKeyword:     Color in Srgb = hex(0xff6633)  // keyword
    def scalaSymbol:      Color in Srgb = hex(0xcc6699)  // punctuation.bracket — `(` `)` `[` `]` `:`
    def scalaParenthesis: Color in Srgb = hex(0xf28534)  // operator — `=` `.`
    def scalaModifier:    Color in Srgb = hex(0xff6633)  // keyword (no distinct modifier scope)
    def scalaComment:     Color in Srgb = hex(0x928374)  // comment
    def subdued:          Color in Srgb = hex(0x5a524c)  // editor.line_number
    def accented:         Color in Srgb = hex(0xd4be98)  // editor.active_line_number
    def margin:           Color in Srgb = hex(0x111111)  // editor.gutter.background

  // Renders `notices` to the diagnostics string. `reifier` reifies the TASTy of each type marker
  // (`Unset` — no marker present, or the reifier could not be built — falls back to compiler text).
  // `html` picks the target: HTML spans for the web, coloured ANSI for the terminal.
  def render(notices: List[Notice], reifier: Optional[Reifier], html: Boolean)(using Imports): Text =
    notices.map { (notice: Notice) => renderNotice(notice, reifier, html) }.join(t"; ")

  // The column at which a diagnostic's PROSE word-wraps in the terminal rendering. Neither the daemon
  // nor basic mode knows the client terminal's true width, so this follows the compiler's own
  // page-width convention. Embedded code samples are never wrapped; a single word (or type) longer
  // than the width is emitted whole on its own line.
  private val wrapWidth: Int = 80

  private def renderNotice(notice: Notice, reifier: Optional[Reifier], html: Boolean)(using Imports)
  :   Text =
    notice.semantic.lay(if html then htmlPlain(notice.message) else wrapPlain(notice.message)): message =>
      if html then htmlNodes(message.markup, reifier)
      else ansiMessage(message, reifier).render(xtermTrueColorTermcap)

  // ── ANSI (the CLI's `Inspect` mode) ──────────────────────────────────────────────────────────
  // Mirrors `delicious.ansi`'s Teletype rendering (harlequin-highlighted code samples,
  // stenography-rendered and type-position-highlighted types), but WORD-WRAPS the prose at
  // `wrapWidth`: the flattened tree becomes a stream of `Piece`s — explicit line breaks, runs of
  // spaces, atomic words (a prose word, or a whole highlighted type), and unbreakable code samples —
  // and the flow inserts a newline (swallowing the intervening spaces) wherever the next word would
  // overrun the width. Existing newlines and space runs are preserved verbatim, so the compiler's own
  // alignment (`Found:    …`) survives; only overlong lines gain breaks, and never inside code.
  private enum Piece:
    case Break                                  // an explicit newline in the message
    case Space(run: Text)                       // a run of spaces/tabs (dropped at a soft break)
    case Word(styled: Teletype, width: Int)     // an atomic word: prose, or a highlighted type
    case Atom(styled: Teletype, plain: Text)    // a code sample: emitted verbatim, never wrapped

  private def ansiMessage(message: SemanticMessage, reifier: Optional[Reifier])(using Imports)
  :   Teletype =
    given ScalaSyntaxPalette = palette

    // Harlequin-highlights `text` in `context` as a Teletype. Called through the typeclass instance
    // (`soundness.*` shadows escapade's generic `.teletype` extension with delicious's — see the
    // note in `flame_client.colourful`).
    def code(text: Text, context: Scala.Context): Teletype =
      harlequin.syntaxHighlighting.unnumberedTeletypeable
       . teletype(Scala.highlight(stripAnsi(text), context))

    def pieces(nodes: List[Markup]): List[Piece] = nodes.flatMap(piece)

    def piece(markup: Markup): List[Piece] = markup match
      case Markup.Textual(text) => prose(text)
      case Markup.Code(_, _)    => List(Piece.Atom(code(markup.plain, Scala.Context.Term), markup.plain))

      case typed: Markup.Typed =>
        val text: Text = typeText(typed, reifier)
        List(Piece.Word(code(text, Scala.Context.Type), text.length))

      case Markup.Symbolic(_, _, _, children) => pieces(children)
      case Markup.Named(_, _, children)       => pieces(children)
      case Markup.Spanned(_, _, children)     => pieces(children)

    flow(pieces(message.markup))

  // Flows `pieces` into a Teletype, wrapping at `wrapWidth` (see `ansiMessage`).
  private def flow(pieces: List[Piece]): Teletype =
    var out:     Teletype = e""
    var column:  Int      = 0
    var pending: Text     = t""

    pieces.each:
      case Piece.Break =>
        out = out.append(e"\n")
        column = 0
        pending = t""

      case Piece.Space(run) =>
        pending = t"$pending$run"

      case Piece.Word(styled, width) =>
        if column > 0 && column + pending.length + width > wrapWidth then
          out = out.append(e"\n").append(styled)
          column = width
        else
          out = out.append(e"$pending").append(styled)
          column += pending.length + width
        pending = t""

      case Piece.Atom(styled, plain) =>
        out = out.append(e"$pending").append(styled)
        val newline: Int = plain.s.lastIndexOf('\n')
        column =
          if newline < 0 then column + pending.length + plain.length
          else plain.length - newline - 1
        pending = t""

    out

  // Splits plain prose into `Piece`s: explicit breaks, runs of spaces/tabs, and words — preserving
  // every character, so unwrapped text round-trips exactly.
  private def prose(text: Text): List[Piece] =
    val s: String = stripAnsi(text).s
    val pieces = sci.List.newBuilder[Piece]
    var i = 0
    while i < s.length do
      val char = s.charAt(i)
      if char == '\n' then
        pieces += Piece.Break
        i += 1
      else if char == ' ' || char == '\t' then
        val start = i
        while i < s.length && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
        pieces += Piece.Space(s.substring(start, i).nn.tt)
      else
        val start = i
        while i < s.length && s.charAt(i) != '\n' && s.charAt(i) != ' ' && s.charAt(i) != '\t'
        do i += 1
        val word: Text = s.substring(start, i).nn.tt
        pieces += Piece.Word(e"$word", word.length)

    List.of(pieces.result())

  // Word-wraps a PLAIN (no semantic markup) message for the terminal, through the same flow — no
  // styling, so the wrapped Teletype's plain text is returned directly.
  private def wrapPlain(text: Text): Text = flow(prose(text)).plain

  // The stenography rendering of a type marker (Unset reifier, or a failed reification, falls back to
  // the compiler-printed text the marker also carries in `plain`).
  private def typeText(typed: Markup.Typed, reifier: Optional[Reifier])(using Imports): Text =
    reifier.lay(Unset: Optional[Syntax])(_.syntax(typed)).let(_.text).or(typed.plain)

  // ── HTML (the web's `Html` mode) ─────────────────────────────────────────────────────────────
  // Mirrors `delicious.ansi`'s Teletype rendering for the web: code samples and types are tokenized
  // by harlequin (`Tokenized` depth — no compiler, no classpath) and emitted as `tok-<accent>` spans,
  // the same classes the web editor already colours.
  private def htmlNodes(nodes: List[Markup], reifier: Optional[Reifier])(using Imports): Text =
    nodes.map { (node: Markup) => htmlNode(node, reifier) }.join

  private def htmlNode(markup: Markup, reifier: Optional[Reifier])(using Imports): Text = markup match
    case Markup.Textual(text)               => htmlPlain(text)

    // A code sample must not word-wrap mid-token: the `.code-sample` class (styled `white-space: pre`
    // in the web page) exempts it from the surrounding `pre-wrap` prose flow.
    case Markup.Code(_, _) =>
      t"""<span class="code-sample">${htmlHighlight(markup.plain, Scala.Context.Term)}</span>"""
    case typed: Markup.Typed                => htmlHighlight(typeText(typed, reifier), Scala.Context.Type)
    case Markup.Symbolic(_, _, _, children) => htmlNodes(children, reifier)
    case Markup.Named(_, _, children)       => htmlNodes(children, reifier)
    case Markup.Spanned(_, _, children)     => htmlNodes(children, reifier)

  // Harlequin-highlights `text` (ANSI-stripped first — the compiler may have styled it) in `context`,
  // as `tok-*` spans. Newlines are re-inserted between the token lines, exactly as `Repl.project` does.
  private def htmlHighlight(text: Text, context: Scala.Context): Text =
    val lines: List[List[harlequin.Token]] =
      List.from(Scala.highlight(stripAnsi(text), context).lines.readable)

    lines
     . map: (line: List[harlequin.Token]) =>
         line.map: (token: harlequin.Token) =>
           t"""<span class="tok-${token.accent.toString.tt.lower}">${escapeHtml(token.text)}</span>"""
         . join
     . join(t"\n")

  private def escapeHtml(text: Text): Text =
    text.s.replace("&", "&amp;").nn.replace("<", "&lt;").nn.replace(">", "&gt;").nn.tt

  // Strips ANSI SGR escapes, then HTML-escapes — used to make an already-ANSI diagnostic (a thrown
  // exception's stack trace) safe for the web's trusted-HTML diagnostics slot without colour.
  def stripAnsi(text: Text): Text =
    text.s.replaceAll("\\e?\\[[0-9;]*m", "").nn.tt

  def htmlPlain(text: Text): Text = escapeHtml(stripAnsi(text))
