                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
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

import java.io as ji
import java.lang as jl

import scala.quoted.*

// Dotty parser internals, used (aliased, to avoid clashing with Soundness names) only to
// decide whether a line is a syntactically-incomplete prefix — see `Repl.incomplete`.
import dotty.tools.dotc.core.Contexts as DottyContexts
import dotty.tools.dotc.parsing.Parsers as DottyParsers
import dotty.tools.dotc.reporting.StoreReporter as DottyStoreReporter
import dotty.tools.dotc.util.SourceFile as DottySourceFile

import ambience.*
import anthology.*
import anticipation.*
import coaxial.*
import contingency.*
import denominative.*
import digression.*
import distillate.*
import galilei.*
import gossamer.*
import harlequin.*
import hellenism.*
import inimitable.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import stratiform.*
import vacuous.*

import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.overwritePreexisting.disabled
import interfaces.paths.pathOnLinux
import stenography.Syntax

object Repl:
  object Layout:
    class Standard() extends Layout:
      def objectName(index: Int): Text = t"rs$$line$$$index"

      def wrap(index: Int, history: List[Text], imports: List[Text], code: Text): Text =
        // `import p.*` brings regular members into scope but NOT `given` instances (Scala 3),
        // so `given` selector too — otherwise a `given` declared on an earlier line would be
        // invisible to later ones, unlike a `val`/`def`.
        val historyImports = history.map: previous =>
          t"  import $previous.{given, *}"

        val body = code.cut(t"\n").map: line =>
          t"  $line"

        // The re-injected imports go at file scope — outside the wrapper object — so the
        // object's history imports, which carry the user's prior definitions, sit in an
        // inner scope and take precedence, just as a definition outranks a wildcard import.
        // (Otherwise a prior `val z` and a wildcard `import …*` that also binds `z` would
        // be two same-scope wildcard imports, and any use of `z` would be ambiguous.)
        val wrapper = t"object ${objectName(index)}:" :: historyImports ::: body
        (imports ::: wrapper).join(t"\n")

  trait Layout:
    def objectName(index: Int): Text
    def wrap(index: Int, history: List[Text], imports: List[Text], code: Text): Text

  enum Outcome:
    // `name`/`tpe` are set only for a value-producing expression: the binding name chosen for
    // its result (shown, and usable on later lines) and its rendered type.
    case Ran(notices: List[Notice], value: Optional[Text], output: Text,
             name: Optional[Text] = Unset, tpe: Optional[Text] = Unset)
    case Threw(notices: List[Notice], error: Throwable, output: Text)
    case Rejected(notices: List[Notice])
    case Crashed(notices: List[Notice], error: StackTrace)

  // One syntax-highlighting token of the submitted line. `accent` is the lowercased Harlequin
  // accent (a COLOUR category — `keyword`, `term`, `typal`, …); `role` is `binding`/`usage` for
  // a term or type (a styling policy may e.g. italicise bindings); `tpe` is the token's
  // fully-qualified Scala type, where the typechecker resolved one. ANSI/CSS rendering is the
  // front-end's concern.
  case class Token(text: Text, accent: Text, tpe: Optional[Text], role: Optional[Text] = Unset)

  // One tab-completion candidate. The Harlequin `Completion`'s `Syntax` signature is rendered
  // to text here so the reply serializes simply.
  case class CompletionItem(name: Text, kind: Text, signature: Text)

  // A `Reply` variant carries a `List` of these leaf products, so its Decodable
  // derivation graph (sum → variant → `List` → product) overflows inline derivation
  // under the REPL's minimal predef. Anchoring the leaves with explicit derived
  // instances keeps the graph shallow enough to resolve.
  given tokenDecodable: Tactic[TelError] => Token is Tel.Decodable =
    Tel.DecodableDerivation.derived

  given completionItemDecodable: Tactic[TelError] => CompletionItem is Tel.Decodable =
    Tel.DecodableDerivation.derived

  // A request from a connected client. `id` is echoed in the reply so the client
  // can re-associate replies that arrive out of order (a fast `tokenize` may
  // overtake a slow `submit`). Serialized as JSON with a `kind` discriminator.
  enum Request:
    case Submit(id: Int, code: Text)
    case Tokenize(id: Int, code: Text)
    case Complete(id: Int, code: Text, offset: Int)
    case Quit(id: Int)
    // Switch this connection to the session named `name`, or — when `name` is empty — just report the
    // current session and the list of all sessions (used on connect and to refresh tab-completion).
    case Session(id: Int, name: Text)

  // A reply to a connected client, echoing the request's `id`. `highlight` is the
  // Harlequin tokenization of the submitted line. Serialized as JSON with a `kind`
  // discriminator.
  enum Reply:
    case Tokenized(id: Int, highlight: List[Token], incomplete: Boolean)
    case Completed(id: Int, completions: List[CompletionItem])

    case Ran(id: Int, value: Optional[Text], output: Text, tpe: Optional[Text],
             name: Optional[Text], diagnostics: Text, highlight: List[Token])

    case Rejected(id: Int, diagnostics: Text, highlight: List[Token])
    case Threw(id: Int, output: Text, diagnostics: Text, highlight: List[Token])
    case Crashed(id: Int, diagnostics: Text, highlight: List[Token])
    case Failed(id: Int, message: Text)
    // Sent in async mode (`/set async`) immediately on submit: a placeholder acknowledgement. The real
    // result follows out-of-band later as an ordinary `Ran`/`Threw`/`Rejected`/`Crashed`/`Failed` reply
    // carrying this same `id`.
    case Pending(id: Int)
    // A chunk of the submission's stdout, streamed out-of-band as the run produces it (async mode), so a
    // front-end shows output as it appears rather than only in the final reply. Carries the same `id`.
    case Output(id: Int, chunk: Text)
    // The connection's current session `name`, plus `names` — every session on the server (for the
    // startup display and `/session` tab-completion).
    case Session(id: Int, name: Text, names: List[Text])

  // Highlights `code` with Harlequin's typechecked pipeline (the compiler
  // resolves symbols, so accents are accurate and each token carries its type).
  // Needs the session's `Scalac` and compile classpath; used for `submit`.
  def highlight(code: Text)(using Scalac[?], LocalClasspath): List[Token] =
    import highlighting.typecheckedScala
    project(Scala.highlight(code))

  // Highlights `code` with Harlequin's standalone lexer — no compiler, so it is
  // fast enough to run on every keystroke for live editing (no type information).
  def tokenize(code: Text): List[Token] =
    project(Scala.highlight(code))

  // True when `code` is a syntactically-*incomplete* prefix of a Scala statement or
  // expression — an unclosed bracket, a trailing infix operator, `val x =` with no
  // right-hand side, `if (c)` with no body — as opposed to either complete or outright
  // malformed. A front-end uses this to decide what Enter does: insert a continuation
  // newline when incomplete, otherwise submit (a complete line runs; a malformed line also
  // runs, surfacing its error, rather than trapping the user mid-edit).
  //
  // It runs only Dotty's parser (no typer, no classpath, so it is cheap enough per
  // keystroke), reusing the parser's own notion of incompleteness: reaching EOF where more
  // input is required is reported through the reporter's *incomplete handler*, which is
  // distinct from an ordinary syntax error. So `1 +` is incomplete, but `1 ++ 2` (a real
  // error) is not — it submits and the compiler reports the problem.
  def incomplete(code: Text): Boolean =
    code.s.trim.nn.length > 0 && {
      val source    = DottySourceFile.virtual("<incomplete>", code.s)
      val reporter  = DottyStoreReporter()
      val context   = DottyContexts.ContextBase().initialCtx.fresh.setReporter(reporter).withSource(source)
      var needsMore = false

      reporter.withIncompleteHandler((_, _) => needsMore = true) {
        DottyParsers.Parser(source)(using context).blockStatSeq()
        ()
      }

      needsMore && !reporter.hasErrors
    }

  private def project(source: SourceCode): List[Token] =
    val lines: List[List[Token]] = source.lines.to(List).map: line =>
      line.map: token =>
        Token
         ( token.text,
           token.accent.toString.tt.lower,
           token.meta.let(_.tpe.qualified),
           token.role.let(_.toString.tt.lower) )

    // `SourceCode.lines` was split on (and dropped) the newlines, so re-insert a
    // newline token between consecutive lines — otherwise multi-line code collapses
    // to a single line on the client and its cursor maths drift apart.
    lines match
      case Nil          => Nil
      case head :: rest => head ::: rest.flatMap(Token(t"\n", t"unparsed", Unset) :: _)

  // Tab completions at character `offset` in `code`, from Harlequin's typechecked
  // pipeline (so it needs the session's `Scalac` and compile classpath). `context` is the
  // session scope — the persistent imports and the prior wrapper objects — emitted as
  // top-level lines before the code so completion sees the same names a real line would:
  // names from an earlier `import soundness.*` (so `Ou` completes to `Out`), prior
  // definitions, and bare first-token identifiers as well as member selections. An import
  // line is completed in import position (a package/member path); anything else is wrapped
  // as `val __completion = <code>` so a bare expression is valid to complete against. The
  // caret is shifted past the whole prefix. Each candidate's `Syntax` signature is rendered
  // to text for the wire.
  def complete(context: List[Text], code: Text, offset: Int)(using Scalac[?], LocalClasspath)
  :   List[CompletionItem] =
    import highlighting.typecheckedScala

    val contextLines: Text = context.map { line => t"$line\n" }.join

    // The completion point can sit inside a DEFINITION, not just a bare expression — e.g.
    // `def foo() = System.o`, `val x = System.o`, or a nested `class C { def m = System.o }`.
    // `val __completion = <code>` only parses when `<code>` is an expression (`val __ = def f = …`
    // is a syntax error), which is why completing inside a `def`/`val`/`class` body previously
    // produced nothing. Wrapping in a BLOCK — `val __completion = { <code> }` — accepts ANY
    // statement (a definition, an expression, even an import), so completion works anywhere in the
    // line. A line that IS an import is still placed at top level, where a bare `import` is valid.
    val importLine: Boolean = code.trim.starts(t"import ")
    val prefix:  Text = if importLine then contextLines else t"${contextLines}val __completion = {\n"
    val suffix:  Text = if importLine then t"" else t"\n}"
    val wrapped: Text = t"$prefix$code$suffix"
    val caret:   Int  = prefix.length + offset

    Scala.highlight(wrapped, caret = caret.z).completions.lay(Nil): completions =>
      completions.items.map: item =>
        CompletionItem(item.name, item.kind.toString.tt, item.signature.qualified)

  private def identifierChar(char: Char): Boolean = char.isLetterOrDigit || char == '_'

  // The offset at which the partial identifier ending at `offset` begins.
  private def identifierStart(code: Text, offset: Int): Int =
    var start: Int = offset
    while start > 0 && identifierChar(code.s.charAt(start - 1)) do start -= 1
    start

  // Splits `code` at the cursor into the member-selection base — everything up to and
  // including the `.` immediately before the partial member name — and that partial. A
  // `Unset` base means the cursor is not selecting a member (a first-token identifier, the
  // first segment of an import, …), so there is no fixed type to enumerate and cache against.
  def memberBase(code: Text, offset: Int): (Optional[Text], Text) =
    val start:  Int  = identifierStart(code, offset)
    val prefix: Text = code.keep(offset).skip(start)

    if start > 0 && code.s.charAt(start - 1) == '.' then (code.keep(start), prefix)
    else (Unset, prefix)

  // Keyword completion (the Scala compiler offers none). A coarse classification of the
  // cursor's syntactic position, derived from the tokens before it. Precise classification
  // would be a full parser — Scala's grammar is context-sensitive — so this trades accuracy
  // for a cheap approximation; its worst case is offering a keyword where it is not strictly
  // valid (harmless — the user ignores it) or missing one (a keyword just isn't suggested).
  // Scala 3 keywords only: `implicit` (superseded by `given`/`using`) and do-while `do`
  // (dropped in Scala 3) are deliberately excluded, so they are never offered.
  private val defKeywords: List[Text] =
    List(t"val", t"var", t"def", t"lazy", t"given", t"type", t"import", t"export", t"class",
         t"object", t"trait", t"enum", t"case", t"final", t"sealed", t"abstract", t"private",
         t"protected", t"override", t"inline", t"transparent", t"opaque", t"open", t"extension")

  private val exprKeywords: List[Text] =
    List(t"new", t"if", t"for", t"while", t"try", t"throw", t"return", t"this", t"super",
         t"true", t"false", t"null")

  private val paramKeywords: List[Text] = List(t"using", t"inline", t"val", t"var")

  // At a statement boundary either a definition or an expression may begin.
  private val statementKeywords: List[Text] = defKeywords ++ exprKeywords

  // Each modifier constrains what may follow it, so a curated follow-set is far more useful
  // than the whole definition list — especially the soft modifiers.
  private val modifierFollowers: Map[Text, List[Text]] =
    Map
     ( t"transparent" -> List(t"inline", t"trait"),
       t"inline"      -> List(t"def", t"given", t"val"),
       t"opaque"      -> List(t"type"),
       t"open"        -> List(t"class"),
       t"sealed"      -> List(t"trait", t"class", t"abstract"),
       t"abstract"    -> List(t"class", t"override"),
       t"final"       -> List(t"class", t"object", t"def", t"val", t"var", t"case"),
       t"override"    -> List(t"def", t"val", t"var", t"type", t"given"),
       t"lazy"        -> List(t"val"),
       t"private"     -> List(t"val", t"var", t"def", t"lazy", t"given", t"type", t"class",
                              t"object", t"trait", t"enum", t"final", t"sealed", t"override"),
       t"protected"   -> List(t"val", t"var", t"def", t"lazy", t"given", t"type", t"class",
                              t"object", t"trait", t"enum", t"final", t"sealed", t"override") )

  // After any of these, an expression (a new operand) may begin.
  private val exprTriggers: Set[Text] =
    Set(t"=", t"(", t",", t"return", t"throw", t"if", t"while", t"else", t"do", t"yield",
        t"then", t"<-", t"=>")

  // `with` is valid only in a template header (`extends A with B`), NOT after a type ascription
  // `:` (a compound `A with B` there is Scala-2 style, superseded by `A & B`).
  private val withTriggers: Set[Text] = Set(t"extends", t"with", t"derives")

  // Keywords that make a following identifier a NAME/TYPE/PATH rather than a value, so it is not
  // an infix receiver (`val x`, `def f`, `import p`, `case P`, `new T`, …).
  private val infixExcluded: Set[Text] =
    Set(t"val", t"var", t"def", t"type", t"class", t"object", t"trait", t"enum", t"given",
        t"package", t"import", t"export", t"case", t"extension", t"new")

  private val defKeywordSet: Set[Text] = Set(t"def", t"class", t"trait", t"enum", t"given", t"extension")
  private val valueAccents:  Set[Text] = Set(t"term", t"number", t"string", t"typal")

  // Every Scala 3 keyword, hard and soft. Harlequin's lexer tags SOFT keywords (`inline`,
  // `transparent`, `opaque`, `open`, `using`, `extension`, …) as identifiers — as Scala does —
  // so they'd otherwise pass as infix receivers; this set excludes them (and the hard keywords).
  private val allKeywords: Set[Text] =
    Set(t"abstract", t"case", t"catch", t"class", t"def", t"do", t"else", t"enum", t"export",
        t"extends", t"false", t"final", t"finally", t"for", t"given", t"if", t"implicit",
        t"import", t"lazy", t"match", t"new", t"null", t"object", t"override", t"package",
        t"private", t"protected", t"return", t"sealed", t"super", t"then", t"this", t"throw",
        t"trait", t"true", t"try", t"type", t"val", t"var", t"while", t"with", t"yield",
        t"as", t"derives", t"end", t"extension", t"infix", t"inline", t"opaque", t"open",
        t"transparent", t"using")

  // Harlequin's lexer tags a symbolic operator (`+`, `::`, `<=`, …) as an identifier, just as
  // Scala treats it, and a closing bracket as a symbol — so accent alone can't tell an operator
  // (after which an expression is expected) from a value. `symbolic` (all non-word, non-space
  // characters) distinguishes them by text.
  private def symbolic(text: Text): Boolean =
    text.s.length > 0 && text.s.forall { char => !identifierChar(char) && !char.isWhitespace }

  // The innermost unclosed bracket before the cursor, and (for `(`) whether it opens a
  // `def`/`class`/`given`/`extension` parameter list rather than an argument list — told
  // apart by the keyword one or two significant tokens before the `(`.
  private def enclosingContext(sig: IndexedSeq[Token]): (Text, Boolean) =
    var stack: List[(Text, Boolean)] = Nil
    var i = 0
    while i < sig.length do
      sig(i).text match
        case t"(" =>
          val param = i >= 1 && (defKeywordSet.contains(sig(i - 1).text)
                                 || (i >= 2 && defKeywordSet.contains(sig(i - 2).text)))
          stack = (t"(", param) :: stack
        case t"[" => stack = (t"[", false) :: stack
        case t"{" => stack = (t"{", false) :: stack
        case t")" | t"]" | t"}" => if stack.nonEmpty then stack = stack.tail
        case _ => ()
      i += 1

    if stack.isEmpty then (t"", false) else stack.head

  // Whether the whitespace immediately before the cursor contains a line break (so the cursor
  // sits on a fresh line — a statement boundary at top level or inside a `{ … }` block).
  private def trailingNewline(before: Text): Boolean =
    before.s.reverseIterator.takeWhile(_.isWhitespace).contains('\n')

  // The keywords valid at the cursor, from a coarse token-based classification of `before`.
  private def keywordsAt(before: Text): List[Text] =
    val sig: IndexedSeq[Token] =
      tokenize(before).filter { tok => tok.accent != t"unparsed" && tok.text.trim != t"" }.toIndexedSeq

    if sig.isEmpty then statementKeywords
    else
      val last = sig(sig.length - 1)
      val text = last.text
      val (opener, param) = enclosingContext(sig)

      if text == t"." then Nil
      else if text == t"{" || text == t";" then statementKeywords
      else if modifierFollowers.contains(text) then modifierFollowers(text)
      // A type position (ascription or bound): the type itself is an identifier — no keyword,
      // and specifically no `with` (which belongs only to a template header, see below).
      else if text == t":" || text == t"<:" || text == t">:" then Nil
      else if exprTriggers.contains(text) then
        if (text == t"(" || text == t",") && param then paramKeywords else exprKeywords
      else if withTriggers.contains(text) then List(t"with")
      else if text == t"case" then Nil
      else
        val closeBracket = text == t")" || text == t"]" || text == t"}"
        val operator     = symbolic(text) && !closeBracket
        val valueEnding  = closeBracket || text == t"_" || (valueAccents.contains(last.accent) && !symbolic(text))

        // A complete value + newline starts a new statement (top level / in a block); an
        // operator does not. A value with no newline (postfix) offers no keyword here — `match`
        // is offered only via the infix path (`completionsAt`), which requires a trailing space.
        if valueEnding && trailingNewline(before) && (opener == t"" || opener == t"{")
        then statementKeywords
        else if operator then exprKeywords
        else Nil

  // Scala keywords valid at the cursor, as completion candidates. Merged with the compiler's
  // (member/name) completions, which never include keywords.
  def keywordCompletions(code: Text, offset: Int): List[CompletionItem] =
    val start:  Int  = identifierStart(code, offset)
    val prefix: Text = code.keep(offset).skip(start)

    keywordsAt(code.keep(start)).filter(_.starts(prefix)).map(CompletionItem(_, t"keyword", t""))

  // The infix-completion receiver: when the cursor is at `<value-expr> <space> <partial>` — a
  // value followed by whitespace, not a member selection — returns that value expression with a
  // synthetic trailing `.` (so it reuses the member-completion path) and the partial method
  // name. `Unset` when there is no value receiver (the token before the space is a keyword,
  // operator, comma, or open bracket, or a name/type/path in a definition/import position).
  def infixBase(code: Text, offset: Int): (Optional[Text], Text) =
    val s = code.s
    val start: Int = identifierStart(code, offset)
    val prefix: Text = code.keep(offset).skip(start)

    // Require whitespace immediately before the partial (the space between receiver and method).
    if start == 0 || !s.charAt(start - 1).isWhitespace then (Unset, prefix) else
      val before: Text = code.keep(start)
      val sig: IndexedSeq[Token] =
        tokenize(before).filter { tok => tok.accent != t"unparsed" && tok.text.trim != t"" }.toIndexedSeq

      if sig.isEmpty then (Unset, prefix) else
        val last = sig(sig.length - 1)
        val text = last.text
        val closeBracket = text == t")" || text == t"]" || text == t"}"

        // A value receiver: an identifier/literal (not a symbolic operator) or a closing bracket,
        // and NOT a keyword — including a soft keyword (`inline`, `transparent`, …) the lexer tags
        // as an identifier, which introduces a definition rather than being an infix receiver.
        val valueEnding =
          !allKeywords.contains(text)
          && (closeBracket || text == t"_" || (valueAccents.contains(last.accent) && !symbolic(text)))

        if !valueEnding then (Unset, prefix) else
          // Strip the trailing whitespace, find where the value expression begins, and reject it
          // if it is a bare name/path introduced by a definition/import/`new` keyword.
          var end = start
          while end > 0 && s.charAt(end - 1).isWhitespace do end -= 1
          val baseStart = expressionStart(code.keep(end))
          val base: Text = code.keep(end).skip(baseStart)

          val preceding: Text =
            tokenize(code.keep(baseStart))
            . filter { tok => tok.accent != t"unparsed" && tok.text.trim != t"" }
            . lastOption.map(_.text).getOrElse(t"")

          if infixExcluded.contains(preceding) then (Unset, prefix) else (t"$base.", prefix)

  // The character index where the value expression ending at the last character of `s` begins:
  // scans back over identifiers, `.`, and balanced bracket groups, stopping at an operator, a
  // space, or a boundary at depth 0.
  private def expressionStart(s: Text): Int =
    val str = s.s
    var i = str.length - 1
    var depth = 0
    var scanning = true

    while i >= 0 && scanning do
      val c = str.charAt(i)
      if c == ')' || c == ']' || c == '}' then { depth += 1; i -= 1 }
      else if c == '(' || c == '[' || c == '{' then
        if depth == 0 then { i += 1; scanning = false } else { depth -= 1; i -= 1 }
      else if depth > 0 then i -= 1
      else if identifierChar(c) || c == '.' then i -= 1
      else { i += 1; scanning = false }

    if i < 0 then 0 else i

  // Which command a setting is toggled through: `/set` for compiler options (diagnostics, the syntax
  // mode, the `experimental` master switch), `/language` for `import language.*` features.
  enum Kind:
    case Set, Language

  // A toggleable setting: the user-facing `name`, the scalac `flag` it adds to every subsequent line's
  // compile, a short `description`, the `kind` (which command addresses it), and whether it is an
  // EXPERIMENTAL language feature (`import language.experimental.*`) — those become available only after
  // `/set experimental`. Held as raw flag text (rather than anthology's version-typed `scalacOptions`)
  // so flame can build `Scalac.Option[version](flag)` directly for its `version`.
  case class Setting
     (name: Text, flag: Text, description: Text, kind: Kind, experimental: Boolean = false)

  // Every recognised setting. `/set` covers the compiler options; `/language` covers `import
  // language.*` features — the plain ones always, the experimental ones only once `experimental` is on.
  // (`async` is a session toggle, not a compiler flag, so it is handled separately, not listed here.)
  // Order is the listing order of `/set` / `/language` with no argument.
  val settings: List[Setting] =
    import Kind.*
    List
     ( Setting(t"experimental", t"-experimental",
         t"allow experimental language features and definitions", Set),
       Setting(t"explain", t"-explain", t"print a detailed explanation for each error", Set),
       Setting(t"explicit-nulls", t"-Yexplicit-nulls",
         t"treat reference types as non-nullable (Null is a separate type)", Set),
       Setting(t"deprecation", t"-deprecation", t"warn about uses of deprecated APIs", Set),
       Setting(t"feature", t"-feature",
         t"warn about advanced language features that should be enabled explicitly", Set),
       Setting(t"new-syntax", t"-new-syntax",
         t"require the new `then`/`do`-free control-flow syntax", Set),

       // Plain `import language.*` features.
       Setting(t"postfixOps", t"-language:postfixOps", t"allow postfix operator notation", Language),
       Setting(t"implicitConversions", t"-language:implicitConversions",
         t"allow the definition and use of implicit conversions", Language),
       Setting(t"reflectiveCalls", t"-language:reflectiveCalls",
         t"allow structural-type member access via reflection", Language),
       Setting(t"dynamics", t"-language:dynamics",
         t"allow defining classes that extend `scala.Dynamic`", Language),
       Setting(t"existentials", t"-language:existentials",
         t"allow writing existential types explicitly", Language),
       Setting(t"strictEquality", t"-language:strictEquality",
         t"require a `CanEqual` instance for `==` / `!=`", Language),
       Setting(t"adhocExtensions", t"-language:adhocExtensions",
         t"allow extending a non-`open` class from another file", Language),
       Setting(t"unsafeNulls", t"-language:unsafeNulls",
         t"relax null-checking (nullable references usable as non-null)", Language),

       // `import language.experimental.*` features — available only after `/set experimental`.
       Setting(t"captureChecking", t"-language:experimental.captureChecking",
         t"enable experimental capture checking", Language, experimental = true),
       Setting(t"saferExceptions", t"-language:experimental.saferExceptions",
         t"enable the checked-exceptions (`saferExceptions`) feature", Language, experimental = true),
       Setting(t"pureFunctions", t"-language:experimental.pureFunctions",
         t"enable pure-function types (part of capture checking)", Language, experimental = true),
       Setting(t"namedTuples", t"-language:experimental.namedTuples",
         t"enable named tuples", Language, experimental = true),
       Setting(t"modularity", t"-language:experimental.modularity",
         t"enable experimental modularity features", Language, experimental = true),
       Setting(t"betterFors", t"-language:experimental.betterFors",
         t"enable the improved `for`-comprehension desugaring", Language, experimental = true),
       Setting(t"erasedDefinitions", t"-language:experimental.erasedDefinitions",
         t"allow `erased` parameters and definitions", Language, experimental = true),
       Setting(t"genericNumberLiterals", t"-language:experimental.genericNumberLiterals",
         t"enable generic number literals via `FromDigits`", Language, experimental = true) )

  // The `/language` features to offer in completion for `partial`: the plain ones always, the
  // experimental ones only when `experimentalOn`. Each item's name is the bare feature name, inserted
  // after the `/language ` prefix (the front-ends complete the argument, not the whole line).
  def languageCompletions(partial: Text, experimentalOn: Boolean): List[CompletionItem] =
    settings
     . filter { setting => setting.kind == Kind.Language && (!setting.experimental || experimentalOn) }
     . filter { setting => setting.name.starts(partial) }
     . map { setting => CompletionItem(setting.name, t"command", setting.description) }

  // The `/`-commands the engine itself recognises, with help text. Front-ends offer these
  // as completions when a line begins with `/`; the CLI appends its own client-only
  // commands (`/disconnect`, `/quit`) to these. Every compiler setting contributes a
  // `/set <name>` entry so each is offered (and documented) in tab-completion.
  val slashCommands: List[(Text, Text)] =
    List(t"/context" -> t"show the imports currently in scope")
    ::: List(t"/set async" -> t"evaluate submissions asynchronously (slow results arrive later)")
    ::: settings.filter(_.kind == Kind.Set).map { setting => t"/set ${setting.name}" -> setting.description }
    ::: List(t"/language" -> t"enable a Scala `language` feature (its argument is completed per session)")
    ::: List
         ( t"/tasty"    -> t"show the rendered TASTy (typed AST) of an expression",
           t"/bytecode" -> t"show the JVM bytecode of an expression or definition",
           t"/unimport" -> t"remove an earlier import from scope (by the tokens it was imported with)" )

  def slashCompletions(prefix: Text): List[CompletionItem] =
    slashCommands.filter { (name, _) => name.starts(prefix) }.map: (name, help) =>
      CompletionItem(name, t"command", help)

  // The leading tokens of the `/`-commands the ENGINE recognises (`/set`, `/context`, `/tasty`, …),
  // so a front-end can tell a real command from a typo without hard-coding the list. Connection-level
  // commands (`/session`, `/clear`, `/quit`, `/disconnect`) belong to the front-end and are handled
  // before this check.
  lazy val commandTokens: Set[Text] = slashCommands.map { (name, _) => name.cut(t" ").head }.to(Set)

  // True when `line` begins with a `/`-command the engine recognises. Used by both front-ends to give
  // the identical `unknown command` message (see `messages.unknownCommand`) for anything else.
  def isCommand(line: Text): Boolean = commandTokens.contains(line.cut(t" ").head)

  // The user-facing status/notice lines that BOTH front-ends show, kept here so the CLI and the web
  // stay word-for-word identical. Each is a complete line carrying the `flame:` program prefix (as the
  // engine's other messages, e.g. `/tasty` usage, already do); the front-end adds its own trailing
  // newline (CLI) or styling (web).
  object messages:
    def session(name: Text): Text = t"flame: session $name"

    def sessionList(name: Text, names: List[Text]): Text =
      t"flame: session $name — available: ${names.join(t", ")}"

    def switched(name: Text): Text = t"flame: switched to session $name"
    def noSession(name: Text): Text = t"flame: no session named '$name'"

    def joinFailed(requested: Text, started: Text): Text =
      t"flame: no session named '$requested' — started session $started"

    def unknownCommand(line: Text): Text = t"flame: unknown command: $line"

    val serverRestarted: Text =
      t"flame: the server restarted — your previous session (definitions, imports and settings) was lost"

  // The Scala type of an expression, read from a typechecked highlight of
  // `<context> val __result = <code>`: the binding's token carries the resolved type as a
  // `Syntax`. The context (the session's imports and prior wrapper objects) is prepended so
  // the type resolves even for expressions that use session definitions. Only meaningful for
  // expression lines (statements have no value).
  def resultType(context: List[Text], code: Text)(using Scalac[?], LocalClasspath): Optional[Syntax] =
    import highlighting.typecheckedScala
    val contextLines: Text = context.map { line => t"$line\n" }.join
    val tokens = Scala.highlight(t"${contextLines}val __result = $code").lines.to(List).flatten

    tokens.find(_.text == t"__result").optional.let(_.meta).let(_.tpe)

  // The simple base type name of a qualified type, lowercased at the first letter — e.g.
  // `scala.collection.immutable.List[scala.Int]` → `list`, `Foo is Addable by Bar` → `addable`.
  // `Unset` when the base does not begin with a letter or isn't a plain type name (function,
  // tuple, intersection, literal types), so the caller falls back to `resN`.
  def baseName(qualified: Text): Optional[Text] =
    val q: Text = qualified.trim
    if q.contains(t"=>") then Unset else
      // Soundness infix `X is Y [by Z]` names the trait `Y`; otherwise take the type
      // constructor (strip any `[…]` type arguments) and its last dotted segment.
      val core: Text =
        if q.contains(t" is ")
        then q.cut(t" is ", 2).last.trim.s.takeWhile { char => !char.isWhitespace && char != '[' }.tt
        else q.cut(t"[", 2).head.trim

      val simple: String = core.cut(t".").last.s

      if simple.length > 0 && simple.charAt(0).isLetter && simple.forall(identifierChar)
      then (simple.charAt(0).toLower.toString + simple.substring(1).nn).tt
      else Unset

  // A binding name for a new result: `base` if free, else `base2`, `base3`, … A candidate is
  // taken if it's a keyword or already resolves in the session scope (a prior binding or an
  // imported name) — reusing it would shadow, or make two same-scope wildcard imports ambiguous
  // on a later line. Resolution is checked by asking the compiler to complete the bare name.
  def freeName(base: Text, context: List[Text])(using Scalac[?], LocalClasspath): Text =
    def taken(candidate: Text): Boolean =
      allKeywords.contains(candidate)
      || complete(context, candidate, candidate.length).exists(_.name == candidate)

    if !taken(base) then base else
      var n = 2
      while taken(t"$base${n.toString.tt}") do n += 1
      t"$base${n.toString.tt}"

  // Whether `code` is a single bare identifier — an existing name typed on its own, with no
  // operator, selection, application, or whitespace. Such a line is echoed under that very name
  // (see `evaluate`/`echoCode`) instead of being bound to a fresh, numbered result: it already
  // has a name, so there is no reason to mint another. Keywords — including the literals `true`,
  // `false` and `null` — are excluded, so those still take a type-derived name.
  def isBareIdentifier(code: Text): Boolean =
    val identifier: String = code.s.trim.nn

    identifier.length > 0
    && !identifier.charAt(0).isDigit
    && identifier.forall { char => identifierChar(char) || char == '$' }
    && !allKeywords.contains(identifier.tt)

  // A `val`/`var`/`def` line's kind and the name it binds, past any leading modifiers — so the REPL
  // can show a definition's name/value/signature the way it shows an auto-named expression's. `Unset`
  // for anything else (imports, `given`/`type`/`class`, pattern `val (a, b) = …`, symbolic operator
  // names, plain statements), which then keep their current no-output behaviour.
  private val definitionModifiers: Set[Text] =
    Set(t"private", t"protected", t"final", t"lazy", t"override", t"inline", t"transparent",
        t"implicit", t"sealed", t"abstract", t"open")

  def definitionKind(line: Text): Optional[(Text, Text)] =
    def scan(rest: String): Optional[(Text, Text)] =
      val word:  String = rest.takeWhile(!_.isWhitespace).nn
      val after: String = rest.drop(word.length).nn.dropWhile(_.isWhitespace).nn

      if definitionModifiers.contains(word.tt) then scan(after)
      else if (word == "val" || word == "var" || word == "def") && after.length > 0 then
        val name: String = after.takeWhile { char => identifierChar(char) || char == '$' }.nn
        if name.length == 0 || name.charAt(0).isDigit then Unset else (word.tt, name.tt)
      else Unset

    scan(line.trim.s)

  // The index in `line` of the `def` body's `=` — the first `=` at bracket depth 0 that is not part
  // of `=>`, `==`, `<=`, `>=` or `!=` (so default-argument `=`s, inside parens, and a `Int => String`
  // return type are all skipped). `-1` if there is none (an abstract `def`, not valid at the REPL).
  private def defBodyEquals(line: Text): Int =
    val s: String = line.s
    var depth: Int = 0
    var i: Int = 0
    var found: Int = -1
    while found < 0 && i < s.length do
      s.charAt(i) match
        case '(' | '[' | '{' => depth += 1
        case ')' | ']' | '}' => depth -= 1
        case '=' if depth == 0 =>
          val prev = if i > 0 then s.charAt(i - 1) else ' '
          val next = if i + 1 < s.length then s.charAt(i + 1) else ' '
          if next != '>' && next != '=' && prev != '<' && prev != '>' && prev != '!' && prev != '='
          then found = i
        case _ => ()
      i += 1

    found

  // A `def`'s header — everything up to (not including) its body `=`, trimmed — e.g. `def f(a: Int):
  // String`. Used to display the signature without invoking the method.
  private def defHeader(line: Text): Text =
    val equals = defBodyEquals(line)
    (if equals < 0 then line else line.s.substring(0, equals).nn.tt).trim

  // Whether a `def` header annotates its return type: a `:` at bracket depth 0 (a parameter's `:` is
  // inside `(…)`, a type-parameter bound's inside `[…]`, so only the return-type colon is at depth 0).
  private def annotatesReturnType(header: Text): Boolean =
    val s: String = header.s
    var depth: Int = 0
    var i: Int = 0
    var found: Boolean = false
    while !found && i < s.length do
      s.charAt(i) match
        case '(' | '[' | '{' => depth += 1
        case ')' | ']' | '}' => depth -= 1
        case ':' if depth == 0 => found = true
        case _ => ()
      i += 1

    found

  // Applies `name` to a `???` for each parameter its header declares (`f(???, ???)(using ???)`), so
  // typechecking `{ <def>; <application> }` yields the method's return type — used to infer an
  // omitted return type without invoking the method (`???` is `Nothing`, so it conforms everywhere).
  // Nullary `def`s (no parameter list) apply to nothing.
  private def defApplication(header: Text, name: Text): Text =
    // Each top-level `(…)` group's contents, in order.
    def groups(text: Text): List[Text] =
      val (found, _, _) = text.s.foldLeft((List[Text](), t"", 0)):
        case ((found, current, depth), '(') =>
          if depth == 0 then (found, current, 1) else (found, t"$current(", depth + 1)

        case ((found, current, depth), ')') =>
          if depth == 1 then (current :: found, t"", 0) else (found, t"$current)", depth - 1)

        case ((found, current, depth), char) =>
          if depth > 0 then (found, t"$current$char", depth) else (found, current, depth)

      found.reverse

    // The number of top-level (comma-separated) parameters in one clause's contents.
    def params(clause: Text): Int =
      if clause.trim == t"" then 0 else
        var depth: Int = 0
        var commas: Int = 0
        clause.s.foreach:
          case '(' | '[' | '{' => depth += 1
          case ')' | ']' | '}' => depth -= 1
          case ',' if depth == 0 => commas += 1
          case _ => ()
        commas + 1

    val clauses: List[Text] = groups(header)
    val applied: Text = clauses.map: clause =>
      val prefix: Text = if clause.trim.starts(t"using ") || clause.trim.starts(t"implicit ") then t"using " else t""
      val holes:  Text = List.fill(params(clause))(t"???").join(t", ")
      t"($prefix$holes)"
    . join

    t"$name$applied"

  // A `def`'s displayed signature: its header, plus the inferred return type when the source omits
  // one (`def f(a: Int) = a` → `def f(a: Int): Int`). Inference typechecks the def applied to `???`s;
  // if that fails (an unusual signature), the header is shown without a return type.
  def defSignature(line: Text, name: Text, context: List[Text])(using Scalac[?], LocalClasspath): Text =
    val header: Text = defHeader(line)
    if annotatesReturnType(header) then header else
      val probe: Text = t"{ $line\n${defApplication(header, name)} }"
      safely(resultType(context, probe)).let(_.qualified) match
        case tpe: Text => t"$header: $tpe"
        case _         => header

  object Prelude:
    val empty: Prelude = Prelude(Nil, Nil)

  // Declarations lifted from an inline binding block to seed the REPL context:
  // `imports` are re-injected into every line; `seedTasty` is a TASTy-pickled
  // block of the lifted definitions and binding accessors (carried as data from
  // macro-expansion time), recompiled once into a seed object with full type
  // fidelity.
  case class Prelude(imports: List[String], seedTasty: List[String])

  // How a result value is rendered for display. `Inspect` (the default, the CLI) uses spectacular's
  // `Inspectable` to produce a teletype/text rendering; `Html` (the web front-end) uses the
  // `flame.HtmlRender` cascade (a `Renderable` → HTML, else `Showable` → text, else `toString`),
  // producing an HTML string. Both render INSIDE the compiled wrapper (where the value's static type
  // is known) and stash the resulting `Text` via `ReplBridge`, so the choice only changes the
  // rendering call emitted into the wrapper.
  enum Rendering:
    case Inspect, Html

  def make[version <: Scalac.Versions]
    ( prelude: Repl.Prelude, render: Repl.Rendering = Repl.Rendering.Inspect )
    ( using Scalac[version], Classloader, TemporaryDirectory )
  :   Repl[version] =

    new Repl[version](prelude = prelude, render = render)

  inline def apply[version <: Scalac.Versions](inline body: Unit = ())
    ( using scalac: Scalac[version], classloader: Classloader, temporary: TemporaryDirectory )
  :   Repl[version] =

    ${ReplMacro.bound[version]('body, 'scalac, 'classloader, 'temporary)}

class Repl[version <: Scalac.Versions]
  ( layout:  Repl.Layout    = Repl.Layout.Standard(),
    prelude: Repl.Prelude   = Repl.Prelude.empty,
    render:  Repl.Rendering = Repl.Rendering.Inspect )
  ( using scalac: Scalac[version], classloader: Classloader, temporary: TemporaryDirectory ):

  import Repl.Outcome
  import Repl.Kind

  val session: Long = ReplBridge.freshSession()

  // Serializes `interpret` across connections: the shared `Scalac` compiler is
  // not reentrant and the REPL's state is mutable.
  private val mutex: Mutex = Mutex()

  // Caches the full member list for each `expr.` base completed on the current line, so the
  // live suggestion and Tab completion filter it instead of recompiling per keystroke. The
  // type of `expr` is fixed within a line, so the cache holds until the next submission,
  // which clears it. Filled under `mutex`; `@volatile` makes the submit-time clear visible
  // without re-entering the mutex.
  @volatile
  private var completionCache: Map[Text, List[Repl.CompletionItem]] = Map()

  private var index:   Int        = 0
  private var result:  Int        = 0
  private var history: List[Text] = Nil
  private var seeded:  Boolean     = false

  // The user's persistent imports. An `import` statement creates no member, so — unlike
  // the `val`/`def`/`class` definitions that later lines see through the history imports
  // — it would vanish after its own line. We accumulate the imports each line introduces
  // and re-inject them into every subsequent line, exactly as the prelude's imports are.
  private var imports: List[Text] = Nil

  // The settings the user has switched on with `/set <name>` or `/language <name>`; each contributes
  // its scalac flag to every later line's compile (see `effectiveScalac`). Holds setting NAMES (keys
  // into `Repl.settings`), not the flags themselves.
  private var enabledSettings: Set[Text] = Set()

  // Async mode (`/set async`): when on, a front-end evaluates each submission on a background worker,
  // acknowledging with a placeholder and delivering the real result out-of-band once it is ready, so a
  // slow computation never blocks the editor. A plain per-session boolean (not a compiler flag).
  private var asyncMode: Boolean = false
  def asyncEnabled: Boolean = asyncMode

  // The sink a run's stdout is streamed to as it appears (async mode), set by `react` around the run
  // (under `mutex`, so only one run ever uses it at a time) and reset afterwards. `_ => ()` means no
  // streaming — the output is still captured and returned in the reply as usual.
  private var outputSink: Text => Unit = _ => ()

  private val out: Path on Linux = unsafely(temporaryDirectory/Uuid())
  locally(unsafely(out.create[Directory]()))

  private lazy val loader: Classloader =
    LocalClasspath((Classpath.Directory(out) :: Nil)*).classloader(classloader)

  // The compile classpath must come from the *same* loader the wrapper objects
  // are run against (`classloader`, below), not from `classloaders.threadContext`:
  // `interpret` may run on a background worker thread (the TCP accept loop) whose
  // thread-context loader differs from the REPL's, which would compile against one
  // copy of the soundness classes and run against another — a loader-constraint
  // violation.
  private def classpath(using System): LocalClasspath =
    val entries = Classpath.Directory(out) :: (classloader.classpath.match
      case classpath: LocalClasspath => classpath.entries

      case _ =>
        unsafely(System.properties.java.`class`.path().decode[LocalClasspath]).entries)

    LocalClasspath(entries*)

  // The compiler to use for the next line: the session's `Scalac` plus the flag of every setting the
  // user has switched on (`/set <name>`). `Scalac.Option` is contravariant in its version, so an
  // option built for this `version` is a valid extra flag for it.
  private def effectiveScalac: Scalac[version] =
    val experimentalOn: Boolean = enabledSettings.contains(t"experimental")

    // An enabled experimental language feature is only APPLIED while `experimental` is on — so turning
    // `experimental` off (without un-picking the features) can't leave a `-language:experimental.X`
    // flag stranded, which would make the compiler reject every line.
    val extra: List[Scalac.Option[version]] =
      Repl.settings
        .filter { setting => enabledSettings.contains(setting.name) }
        .filter { setting => !setting.experimental || experimentalOn }
        .map { setting => Scalac.Option[version](setting.flag) }

    if extra.isEmpty then scalac else Scalac(scalac.options ::: extra)

  // Compiles `code` as the next wrapper object and, on success, loads it (which
  // runs its body). `rendered` is evaluated after a successful run to supply the
  // `Outcome.Ran` value — `Unset` for statements, or the inspected result for an
  // expression line.
  private def compile(imports: List[Text], code: Text)(rendered: => Optional[Text])
      (using Monitor, System, Probate)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    val name:    Text = layout.objectName(index)
    val source:  Text = layout.wrap(index, history, imports, code)
    val process       = effectiveScalac(classpath)(Map(t"$name.scala" -> source), out)
    val outcome       = process.complete()
    val notices       = process.notices.to(List)

    outcome match
      case CompileResult.Crash(trace) =>
        Outcome.Crashed(notices, trace)

      case CompileResult.Failure =>
        Outcome.Rejected(notices)

      case CompileResult.Success =>
        index += 1
        history = history :+ name

        // Capture whatever the user's code prints to stdout. Scala's `println`
        // writes to `scala.Console.out` (a thread-local), while Java code writes
        // to `System.out` (process-global), so redirect both. `System.out` is
        // process-global, but this runs under `submit`'s `mutex`, so only one run
        // ever redirects it at a time, and the window is just the run itself.
        //
        // `captured` also TEES to `outputSink`: on each flush (an auto-flushing `PrintStream` flushes
        // after every write/`println`) the newly-captured bytes are decoded and handed to the sink, so
        // async mode can stream stdout as it appears. The full text is still returned in `output`.
        val captured: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream()
        val sink:     Text => Unit             = outputSink

        val teeing: ji.OutputStream = new ji.OutputStream:
          private var streamed: Int = 0
          def write(byte: Int): Unit = captured.write(byte)
          override def write(bytes: Array[Byte] | Null, off: Int, len: Int): Unit =
            captured.write(bytes, off, len)

          override def flush(): Unit =
            val all: Array[Byte] = captured.toByteArray.nn
            if all.length > streamed then
              val chunk = String(all, streamed, all.length - streamed, "UTF-8").tt
              streamed = all.length
              safely(sink(chunk))

        val stream:   ji.PrintStream = ji.PrintStream(teeing, true, "UTF-8")
        val previous: ji.PrintStream = jl.System.out.nn
        jl.System.setOut(stream)

        def output: Text =
          stream.flush()
          captured.toString("UTF-8").nn.tt

        try
          // Seed accessors read their session from this thread-local.
          ReplBridge.setCurrentSession(session)
          scala.Console.withOut(stream)(loader.on(t"$name$$"))
          Outcome.Ran(notices, rendered, output)
        catch
          case error: ExceptionInInitializerError =>
            Outcome.Threw(notices, Optional(error.getCause).or(error), output)

          // Running arbitrary user code can throw anything, including `Error`s
          // (`LinkageError`, `StackOverflowError`, …), which must not escape and
          // kill the session.
          case error: Throwable =>
            Outcome.Threw(notices, error, output)
        finally
          jl.System.setOut(previous)

  // The import statements a submitted line introduces — each `;`-or-newline segment that
  // begins with `import`, so any definition or expression sharing the line is left out
  // (those persist as history members, or not at all). A single `import a.*, b.C` statement
  // is a SEQUENCE of imports: its comma-separated clauses are split into one `import` each, so
  // the session tracks (and confirms, lists, and un-imports) them individually.
  private def importsIn(line: Text): List[Text] =
    line.cut(t"\n").flatMap(_.cut(t";")).map(_.trim).filter(_.starts(t"import ")).flatMap: statement =>
      importClauses(statement.skip(t"import ".length)).map { clause => t"import $clause" }

  // Splits an import clause-list on its top-level commas — those outside any `{…}` selector group
  // (or `[…]`/`(…)`), which are separators between clauses, not within one — trimming each clause
  // and dropping empties. E.g. `a.*, b.{c, d}, e.given` → `a.*`, `b.{c, d}`, `e.given`.
  private def importClauses(clauses: Text): List[Text] =
    val (parts, last, _) = clauses.s.foldLeft((List[Text](), t"", 0)):
      case ((parts, current, depth), char @ ('{' | '[' | '(')) => (parts, t"$current$char", depth + 1)
      case ((parts, current, depth), char @ ('}' | ']' | ')')) => (parts, t"$current$char", depth - 1)
      case ((parts, current, 0),     ',')                      => (current.trim :: parts, t"", 0)
      case ((parts, current, depth), char)                     => (parts, t"$current$char", depth)

    (last.trim :: parts).reverse.filter(_ != t"")

  // The imports to prepend to a line's wrapper: the prelude's plus the user's accumulated
  // imports, minus any the line itself repeats (so re-importing the same path never makes
  // a duplicate import within one wrapper's scope).
  private def contextImports(line: Text): List[Text] =
    val repeated = importsIn(line)
    (prelude.imports.map(_.tt) ::: imports).filter { each => !repeated.contains(each) }

  // The two lines that render the value bound to `ref` and stash the rendering under `key` for
  // the outcome: an `@experimental` initializer (experimental mode is enabled just here, since
  // spectacular `inspect` is `@experimental`, so the line compiles even when the session is not
  // in experimental mode) whose body renders per the session's `render` mode — `Inspect` uses
  // `.inspect` (teletype/text, CLI), `Html` uses `flame.HtmlRender.render` (the typeclass
  // cascade → HTML, web). The rendering runs INSIDE the wrapper, where `ref`'s static type is
  // known.
  private def renderInto(ref: Text, key: Text): List[Text] =
    val put: Text = render match
      case Repl.Rendering.Inspect =>
        t"import spectacular.inspect; flame.ReplBridge.put(${session.toString.tt}L, \"$key\", $ref.inspect)"

      case Repl.Rendering.Html =>
        t"flame.ReplBridge.put(${session.toString.tt}L, \"$key\", flame.HtmlRender.render($ref))"

    List
      ( t"@scala.annotation.experimental private val ${ref}_shown: scala.Unit =",
        t"  { $put }" )

  private def expressionCode(name: Text, key: Text, line: Text): Text =
    (t"val $name = $line" :: renderInto(name, key)).join(t"\n")

  // Like `expressionCode`, but for a line that is just an existing identifier: the value is bound
  // to a PRIVATE, differently-named val (`${name}_echo`, not `val $name = $name`, which would be a
  // self-reference) — so nothing new is imported by later lines and no fresh numbered name is
  // minted; the result is shown under the name the user already has. The bound `val` is NOT
  // `@experimental`, so an experimental identifier is still rejected outside experimental mode
  // (only the `inspect` renderer, which is itself `@experimental`, sits in that scope).
  private def echoCode(name: Text, key: Text, line: Text): Text =
    val bound: Text = t"${name}_echo"
    (t"private val $bound = $line" :: renderInto(bound, key)).join(t"\n")

  // The probe object body for `/tasty <expr>`: it reflects `<expr>`'s typed AST with hyperbole's
  // `syntax` macro (which runs at compile time and reifies the tree WITHOUT evaluating `<expr>`),
  // renders it to a coloured ANSI table via `flame.TastyRender`, and prints it — so the render is
  // captured as the line's `output`. Wrapped by `layout.wrap`, it sits inside the numbered object
  // with the history imports, so `<expr>` sees the session's prior imports and definitions.
  // The `val` is `@experimental` because hyperbole (`Introspect`/`internal`) is itself experimental
  // (built with `genericNumberLiterals`); the annotation enables experimental mode for its
  // initializer, so no session-wide `/set experimental` is needed — exactly as `expressionCode`
  // does for the `@experimental` `inspect` renderer.
  private def tastyProbe(expr: Text): Text =
    List
      ( t"@scala.annotation.experimental private val tastyRendered: scala.Unit =",
        t"  scala.Predef.print(flame.TastyRender.render(hyperbole.Introspect.syntax(false)($expr)))" )
    . join(t"\n")

  // `/tasty <expr>` shows the rendered TASTy (typed AST) of an expression — e.g. `/tasty (x: Int) =>
  // x + 1` or `/tasty List(1, 2, 3).map(_ + 1)`. It compiles the probe above in the session scope;
  // the expression is only type-checked and reflected, never run. A blank argument prints usage.
  private def tasty(line: Text)(using Monitor, System, Probate)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    val expr: Text = line.skip(t"/tasty".length).trim
    if expr == t"" then Outcome.Ran(Nil, Unset, t"flame: usage: /tasty <expression>\n")
    else compile(contextImports(line), tastyProbe(expr))(Unset)

  // `/bytecode <code>` disassembles the JVM bytecode of an expression or definition — e.g. `/bytecode
  // def fib(n: Int): Int = if n < 2 then n else fib(n - 1) + fib(n - 2)`. Unlike `/tasty`, this is a
  // pure COMPILE (no macro, no run): the code is compiled into a throwaway wrapper object in the
  // session scope, then `flame.BytecodeRender` reads that class's bytes back through the REPL's
  // classloader (never loading the class) and renders each method. An expression is wrapped in a
  // method first (so it becomes disassemblable code); a definition that cannot be a method's RHS is
  // then compiled directly. The wrapper is neither run nor added to `history`, so it does not pollute
  // the session — only `index` advances, to keep the object name unique.
  private def bytecode(line: Text)(using Monitor, System, Probate)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    val code: Text = line.skip(t"/bytecode".length).trim
    if code == t"" then Outcome.Ran(Nil, Unset, t"flame: usage: /bytecode <expression or definition>\n")
    else
      given LocalClasspath = classpath
      val name: Text = layout.objectName(index)

      // Compile `body` as the wrapper object's contents, returning the result and its notices.
      def attempt(body: Text): (CompileResult, List[Notice]) =
        val source  = layout.wrap(index, history, contextImports(line), body)
        val process = effectiveScalac(classpath)(Map(t"$name.scala" -> source), out)
        (process.complete(), process.notices.to(List))

      // Try the code as an expression (wrapped in a method) first, then as a raw definition.
      val (result, notices) = attempt(t"def bytecodeResult = $code") match
        case (CompileResult.Success, ns) => (CompileResult.Success, ns)
        case _                           => attempt(code)

      result match
        case CompileResult.Success =>
          index += 1
          BytecodeRender.render(t"$name$$.class")(using loader) match
            case rendered: Text if rendered.trim != t"" => Outcome.Ran(Nil, Unset, t"$rendered\n")
            case _ => Outcome.Ran(Nil, Unset, t"flame: no bytecode was produced\n")

        case CompileResult.Crash(trace) => Outcome.Crashed(notices, trace)
        case CompileResult.Failure      => Outcome.Rejected(notices)

  private def evaluate(line: Text)(using Monitor, System, Probate)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    given LocalClasspath = classpath

    val key: Text = t"result:${session.toString.tt}:${index.toString.tt}"

    // The scope this line sees, for typing the result and for checking name collisions.
    val context: List[Text] =
      (prelude.imports.map(_.tt) ::: imports) ::: history.map { name => t"import $name.{given, *}" }

    val syntax: Optional[Syntax] = safely(Repl.resultType(context, line))
    val tpe:    Optional[Text]   = syntax.let(_.qualified)

    // A line that is just an existing identifier (it type-checks, so it resolves) is echoed under
    // that same name — no fresh `val`, no new numbered result. Otherwise the binding name is the
    // result type's base name, lowercased and made unique in scope (`list`, `list2`, …), falling
    // back to `resN` when no letter-initial type name is available.
    val echo: Boolean = syntax.present && Repl.isBareIdentifier(line)

    val name: Text =
      if echo then line.trim
      else tpe.let(Repl.baseName(_)).let(Repl.freeName(_, context)).or(t"res${result.toString.tt}")

    // Try the line as an expression first; a definition or import fails to parse as `val <name> =
    // …` and falls back to being compiled as a plain statement. A bare identifier is inspected
    // directly (`echoCode`) rather than re-bound.
    val code: Text = if echo then echoCode(name, key, line) else expressionCode(name, key, line)

    val expression: Outcome = compile(contextImports(line), code):
      Optional(ReplBridge.fetch[String](session, key.s)).let(_.tt)

    expression match
      case _: Outcome.Rejected =>
        // The line is a definition or statement, not an expression. A `val`/`var`/`def` is displayed
        // like an auto-named expression (name/value/type, or a `def`'s signature); everything else
        // (imports, `given`/`class`/…, plain statements) keeps its prior no-output behaviour.
        Repl.definitionKind(line) match
          case (t"val" | t"var", bound: Text) => valDefinition(line, bound, key, context)

          case (t"def", defName: Text) =>
            compile(contextImports(line), line)(Unset) match
              case ran: Outcome.Ran => ran.copy(output = t"${Repl.defSignature(line, defName, context)}\n")
              case other            => other

          case _ =>
            val statement = compile(contextImports(line), line)(Unset)

            // The line compiled as a statement; if it introduced imports, remember them so every
            // later line re-establishes them, and confirm each imported clause on its own line.
            statement match
              case ran: Outcome.Ran =>
                val introduced: List[Text] = importsIn(line)
                imports = (imports ::: introduced).distinct

                if introduced.isEmpty then ran else
                  val confirmed: Text =
                    introduced.map { each => t"Imported ${importClause(each)}" }.join(t"", t"\n", t"\n")

                  ran.copy(output = t"${ran.output}$confirmed")

              case other => other

      case ran: Outcome.Ran =>
        // An echoed identifier consumes no `resN` number — it introduced no new result.
        if !echo then result += 1

        // A bare EXPRESSION that evaluates to `Unit` shows only its side-effect output — no
        // `resN = () : scala.Unit` noise. A `val`/`var` DEFINITION of `Unit` type is handled by
        // `valDefinition`, which does NOT suppress, so its binding is still confirmed (`x = () :
        // scala.Unit`), matching the Scala REPL's `val x: Unit = ()`.
        val unit: Boolean = tpe.let(_ == t"scala.Unit").or(false) || ran.value.let(_ == t"()").or(false)

        if unit then ran.copy(value = Unset, name = Unset, tpe = Unset)
        else ran.copy(name = name, tpe = tpe)

      case other =>
        other

  // Displays a `val`/`var` definition like an auto-named expression: the definition is compiled (so
  // the binding persists in the session) with an appended inspection of its value, then shown as
  // `name = value : type`. A `lazy val` is NOT inspected — that would force it — so only its name and
  // type are shown. The type comes from typechecking `{ <definition>; <name> }` (never run).
  private def valDefinition(line: Text, bound: Text, key: Text, context: List[Text])
      (using Monitor, System, Probate)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    given LocalClasspath = classpath

    val tpe: Optional[Text] =
      safely(Repl.resultType(context, t"{ $line\n$bound }")).let(_.qualified)

    val lazyVal: Boolean =
      line.trim.cut(t" ").takeWhile { word => word != t"val" && word != t"var" }.contains(t"lazy")

    if lazyVal then
      compile(contextImports(line), line)(Unset) match
        case ran: Outcome.Ran => ran.copy(output = t"$bound${tpe.let { each => t": $each" }.or(t"")}\n")
        case other            => other
    else
      val inspected: Text = (line :: renderInto(bound, key)).join(t"\n")

      compile(contextImports(line), inspected):
        Optional(ReplBridge.fetch[String](session, key.s)).let(_.tt)
      . match
          case ran: Outcome.Ran => ran.copy(name = bound, tpe = tpe)
          case other            => other

  // The prelude's pickled definitions and binding accessors are recompiled once,
  // as the first object (`rs$line$0`), straight from TASTy — preserving the
  // original types rather than re-rendering them as source. Later lines see its
  // members via the history import. Imports create no members, so they are
  // re-injected into every line instead.
  private def ensureSeeded()(using Monitor, System, Probate)
  :   Optional[Outcome] logs CompileEvent raises CompilerError raises AsyncError =

    if seeded || prelude.seedTasty.isEmpty then Unset
    else
      seeded = true
      val name:   Text       = layout.objectName(index)

      val errors: List[Text] =
        ReplModuleCompiler.compile(classpath)(name, out.encode)(prelude.seedTasty)

      if errors.isEmpty then
        index += 1
        history = history :+ name
        Outcome.Ran(Nil, Unset, t"")
      else
        Outcome.Rejected(errors.map(Notice(Importance.Error, t"<seed>", _, Unset)))

  // The tokens of an import statement after the `import` keyword — the form the user sees and
  // names when removing it. `importKey` canonicalises for matching (whitespace ignored).
  private def importClause(statement: Text): Text =
    val trimmed = statement.trim
    if trimmed.starts(t"import ") then trimmed.skip(t"import ".length).trim else trimmed

  private def importKey(text: Text): Text = importClause(text).s.filterNot(_.isWhitespace).tt

  // `/unimport <tokens>` removes an earlier persistent import, named by the same tokens it was
  // imported with (e.g. `import soundness.*` → `/unimport soundness.*`); with no argument it
  // lists the removable imports. Only the user's own imports are removable, not the prelude's.
  private def unimport(line: Text): Outcome =
    val arg: Text = line.trim.skip(t"/unimport".length).trim

    if arg == t"" then
      if imports.isEmpty then Outcome.Ran(Nil, Unset, t"flame: no imports to remove\n")
      else
        val listing = imports.map { each => t"  ${importClause(each)}" }.join(t"\n")
        Outcome.Ran(Nil, Unset, t"flame: imports in scope (remove with /unimport <tokens>):\n$listing\n")
    else
      val (removed, kept) = imports.partition { each => importKey(each) == importKey(arg) }
      if removed.isEmpty then Outcome.Ran(Nil, Unset, t"flame: no matching import to remove: $arg\n")
      else
        imports = kept
        Outcome.Ran(Nil, Unset, t"flame: removed import: ${importClause(arg)}\n")

  // The `on`/`off` word of a `/set`/`/language <name> [on|off]` line — defaulting to on when omitted.
  private def enabledBy(rest: List[Text]): Boolean = rest match
    case value :: _ => !(value.lower == t"off" || value.lower == t"false")
    case Nil        => true

  // `/set <name> [on|off]` toggles a compiler setting for every subsequent line (no argument = on);
  // `/set` with no name lists the compiler settings (plus `async`) and their state. `async` is a plain
  // session toggle, not a compiler flag, so it is handled here rather than in the shared `toggle`.
  private def set(line: Text): Outcome = line.cut(t" ").filter(_ != t"") match
    case _ :: t"async" :: rest =>
      asyncMode = enabledBy(rest)
      Outcome.Ran(Nil, Unset, t"flame: async ${if asyncMode then t"enabled" else t"disabled"}\n")

    case _ :: name :: rest => toggle(Kind.Set, t"setting", name, rest)

    case _ =>
      val asyncMark: Text = if asyncMode then t"on " else t"off"
      val lines: List[Text] =
        t"  [$asyncMark] async — evaluate submissions asynchronously (slow results arrive later)"
        :: settingLines(Kind.Set, experimentalOn = true)

      Outcome.Ran(Nil, Unset, t"flame: compiler settings (/set <name> [on|off]):\n${lines.join(t"\n")}\n")

  // `/language <name> [on|off]` toggles an `import language.*` feature; `/language` with no name lists
  // the plain features always and the experimental ones once `/set experimental` is on.
  private def language(line: Text): Outcome = line.cut(t" ").filter(_ != t"") match
    case _ :: name :: rest => toggle(Kind.Language, t"language feature", name, rest)

    case _ =>
      val expOn = enabledSettings.contains(t"experimental")
      val note: Text =
        if expOn then t"" else t"\n  (more become available after `/set experimental`)"

      Outcome.Ran
       ( Nil, Unset,
         t"flame: language features (/language <name> [on|off]):\n${settingLines(Kind.Language, expOn).join(t"\n")}$note\n" )

  // Toggles the setting of `kind` named `name`, gating experimental language features on `experimental`
  // being enabled; `label` names the category for the messages.
  private def toggle(kind: Kind, label: Text, name: Text, rest: List[Text]): Outcome =
    Repl.settings.find { setting => setting.kind == kind && setting.name == name } match
      case Some(setting) if setting.experimental && !enabledSettings.contains(t"experimental") =>
        Outcome.Ran(Nil, Unset, t"flame: $name is experimental — enable it first with `/set experimental`\n")

      case Some(setting) =>
        val enable: Boolean = enabledBy(rest)
        if enable then enabledSettings += name else enabledSettings -= name
        Outcome.Ran(Nil, Unset, t"flame: $name ${if enable then t"enabled" else t"disabled"}\n")

      case None =>
        val expOn = enabledSettings.contains(t"experimental")
        val known: Text =
          Repl.settings
           . filter { s => s.kind == kind && (!s.experimental || expOn) }.map(_.name).join(t", ")

        Outcome.Ran(Nil, Unset, t"flame: unknown $label: $name (known: $known)\n")

  // The `[on|off] name — description` lines for the settings of `kind` (experimental ones only when
  // `experimentalOn`), for the no-argument `/set` / `/language` listings.
  private def settingLines(kind: Kind, experimentalOn: Boolean): List[Text] =
    Repl.settings
     . filter { setting => setting.kind == kind && (!setting.experimental || experimentalOn) }
     . map: setting =>
         val mark: Text = if enabledSettings.contains(setting.name) then t"on " else t"off"
         t"  [$mark] ${setting.name} — ${setting.description}"

  // `/context` lists every import currently in scope — the prelude's baseline imports plus the
  // ones the user has added — so the session's namespace can be inspected without changing it.
  private def showContext: Outcome =
    val all = prelude.imports.map(_.tt) ::: imports
    if all.isEmpty then Outcome.Ran(Nil, Unset, t"flame: no imports in scope\n")
    else
      val listing = all.map { each => t"  import ${importClause(each)}" }.join(t"\n")
      Outcome.Ran(Nil, Unset, t"flame: imports in scope:\n$listing\n")

  def interpret(line: Text)(using Monitor, System, Probate)
  :   Outcome logs CompileEvent raises CompilerError raises AsyncError =

    // A submission can change the session scope (a new definition, import, or `/set`), so any
    // cached member lists may be stale; drop them.
    completionCache = Map()

    def lineOutcome: Outcome =
      if line.trim.starts(t"/unimport") then unimport(line.trim)
      else if line.trim.starts(t"/set") then set(line.trim)
      else if line.trim.starts(t"/language") then language(line.trim)
      else if line.trim == t"/context" then showContext
      else if line.trim.starts(t"/tasty") then tasty(line.trim)
      else if line.trim.starts(t"/bytecode") then bytecode(line.trim)
      else evaluate(line)

    ensureSeeded().lay(lineOutcome):
      case _: Outcome.Ran => lineOutcome
      case failure        => failure

  // Tab completions at character `offset` in `code`, from the typechecked engine, computed
  // under the compiler mutex (the shared compiler is not reentrant). Public so in-process
  // front-ends (the web server) can request completions directly, like `complete` does for
  // the socket protocol.
  def completionsAt(code: Text, offset: Int)(using Monitor, System, Probate)
  :   List[Repl.CompletionItem] logs CompileEvent =

    // `/unimport <tokens>` completes against the imports currently in scope, so the user can pick
    // the one to remove; `/tasty <expr>` and `/bytecode <code>` complete their ARGUMENT as ordinary
    // Scala (the command prefix is stripped and the offset shifted, so member/keyword completion
    // behaves as on a normal line — the client's token-based insertion keeps the command prefix);
    // other `/`-command lines complete against the engine's known commands, not the Scala compiler.
    val exprHead: Optional[Text] = List(t"/tasty ", t"/bytecode ").find(code.starts(_)).getOrElse(Unset)

    if code.trim.starts(t"/unimport") then
      val arg = code.trim.skip(t"/unimport".length).trim
      imports.filter { each => importClause(each).starts(arg) }.map: each =>
        Repl.CompletionItem(t"/unimport ${importClause(each)}", t"command", t"")

    // `/language <partial>` completes its argument against the session's available features — the plain
    // ones always, the experimental ones only once `experimental` is on (SESSION-aware, unlike the
    // static `slashCommands`). The partial is the token being typed after the `/language ` prefix.
    else if code.starts(t"/language ") then
      val partial = code.keep(offset).cut(t" ").last
      Repl.languageCompletions(partial, enabledSettings.contains(t"experimental"))

    else exprHead match
      case head: Text => scalaCompletions(code.skip(head.length), (offset - head.length).max(0))
      case _ =>
        if code.trim.starts(t"/") then Repl.slashCompletions(code.trim)
        else scalaCompletions(code, offset)

  // Ordinary Scala completions at `offset` in `code`: member selection (`expr.partial`), infix
  // (`expr partial`, plus `match`), or — at a bare position — the syntactic keywords merged with
  // the compiler's name/definition completions. Split out of `completionsAt` so `/tasty <expr>`
  // can reuse it on the stripped expression.
  private def scalaCompletions(code: Text, offset: Int)(using Monitor, System, Probate)
  :   List[Repl.CompletionItem] logs CompileEvent =

    given LocalClasspath = classpath

    // The scope a compiled line would see: the prelude's and the user's persistent imports,
    // plus the prior wrapper objects' members and givens (as `import rs$line$N.{given, *}`).
    val context: List[Text] =
      (prelude.imports.map(_.tt) ::: imports) ::: history.map { name => t"import $name.{given, *}" }

    // The full member list of `expr.`, compiled ONCE and cached under the base, so typing
    // further member characters (or backspacing) costs no recompilation. The base's type is
    // fixed within a line; the cache is cleared on the next submission (`interpret`). Shared
    // by member selection (`expr.partial`) and infix completion (`expr partial`).
    def members(base: Text): List[Repl.CompletionItem] =
      completionCache.get(base).getOrElse:
        val items = safely(Repl.complete(context, base, base.length)).or(Nil)
        completionCache = completionCache.updated(base, items)
        items

    Repl.memberBase(code, offset) match
      case (base: Text, prefix) =>
        mutex(members(base)).filter(_.name.starts(prefix))

      case _ =>
        Repl.infixBase(code, offset) match
          // A value followed by a space: offer the receiver's methods (usable infix) plus the
          // `match` keyword (only ever offered here, i.e. after a trailing space).
          case (base: Text, prefix) =>
            val matched  = mutex(members(base)).filter(_.name.starts(prefix))
            val matchKw  =
              if t"match".starts(prefix) then List(Repl.CompletionItem(t"match", t"keyword", t"")) else Nil

            matchKw ::: matched

          // Otherwise prepend the keywords valid at this position (the compiler offers none)
          // to its name/definition completions.
          case _ =>
            Repl.keywordCompletions(code, offset) ::: mutex(safely(Repl.complete(context, code, offset)).or(Nil))

  // Typecheck-highlights, compiles, and runs `code`, returning a `Reply` with the
  // highlighting, the result value, its rendered type, and any diagnostics. The whole
  // body holds `mutex`, so concurrent submits serialize and never drive the
  // non-reentrant compiler at once. Shared by the socket protocol (`submit`, which
  // encodes the reply) and by in-process front-ends (e.g. the web server), which render
  // the typed `Reply` directly.
  // `onOutput` (async mode) receives each chunk of the run's stdout as it appears, so a front-end can
  // stream it; the chunk is still captured and returned in the reply's `output` as usual.
  def react(id: Int, code: Text, onOutput: Text => Unit = _ => ())(using Monitor, System, Probate)
  :   Repl.Reply logs CompileEvent =

    given LocalClasspath = classpath

    mutex:
      outputSink = onOutput
      try react0(id, code) finally outputSink = _ => ()

  private def react0(id: Int, code: Text)(using Monitor, System, Probate, LocalClasspath)
  :   Repl.Reply logs CompileEvent =

      val tokens      = Repl.highlight(code)
      val unprocessed = Repl.Reply.Failed(id, t"the input could not be processed")

      safely(interpret(code)).lay(unprocessed):
        case Outcome.Ran(notices, value, output, name, tpe) =>
          // `evaluate` has already chosen the binding name/type and suppressed the value of a bare
          // `Unit` EXPRESSION (a `Unit` `val`/`var` definition keeps its binding, so it still shows).
          Repl.Reply.Ran(id, value, output, tpe, name, notices.map(_.message).join(t"; "), tokens)

        case Outcome.Rejected(notices) =>
          Repl.Reply.Rejected(id, notices.map(_.message).join(t"; "), tokens)

        case Outcome.Threw(_, error, output) =>
          // Render the exception's (trimmed) stack trace as a coloured teletype listing, rather than
          // just its `toString`, so a thrown error surfaces where it came from.
          Repl.Reply.Threw(id, output, StackTraceRender.render(error), tokens)

        case Outcome.Crashed(notices, _) =>
          Repl.Reply.Crashed(id, notices.map(_.message).join(t"; "), tokens)
