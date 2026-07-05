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
import java.net as jn
import java.nio.channels as jnc
import java.nio.file as jnf
import java.util.regex as jur

import scala.collection.concurrent.TrieMap
import scala.collection.mutable as scm
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
import gossamer.*
import harlequin.*
import hellenism.*
import inimitable.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import stratiform.*
import turbulence.*
import urticose.*
import vacuous.*

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

  // One syntax-highlighting token of the submitted line: its verbatim text, the
  // lowercased name of its Harlequin accent (a COLOUR category — `keyword`, `term`,
  // `typal`, `number`, …), the token's `role` (`binding`/`usage` for a term or type,
  // `Unset` otherwise — a styling policy may e.g. italicise bindings), and — where the
  // typechecker resolved one — the fully-qualified Scala type of the token. Sent to the
  // client to colourise/style; ANSI/CSS rendering is the front-end's concern.
  case class Token(text: Text, accent: Text, tpe: Optional[Text], role: Optional[Text] = Unset)

  // One tab-completion candidate: the `name` to insert, its `kind` (term, method,
  // type, …) and its rendered type `signature`. The Harlequin `Completion`'s
  // `Syntax` signature is rendered to text here so the reply serializes simply.
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

  // Splits `code` at the cursor into the member-selection base — everything up to and
  // including the `.` immediately before the partial member name — and that partial. A
  // `Unset` base means the cursor is not selecting a member (a first-token identifier, the
  // first segment of an import, …), so there is no fixed type to enumerate and cache against.
  def memberBase(code: Text, offset: Int): (Optional[Text], Text) =
    val s: String = code.s
    var start: Int = offset
    while start > 0 && (jl.Character.isLetterOrDigit(s.charAt(start - 1)) || s.charAt(start - 1) == '_')
    do start -= 1

    val prefix: Text = s.substring(start, offset).nn.tt
    if start > 0 && s.charAt(start - 1) == '.' then (s.substring(0, start).nn.tt, prefix)
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
    val s = text.s
    if s.length == 0 then false else
      var i = 0
      var result = true
      while i < s.length && result do
        val c = s.charAt(i)
        if jl.Character.isLetterOrDigit(c) || c == '_' || jl.Character.isWhitespace(c) then result = false
        i += 1

      result

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
    val s = before.s
    var i = s.length - 1
    var result = false
    var scanning = true
    while i >= 0 && scanning do
      val c = s.charAt(i)
      if c == '\n' then result = true
      if jl.Character.isWhitespace(c) then i -= 1 else scanning = false

    result

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
  // (member/name) completions, which never include keywords. Only offered off a member
  // selection is excluded (a `.` position yields none).
  def keywordCompletions(code: Text, offset: Int): List[CompletionItem] =
    val s = code.s
    var start = offset
    while start > 0 && (jl.Character.isLetterOrDigit(s.charAt(start - 1)) || s.charAt(start - 1) == '_')
    do start -= 1

    val prefix: Text = s.substring(start, offset).nn.tt
    val before: Text = s.substring(0, start).nn.tt

    keywordsAt(before).filter(_.starts(prefix)).map(CompletionItem(_, t"keyword", t""))

  // The infix-completion receiver: when the cursor is at `<value-expr> <space> <partial>` — a
  // value followed by whitespace, not a member selection — returns that value expression with a
  // synthetic trailing `.` (so it reuses the member-completion path) and the partial method
  // name. `Unset` when there is no value receiver (the token before the space is a keyword,
  // operator, comma, or open bracket, or a name/type/path in a definition/import position).
  def infixBase(code: Text, offset: Int): (Optional[Text], Text) =
    val s = code.s
    var start = offset
    while start > 0 && (jl.Character.isLetterOrDigit(s.charAt(start - 1)) || s.charAt(start - 1) == '_')
    do start -= 1

    val prefix: Text = s.substring(start, offset).nn.tt

    // Require whitespace immediately before the partial (the space between receiver and method).
    if start == 0 || !jl.Character.isWhitespace(s.charAt(start - 1)) then (Unset, prefix) else
      val before: Text = s.substring(0, start).nn.tt
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
          while end > 0 && jl.Character.isWhitespace(s.charAt(end - 1)) do end -= 1
          val trimmed: Text = s.substring(0, end).nn.tt
          val baseStart = expressionStart(trimmed)
          val base: Text = s.substring(baseStart, end).nn.tt

          val preceding: Text =
            tokenize(s.substring(0, baseStart).nn.tt)
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
      else if jl.Character.isLetterOrDigit(c) || c == '_' || c == '.' then i -= 1
      else { i += 1; scanning = false }

    if i < 0 then 0 else i

  // A compiler setting toggleable with `/set <name> [on|off]`: the user-facing `name`, the scalac
  // `flag` it adds to every subsequent line's compile, and a short `description`. Held as raw flag
  // text (rather than anthology's version-typed `scalacOptions` values) so flame can build
  // `Scalac.Option[version](flag)` directly for its `version`, exactly as it did for `-experimental`.
  case class CompilerSetting(name: Text, flag: Text, description: Text)

  // The settings `/set` recognises. Curated to flags that are boolean toggles (no argument) and
  // valid for the REPL's Scala version; `experimental` keeps its original meaning. Order is the
  // listing order for `/set` with no argument.
  val compilerSettings: List[CompilerSetting] =
    List
     ( CompilerSetting(t"experimental", t"-experimental",
         t"enable experimental language features and definitions"),
       CompilerSetting(t"explain", t"-explain",
         t"print a detailed explanation for each error"),
       CompilerSetting(t"capture-checking", t"-language:experimental.captureChecking",
         t"enable experimental capture checking"),
       CompilerSetting(t"safer-exceptions", t"-language:experimental.saferExceptions",
         t"enable the saferExceptions checked-exceptions feature"),
       CompilerSetting(t"explicit-nulls", t"-Yexplicit-nulls",
         t"treat reference types as non-nullable (Null is a separate type)"),
       CompilerSetting(t"deprecation", t"-deprecation",
         t"warn about uses of deprecated APIs"),
       CompilerSetting(t"feature", t"-feature",
         t"warn about uses of advanced language features that should be enabled explicitly"),
       CompilerSetting(t"new-syntax", t"-new-syntax",
         t"require the new `then`/`do`-free control-flow syntax") )

  // The `/`-commands the engine itself recognises, with help text. Front-ends offer these
  // as completions when a line begins with `/`; the CLI appends its own client-only
  // commands (`/disconnect`, `/quit`) to these. Every compiler setting contributes a
  // `/set <name>` entry so each is offered (and documented) in tab-completion.
  val slashCommands: List[(Text, Text)] =
    List(t"/context" -> t"show the imports currently in scope")
    ::: compilerSettings.map { setting => t"/set ${setting.name}" -> setting.description }
    ::: List
         ( t"/tasty"    -> t"show the rendered TASTy (typed AST) of an expression",
           t"/bytecode" -> t"show the JVM bytecode of an expression or definition",
           t"/unimport" -> t"remove an earlier import from scope (by the tokens it was imported with)" )

  // Completions for a `/`-command line: the recognised commands whose names extend `prefix`,
  // each rendered as a `command` candidate carrying its help text as the signature.
  def slashCompletions(prefix: Text): List[CompletionItem] =
    slashCommands.filter { (name, _) => name.starts(prefix) }.map: (name, help) =>
      CompletionItem(name, t"command", help)

  // The Scala type of an expression, read from a typechecked highlight of
  // `<context> val __result = <code>`: the binding's token carries the resolved type as a
  // `Syntax`. The context (the session's imports and prior wrapper objects) is prepended so
  // the type resolves even for expressions that use session definitions. Only meaningful for
  // expression lines (statements have no value).
  def resultType(context: List[Text], code: Text)(using Scalac[?], LocalClasspath): Optional[Syntax] =
    import highlighting.typecheckedScala
    val contextLines: Text = context.map { line => t"$line\n" }.join
    val tokens = Scala.highlight(t"${contextLines}val __result = $code").lines.to(List).flatten

    tokens.find(_.text == t"__result") match
      case Some(token) => token.meta.let(_.tpe)
      case None        => Unset

  // The simple base type name of a qualified type, lowercased at the first letter — e.g.
  // `scala.collection.immutable.List[scala.Int]` → `list`, `Foo is Addable by Bar` → `addable`.
  // `Unset` when the base does not begin with a letter or isn't a plain type name (function,
  // tuple, intersection, literal types), so the caller falls back to `resN`.
  def baseName(qualified: Text): Optional[Text] =
    val q: String = qualified.s.trim.nn
    if q.contains("=>") then Unset else
      // Soundness infix `X is Y [by Z]` names the trait `Y`; otherwise take the type
      // constructor (strip any `[…]` type arguments) and its last dotted segment.
      val core: String =
        if q.contains(" is ") then q.split(" is ", 2).nn(1).nn.trim.nn.split("[\\s\\[]", 2).nn(0).nn
        else q.split("\\[", 2).nn(0).nn.trim.nn

      val simple: String = core.substring(core.lastIndexOf(".") + 1).nn

      if simple.matches("[A-Za-z][A-Za-z0-9_]*").nn
      then (jl.Character.toLowerCase(simple.charAt(0)).toString + simple.substring(1).nn).tt
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
    identifier.matches("[A-Za-z_$][A-Za-z0-9_$]*").nn && !allKeywords.contains(identifier.tt)

  // A `val`/`var`/`def` line's kind and the name it binds, past any leading modifiers — so the REPL
  // can show a definition's name/value/signature the way it shows an auto-named expression's. `Unset`
  // for anything else (imports, `given`/`type`/`class`, pattern `val (a, b) = …`, symbolic operator
  // names, plain statements), which then keep their current no-output behaviour.
  private val definitionPattern: jur.Pattern =
    val modifiers = "private|protected|final|lazy|override|inline|transparent|implicit|sealed|abstract|open"
    jur.Pattern.compile(s"^(?:(?:$modifiers)\\s+)*(val|var|def)\\s+([A-Za-z_\\$$][A-Za-z0-9_\\$$]*)\\b.*",
        jur.Pattern.DOTALL).nn

  def definitionKind(line: Text): Optional[(Text, Text)] =
    val matcher = definitionPattern.matcher(line.trim.s).nn
    if matcher.matches then (matcher.group(1).nn.tt, matcher.group(2).nn.tt) else Unset

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
      val out:     scm.ListBuffer[Text] = scm.ListBuffer()
      val current: StringBuilder        = StringBuilder()
      var depth:   Int                  = 0
      text.s.foreach: char =>
        char match
          case '(' =>
            if depth > 0 then current.append(char)
            depth += 1

          case ')' =>
            depth -= 1
            if depth == 0 then out += current.toString.tt.also(current.setLength(0))
            else current.append(char)

          case _ =>
            if depth > 0 then current.append(char)

      out.to(List)

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

  val session: Long = ReplBridge.freshSession()

  // Fulfilled when a connected client sends a `Quit` request; a server host can
  // `attend` it to block until then and shut down cleanly.
  private val quit: Promise[Unit] = Promise()

  def awaitQuit(): Unit = quit.attend()

  // Serializes `interpret` across connections: the shared `Scalac` compiler is
  // not reentrant and the REPL's state is mutable.
  private val mutex: Mutex = Mutex()

  // Caches the full member list for each `expr.` base completed on the current line, so the
  // live suggestion and Tab completion filter it instead of recompiling per keystroke. The
  // type of `expr` is fixed within a line, so the cache holds until the next submission,
  // which clears it. Accessed under `mutex` (cache fill) or cleared on submit; `TrieMap`
  // keeps that safe without re-entering the mutex.
  private val completionCache: TrieMap[Text, List[Repl.CompletionItem]] = TrieMap()

  private var index:   Int        = 0
  private var result:  Int        = 0
  private var history: List[Text] = Nil
  private var seeded:  Boolean     = false

  // The user's persistent imports. An `import` statement creates no member, so — unlike
  // the `val`/`def`/`class` definitions that later lines see through the history imports
  // — it would vanish after its own line. We accumulate the imports each line introduces
  // and re-inject them into every subsequent line, exactly as the prelude's imports are.
  private var imports: List[Text] = Nil

  // The compiler settings the user has switched on with `/set <name>`; each contributes its scalac
  // flag to every later line's compile (see `effectiveScalac`). Holds setting NAMES (keys into
  // `Repl.compilerSettings`), not the flags themselves.
  private val enabledSettings: scm.Set[Text] = scm.Set()

  private val out: Path on Linux = unsafely(temporaryDirectory/Uuid())
  locally(jnf.Files.createDirectories(jnf.Path.of(out.encode.s)).nn)

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

  // Compiles `code` as the next wrapper object and, on success, loads it (which
  // runs its body). `rendered` is evaluated after a successful run to supply the
  // `Outcome.Ran` value — `Unset` for statements, or the inspected result for an
  // expression line.
  // The compiler to use for the next line: the session's `Scalac` plus the flag of every setting the
  // user has switched on (`/set <name>`). `Scalac.Option` is contravariant in its version, so an
  // option built for this `version` is a valid extra flag for it.
  private def effectiveScalac: Scalac[version] =
    val extra: List[Scalac.Option[version]] =
      Repl.compilerSettings
        .filter { setting => enabledSettings.contains(setting.name) }
        .map { setting => Scalac.Option[version](setting.flag) }

    if extra.isEmpty then scalac else Scalac(scalac.options ::: extra)

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
        val captured: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream()
        val stream:   ji.PrintStream           = ji.PrintStream(captured, true, "UTF-8")
        val previous: ji.PrintStream           = jl.System.out.nn
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
    val parts:   scm.ListBuffer[Text] = scm.ListBuffer()
    val current: StringBuilder        = StringBuilder()
    var depth:   Int                  = 0

    clauses.s.foreach:
      case ch @ ('{' | '[' | '(') => depth += 1; current.append(ch)
      case ch @ ('}' | ']' | ')') => depth -= 1; current.append(ch)
      case ','  if depth == 0     => parts += current.toString.tt.trim; current.clear()
      case ch                     => current.append(ch)

    parts += current.toString.tt.trim
    parts.to(List).filter(_ != t"")

  // The imports to prepend to a line's wrapper: the prelude's plus the user's accumulated
  // imports, minus any the line itself repeats (so re-importing the same path never makes
  // a duplicate import within one wrapper's scope).
  private def contextImports(line: Text): List[Text] =
    val repeated = importsIn(line)
    (prelude.imports.map(_.tt) ::: imports).filter { each => !repeated.contains(each) }

  // Wraps an expression line as `val resN = <line>`, then renders the bound value
  // through `Inspectable` and stashes the rendering in `ReplBridge`. The renderer
  // sits in an `@experimental` scope because `Inspectable` is `@experimental`, so
  // this compiles even when the contextual `Scalac` is not in experimental mode.
  // Imports are not baked in here — `compile` places them at file scope.
  // The two lines that render the value bound to `ref` and stash the rendering under `key` for the
  // outcome: an `@experimental` initializer (experimental mode is enabled just here, since spectacular
  // `inspect` is `@experimental`) whose body renders per the session's `render` mode — `Inspect` uses
  // `.inspect` (teletype/text, CLI), `Html` uses `flame.HtmlRender.render` (the typeclass cascade →
  // HTML, web). The rendering runs INSIDE the wrapper, where `ref`'s static type is known.
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

    // The result type (if this is a value-producing expression) drives the binding name: the
    // base type name, lowercased and made unique in scope (`list`, `list2`, …), falling back to
    // `resN` when no letter-initial type name is available.
    val syntax:  Optional[Syntax] = safely(Repl.resultType(context, line))
    val tpe:     Optional[Text]   = syntax.let(_.qualified)

    // A line that is just an existing identifier (it type-checks, so it resolves) is echoed under
    // that same name — no fresh `val`, no new numbered result. Otherwise the binding name is
    // derived from the result type, made unique in scope (`list`, `list2`, …).
    val echo: Boolean = !syntax.absent && Repl.isBareIdentifier(line)

    val name: Text =
      if echo then line.trim
      else syntax match
        case syntax: Syntax =>
          Repl.baseName(syntax.qualified) match
            case base: Text => Repl.freeName(base, context)
            case _          => t"res${result.toString.tt}"

        case _ => t"res${result.toString.tt}"

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
        ran.copy(name = name, tpe = tpe)

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

  // A `/set` line toggles a session setting rather than being compiled. Currently only
  // `/set experimental [on|off]` is recognised; it adds (or removes) `-experimental` from
  // the compiler options for every subsequent line.
  // The tokens of an import statement after the `import` keyword — the form the user sees and
  // names when removing it. `importKey` canonicalises for matching (whitespace ignored).
  private def importClause(statement: Text): Text =
    val trimmed = statement.trim
    if trimmed.starts(t"import ") then trimmed.skip(t"import ".length).trim else trimmed

  private def importKey(text: Text): Text = importClause(text).s.replaceAll("\\s+", "").nn.tt

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

  // `/set <name> [on|off]` toggles a compiler setting (see `Repl.compilerSettings`) for every
  // subsequent line — no argument means `on`. `/set` with no name lists the settings and their
  // current state; an unknown name reports the known ones.
  private def set(line: Text): Outcome =
    line.cut(t" ").filter(_ != t"") match
      case _ :: name :: rest =>
        Repl.compilerSettings.find(_.name == name) match
          case Some(setting) =>
            val enable: Boolean = rest match
              case value :: _ => !(value.lower == t"off" || value.lower == t"false")
              case Nil        => true

            if enable then enabledSettings.add(name) else enabledSettings.remove(name)
            val word: Text = if enable then t"enabled" else t"disabled"
            Outcome.Ran(Nil, Unset, t"flame: $name $word\n")

          case None =>
            val known: Text = Repl.compilerSettings.map(_.name).join(t", ")
            Outcome.Ran(Nil, Unset, t"flame: unknown setting: $name (known: $known)\n")

      case _ =>
        val listing: Text = Repl.compilerSettings.map: setting =>
          val mark: Text = if enabledSettings.contains(setting.name) then t"on " else t"off"
          t"  [$mark] ${setting.name} — ${setting.description}"
        . join(t"\n")

        Outcome.Ran(Nil, Unset, t"flame: compiler settings (/set <name> [on|off]):\n$listing\n")

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
    completionCache.clear()

    def lineOutcome: Outcome =
      if line.trim.starts(t"/unimport") then unimport(line.trim)
      else if line.trim.starts(t"/set") then set(line.trim)
      else if line.trim == t"/context" then showContext
      else if line.trim.starts(t"/tasty") then tasty(line.trim)
      else if line.trim.starts(t"/bytecode") then bytecode(line.trim)
      else evaluate(line)

    ensureSeeded().lay(lineOutcome):
      case _: Outcome.Ran => lineOutcome
      case failure        => failure

  // Starts a TCP server on `port` and accepts connections. Each connection is an
  // interactive session over the *same* REPL state. Messages are JSON `Request`/
  // `Reply` values, one per line, each terminated by a blank line (compact JSON
  // has no embedded newline, so the delimiter is unambiguous). Returns a handle
  // whose `stop()` shuts the server down.
  def serve(port: Port over Tcp)(using Monitor, System, Probate)
  :   SocketService logs CompileEvent raises BindError raises StreamError =

    port.listen: socket =>
      converse(socket.getInputStream.nn, socket.getOutputStream.nn)
      Data()

  // Serves the REPL over a UNIX domain socket at `socketPath` (used when no TCP
  // port is given). Coaxial's domain-socket `Connection` does not expose its
  // streams for the bidirectional, asynchronously-written protocol this server
  // needs, so the accept loop runs directly over an NIO channel.
  def serve(socketPath: Text)(using Monitor, System, Probate): SocketService logs CompileEvent =
    val address: jn.UnixDomainSocketAddress = jn.UnixDomainSocketAddress.of(socketPath.s).nn

    val channel: jnc.ServerSocketChannel =
      jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn

    channel.configureBlocking(true)
    channel.bind(address)

    @volatile var listening: Boolean = true

    val task = async:
      while listening do
        safely:
          val client: jnc.SocketChannel = channel.accept().nn
          val input  = jnc.Channels.newInputStream(client).nn
          val output = jnc.Channels.newOutputStream(client).nn

          async:
            try converse(input, output) finally safely(client.close())

    new SocketService:
      def stop(): Unit =
        listening = false
        safely(channel.close())
        safely(task.await())

  private def converse(input: ji.InputStream, output: ji.OutputStream)
    ( using Monitor, System, Probate )
  :   Unit logs CompileEvent =

    // The TCP caller's `listen` owns the accepted socket — it writes this lambda's
    // result to the socket after we return — so we must NOT close it here, or
    // that write fails. And any I/O error (typically the client disconnecting)
    // must not escape: it would propagate out of the accept loop and stop the
    // server from accepting any further connections.
    //
    // Each message is handled in its own task, so a slow `submit` doesn't hold up
    // the `tokenize` replies a client fires while editing: `tokenize` is stateless
    // (runs concurrently), `submit` is serialized by `mutex`, and replies may go
    // back out of order. A write mutex stops concurrent replies interleaving.
    val writes: Mutex = Mutex()

    // Each message is a 4-byte big-endian length prefix followed by that many BinTEL
    // body bytes (binary, so no textual delimiter is safe). `readInt` raises at EOF.
    try
      val in: ji.DataInputStream  = ji.DataInputStream(ji.BufferedInputStream(input))
      val out: ji.DataOutputStream = ji.DataOutputStream(ji.BufferedOutputStream(output))
      var continue: Boolean = true

      while continue do
        val length: Int = try in.readInt() catch case _: ji.IOException => -1

        if length < 0 then continue = false
        else
          val bytes: Array[Byte] = new Array[Byte](length)
          in.readFully(bytes)
          val message: Data = bytes.immutable(using Unsafe)

          async:
            // A stray throwable in one message becomes an error reply, not a
            // dropped connection, so the session survives. `Unset` (a `quit`) is
            // not answered.
            val response: Optional[Data] =
              try respond(message)
              catch case error: Throwable =>
                encode(Repl.Reply.Failed(0, error.toString.tt))

            response.let: payload =>
              writes:
                try
                  out.writeInt(payload.length)
                  out.write(payload.mutable(using Unsafe))
                  out.flush()
                catch case _: Throwable => ()

    catch case _: Throwable => ()

  // Decodes one JSON `Request` and dispatches: a `tokenize` only highlights (cheap,
  // for live editing); a `submit` compiles and runs; a `quit` signals the server to
  // shut down and is not answered. A malformed request becomes a `Failed` reply
  // rather than a dropped connection. `Unset` means no reply is sent.
  private def respond(message: Data)(using Monitor, System, Probate)
  :   Optional[Data] logs CompileEvent =

    val request: Optional[Repl.Request] =
      safely[Exception](Bintel.read[Repl.Request](message))

    request.lay(encode(Repl.Reply.Failed(0, t"the request could not be parsed"))):
      case Repl.Request.Tokenize(id, code)         => tokenized(id, code)
      case Repl.Request.Submit(id, code)           => submit(id, code)
      case Repl.Request.Complete(id, code, offset) => complete(id, code, offset)
      case Repl.Request.Quit(_)                    => quit.offer(()) yet Unset

  private def tokenized(id: Int, code: Text): Data =
    encode(Repl.Reply.Tokenized(id, Repl.tokenize(code), Repl.incomplete(code)))

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
      completionCache.getOrElseUpdate(base, safely(Repl.complete(context, base, base.length)).or(Nil))

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

  // Computes tab completions at `offset` in `code` and replies (with `id`).
  private def complete(id: Int, code: Text, offset: Int)(using Monitor, System, Probate)
  :   Data logs CompileEvent =
    encode(Repl.Reply.Completed(id, completionsAt(code, offset)))

  // Typecheck-highlights, compiles, and runs `code`, replying (with `id`) with the
  // highlighting, the result value, and any diagnostics. The whole body holds
  // `mutex`, so concurrent submits serialize and never drive the compiler at once.
  // Typecheck-highlights, compiles, and runs `code`, returning a `Reply` with the
  // highlighting, the result value, its rendered type, and any diagnostics. The whole
  // body holds `mutex`, so concurrent submits serialize and never drive the
  // non-reentrant compiler at once. Shared by the socket protocol (`submit`, which
  // encodes the reply) and by in-process front-ends (e.g. the web server), which render
  // the typed `Reply` directly.
  def react(id: Int, code: Text)(using Monitor, System, Probate): Repl.Reply logs CompileEvent =
    given LocalClasspath = classpath

    mutex:
      val tokens      = Repl.highlight(code)
      val unprocessed = Repl.Reply.Failed(id, t"the input could not be processed")

      safely(interpret(code)).lay(unprocessed):
        case Outcome.Ran(notices, value, output, name, tpe) =>
          // The binding name and rendered type were chosen by `evaluate` (in the session scope).
          // A `Unit` result carries nothing worth showing, so drop the value, type and name:
          // neither front-end then displays `x = () : scala.Unit` (statements and imports have no
          // value). Side effects still surface through `output`.
          val unit: Boolean = tpe.let(_ == t"scala.Unit").or(false) || value.let(_ == t"()").or(false)

          if unit
          then Repl.Reply.Ran(id, Unset, output, Unset, Unset, notices.map(_.message).join(t"; "), tokens)
          else Repl.Reply.Ran(id, value, output, tpe, name, notices.map(_.message).join(t"; "), tokens)

        case Outcome.Rejected(notices) =>
          Repl.Reply.Rejected(id, notices.map(_.message).join(t"; "), tokens)

        case Outcome.Threw(_, error, output) =>
          // Render the exception's (trimmed) stack trace as a coloured teletype listing, rather than
          // just its `toString`, so a thrown error surfaces where it came from.
          Repl.Reply.Threw(id, output, StackTraceRender.render(error), tokens)

        case Outcome.Crashed(notices, _) =>
          Repl.Reply.Crashed(id, notices.map(_.message).join(t"; "), tokens)

  private def submit(id: Int, code: Text)(using Monitor, System, Probate): Data logs CompileEvent =
    encode(react(id, code))

  // Serializes a `Reply` to BinTEL body bytes, deriving the schema from the type
  // (`value.bintel`). A valid reply always type-assigns, so the encode is total.
  private def encode(reply: Repl.Reply): Data = unsafely(reply.bintel)
