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
┃    Flame, version 0.1.1.                                                                         ┃
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
import pyrocosm.{Block, Tone}
import pyrocosm.{exhibit, sourceCodePresentable, flattened}

// Renders compiler notices for a REPL reply, re-rendering the TYPES embedded in each error message
// through stenography and syntax-highlighting both them and any embedded CODE SAMPLES with harlequin.
// Under `-Zsemantic-diagnostics` the compiler wraps every interpolated type of a message in in-band
// markers carrying its pickled TASTy, and interpolated trees as `code` nodes; `delicious` parses that
// into a `Markup` tree and its `Reifier` turns each type marker's TASTy back into a
// `stenography.Syntax`. For `Inspect` the rendering is `delicious.ansi`'s `teletype`, harlequin-
// highlighted code and types as coloured ANSI; for `Exhibit` the same tree becomes Pyrocosm
// blocks (`blocks`). Both fall back to the compiler's own printed text for anything that cannot
// be reified, and to the plain message when there is no semantic markup at all.
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
    def background:       Color in Srgb = hex(FlameColours.background)
    def foreground:       Color in Srgb = hex(FlameColours.foreground)
    def scalaError:       Color in Srgb = hex(FlameColours.error)
    def scalaNumber:      Color in Srgb = hex(FlameColours.number)
    def scalaString:      Color in Srgb = hex(FlameColours.string)
    def scalaTerm:        Color in Srgb = hex(FlameColours.term)
    def scalaType:        Color in Srgb = hex(FlameColours.tpe)
    def scalaKeyword:     Color in Srgb = hex(FlameColours.keyword)
    def scalaSymbol:      Color in Srgb = hex(FlameColours.symbol)
    def scalaParenthesis: Color in Srgb = hex(FlameColours.operator)
    def scalaModifier:    Color in Srgb = hex(FlameColours.keyword)
    def scalaComment:     Color in Srgb = hex(FlameColours.comment)
    def subdued:          Color in Srgb = hex(FlameColours.subdued)
    def accented:         Color in Srgb = hex(FlameColours.foreground)
    def margin:           Color in Srgb = hex(FlameColours.margin)


  // Renders `notices` to the diagnostics string. `reifier` reifies the TASTy of each type marker
  // (`Unset` — no marker present, or the reifier could not be built — falls back to compiler text).
  def render(notices: List[Notice], reifier: Optional[Reifier])(using Imports): Text =
    notices.map { (notice: Notice) => renderNotice(notice, reifier) }.join(t"; ")

  // The column at which a diagnostic's PROSE word-wraps in the terminal rendering. Neither the daemon
  // nor basic mode knows the client terminal's true width, so this follows the compiler's own
  // page-width convention. Embedded code samples are never wrapped; a single word (or type) longer
  // than the width is emitted whole on its own line.
  private val wrapWidth: Int = 80

  private def renderNotice(notice: Notice, reifier: Optional[Reifier])(using Imports): Text =
    notice.semantic.lay(wrapPlain(notice.message)): message =>
      ansiMessage(message, reifier).render(xtermTrueColorTermcap)

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

    List.from(pieces.result())

  // Word-wraps a PLAIN (no semantic markup) message for the terminal, through the same flow — no
  // styling, so the wrapped Teletype's plain text is returned directly.
  private def wrapPlain(text: Text): Text = flow(prose(text)).plain

  // The stenography rendering of a type marker (Unset reifier, or a failed reification, falls back to
  // the compiler-printed text the marker also carries in `plain`).
  private def typeText(typed: Markup.Typed, reifier: Optional[Reifier])(using Imports): Text =
    reifier.lay(Unset: Optional[Syntax])(_.syntax(typed)).let(_.text).or(typed.plain)

  // The notices as Pyrocosm blocks: one notice per `Notice`, its message as paragraphs (one
  // per line of the message, so the compiler's `Found:`/`Required:` lines keep their own),
  // types highlighted as Scala types after stenography's abbreviation, code samples as Scala
  // terms, a multi-line sample as a code block of its own.
  def blocks(notices: List[Notice], reifier: Optional[Reifier])(using Imports): List[Block] =
    notices.map { (notice: Notice) => noticeBlock(notice, reifier) }

  private def noticeBlock(notice: Notice, reifier: Optional[Reifier])(using Imports): Block =
    val tone: Tone = notice.importance match
      case Importance.Error   => Tone.Failure
      case Importance.Warning => Tone.Warning
      case _                  => Tone.Info

    // The message is shown trimmed of the whitespace around it: a compiler message often ends
    // in a newline, which would stand as a blank line of the notice.
    val content: List[Block] = notice.semantic.lay(paragraphs(stripAnsi(notice.message).trim)): message =>
      markupBlocks(message.markup, reifier)

    Block.Notice(tone, Unset, content)

  // A message's lines stay together in one paragraph, broken where the message breaks.
  private def paragraphs(text: Text): List[Block] =
    val lines: scala.List[Text] = text.cut(t"\n").stdlib
    val content: scala.List[pyrocosm.Inline] = lines.zipWithIndex.flatMap { (line, index) =>
      (if index == 0 then scala.Nil else scala.List(pyrocosm.Inline.Break())) ::: lineInlines(line) }
    List(Block.Paragraph(List.from(content)))

  // One line of a message as phrasing: a line of a missing-given diagnostic's tree (frontier's
  // `■ resolving Foo`, `└─ ▸ propose bar`, and so on) has its mark toned, its keyword kept and
  // its name highlighted as the type or value it is; any other line is text.
  private val treeLine = "^([\\s│├└─]*)([■✓✗▪▸])\\s+(resolving|found|requires|candidate|propose)\\s+(\\S.*?)(\\s+\\(.*\\))?$".r

  private def lineInlines(line: Text): scala.List[pyrocosm.Inline] = line.s match
    case treeLine(prefix0, mark0, keyword0, name0, label) =>
      val prefix = prefix0.nn
      val mark = mark0.nn
      val keyword = keyword0.nn
      val name = name0.nn

      val tone: Tone = mark match
        case "■" => Tone.Accent
        case "✓" => Tone.Success
        case "✗" => Tone.Failure
        case "▪" => Tone.Warning
        case _   => Tone.Info

      val context = if keyword == "propose" then Scala.Context.Term else Scala.Context.Type
      val labelled: scala.List[pyrocosm.Inline] =
        if label == null then scala.Nil else scala.List(pyrocosm.Inline.Toned(Tone.Muted, pyrocosm.Inline.text(label.nn.tt)))

      scala.List(pyrocosm.Inline.Toned(Tone.Muted, pyrocosm.Inline.text(prefix.tt)),
        pyrocosm.Inline.Toned(tone, pyrocosm.Inline.text(mark.tt)),
        pyrocosm.Inline.Textual(t" $keyword "),
        highlighted(name.tt, context)) ::: labelled

    case _ => scala.List(pyrocosm.Inline.Textual(line))

  private def highlighted(text: Text, context: Scala.Context): pyrocosm.Inline =
    pyrocosm.Inline.Code(pyrocosm.Language.Scala, Scala.highlight(stripAnsi(text), context).flattened)

  // Runs of phrasing become one paragraph, broken at the message's own newlines; a code sample
  // with a newline in it becomes a code block between paragraphs.
  private def markupBlocks(nodes: List[Markup], reifier: Optional[Reifier])(using Imports): List[Block] =
    val blocks = sci.List.newBuilder[Block]
    var run: sci.List[pyrocosm.Inline] = sci.Nil

    // A paragraph is trimmed of the breaks and blank text at either end.
    def blank(node: pyrocosm.Inline): Boolean = node match
      case pyrocosm.Inline.Break()      => true
      case pyrocosm.Inline.Textual(text) => text.trim == t""
      case _                            => false

    def flush(): Unit =
      val trimmed: sci.List[pyrocosm.Inline] = run.dropWhile(blank).reverse.dropWhile(blank)
      if trimmed.nonEmpty then blocks += Block.Paragraph(List.from(trimmed))
      run = sci.Nil

    def phrase(markup: Markup): Unit = markup match
      case Markup.Textual(text) =>
        val lines = stripAnsi(text).cut(t"\n").stdlib
        lines.zipWithIndex.foreach: (line, index) =>
          if index > 0 then run = pyrocosm.Inline.Break() :: run
          if line != t"" then run = lineInlines(line).reverse ::: run

      case Markup.Code(_, _) =>
        if markup.plain.contains(t"\n") then
          flush()
          blocks += Scala.highlight(stripAnsi(markup.plain), Scala.Context.Term).exhibit
        else run = highlighted(markup.plain, Scala.Context.Term) :: run

      case typed: Markup.Typed                => run = highlighted(typeText(typed, reifier), Scala.Context.Type) :: run
      case Markup.Symbolic(_, _, _, children) => children.each(phrase)
      case Markup.Named(_, _, children)       => children.each(phrase)
      case Markup.Spanned(_, _, children)     => children.each(phrase)

    nodes.each(phrase)
    flush()
    List.from(blocks.result())

  def stripAnsi(text: Text): Text =
    text.s.replaceAll("\\e?\\[[0-9;]*m", "").nn.tt

