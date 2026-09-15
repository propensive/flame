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

import anticipation.*
import denominative.*
import denominative.dysasymptotics.linearSize
import symbolism.*
import anthology.*
import escapade.*, termcapDefinitions.xtermTrueColorTermcap
import gossamer.*
import harlequin.*
import iridescence.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import stenography.*
import tessellate.Flow
import vacuous.*

import delicious.*  // Markup, SemanticMessage, Reifier, `semantic` on Notice, `teletype` on SemanticMessage
import pyrocosm.{Block, Tone}
import pyrocosm.{exhibit, sourceCodePresentable, flattened}
import FlameColours.srgb

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
  // The syntax palette, matched to the user's Zed theme "Propensive" — the single source of truth,
  // shared with the CLI front-end (whose editor `given` delegates here), so a type or code sample in
  // an error message is coloured exactly as it would be in the editor. (See the palette notes in
  // `flame_client.scala`: an accent is a colour category; `scalaSymbol`/`scalaParenthesis` map to
  // Zed's punctuation.bracket/operator respectively.)
  val palette: ScalaSyntaxPalette = new Palette:
    type Form = Srgb
    def background:       Color in Srgb = srgb(FlameColours.background)
    def foreground:       Color in Srgb = srgb(FlameColours.foreground)
    def scalaError:       Color in Srgb = srgb(FlameColours.error)
    def scalaNumber:      Color in Srgb = srgb(FlameColours.number)
    def scalaString:      Color in Srgb = srgb(FlameColours.string)
    def scalaTerm:        Color in Srgb = srgb(FlameColours.term)
    def scalaType:        Color in Srgb = srgb(FlameColours.tpe)
    def scalaKeyword:     Color in Srgb = srgb(FlameColours.keyword)
    def scalaSymbol:      Color in Srgb = srgb(FlameColours.symbol)
    def scalaParenthesis: Color in Srgb = srgb(FlameColours.operator)
    def scalaModifier:    Color in Srgb = srgb(FlameColours.keyword)
    def scalaComment:     Color in Srgb = srgb(FlameColours.comment)
    def subdued:          Color in Srgb = srgb(FlameColours.subdued)
    def accented:         Color in Srgb = srgb(FlameColours.foreground)
    def margin:           Color in Srgb = srgb(FlameColours.margin)


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
        column = plain.pinpoint(_ == '\n', bidi = Rtl).lay(column + pending.length + plain.length):
          newline => plain.length - newline.n0 - 1
        pending = t""

    out

  // Splits plain prose into `Piece`s: explicit breaks, runs of spaces/tabs, and words — preserving
  // every character, so unwrapped text round-trips exactly.
  private def prose(text: Text): List[Piece] =
    val plain: Text = stripAnsi(text)

    def blank(char: Char): Boolean = char == ' ' || char == '\t'

    // Each step consumes one break, one run of spaces/tabs, or one word, and the accumulator is
    // built in reverse and turned round at the end — the immutable equivalent of appending to a
    // builder, and the same single pass.
    def recur(rest: Text, pieces: List[Piece]): List[Piece] =
      rest.prim.lay(pieces.reverse): char =>
        if char == '\n' then recur(rest.skip(1), Piece.Break :: pieces)
        else if blank(char) then
          val space: Text = rest.keep(blank)
          recur(rest.skip(space.length), Piece.Space(space) :: pieces)
        else
          val word: Text = rest.keep { char => char != '\n' && !blank(char) }
          recur(rest.skip(word.length), Piece.Word(e"$word", word.length) :: pieces)

    recur(plain, Nil)

  // Word-wraps a PLAIN (no semantic markup) message for the terminal. With no code samples to hold
  // together there is nothing `flow` protects, so this is tessellate's own wrap; `flow` survives
  // only for the styled path, until `Flow` learns about unbreakable spans (Soundness #1992).
  private def wrapPlain(text: Text): Text =
    import hieroglyph.textMetrics.uniformMetric
    Flow.wrap(text, wrapWidth).join(t"\n")

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
    // The first line's phrasing stands alone; every later line is preceded by the break it
    // follows, so the message's own line structure survives into the paragraph.
    val content: List[pyrocosm.Inline] =
      text.lines.map(lineInlines).fuse(Nil: List[pyrocosm.Inline]):
        if state.nil then next else state + List(pyrocosm.Inline.Break()) + next

    List(Block.Paragraph(content))

  // One line of a message as phrasing: a line of a missing-given diagnostic's tree (frontier's
  // `■ resolving Foo`, `└─ ▸ propose bar`, and so on) has its mark toned, its keyword kept and
  // its name highlighted as the type or value it is; any other line is text.
  private def lineInlines(line: Text): List[pyrocosm.Inline] = line match
    case r"$prefix([\s│├└─]*)$mark([■✓✗▪▸])\s+$keyword(resolving|found|requires|candidate|propose)\s+$name(\S.*?)$label(\s+\(.*\))?" =>
      val tone: Tone = mark match
        case t"■" => Tone.Accent
        case t"✓" => Tone.Success
        case t"✗" => Tone.Failure
        case t"▪" => Tone.Warning
        case _    => Tone.Info

      val context = if keyword == t"propose" then Scala.Context.Term else Scala.Context.Type

      val labelled: List[pyrocosm.Inline] =
        label.lay(Nil): label =>
          List(pyrocosm.Inline.Toned(Tone.Muted, pyrocosm.Inline.text(label)))

      List(pyrocosm.Inline.Toned(Tone.Muted, pyrocosm.Inline.text(prefix)),
        pyrocosm.Inline.Toned(tone, pyrocosm.Inline.text(mark)),
        pyrocosm.Inline.Textual(t" $keyword "),
        highlighted(name, context)) + labelled

    case _ => List(pyrocosm.Inline.Textual(line))

  private def highlighted(text: Text, context: Scala.Context): pyrocosm.Inline =
    pyrocosm.Inline.Code(pyrocosm.Language.Scala, Scala.highlight(stripAnsi(text), context).flattened)

  // Runs of phrasing become one paragraph, broken at the message's own newlines; a code sample
  // with a newline in it becomes a code block between paragraphs.
  private def markupBlocks(nodes: List[Markup], reifier: Optional[Reifier])(using Imports): List[Block] =
    // Both accumulators are built in reverse and turned round when they are drained: `run` is the
    // phrasing of the paragraph being assembled, `blocks` the blocks completed so far.
    var blocks: List[Block] = Nil
    var run: List[pyrocosm.Inline] = Nil

    // A paragraph is trimmed of the breaks and blank text at either end.
    def blank(node: pyrocosm.Inline): Boolean = node match
      case pyrocosm.Inline.Break()       => true
      case pyrocosm.Inline.Textual(text) => text.blank
      case _                             => false

    def flush(): Unit =
      val trimmed: List[pyrocosm.Inline] = run.skip(blank).reverse.skip(blank)
      if !trimmed.nil then blocks = Block.Paragraph(trimmed) :: blocks
      run = Nil

    def phrase(markup: Markup): Unit = markup match
      case Markup.Textual(text) =>
        stripAnsi(text).lines.each: ordinal ?=>
          line =>
            if ordinal != Prim then run = pyrocosm.Inline.Break() :: run
            if line != t"" then run = lineInlines(line).reverse + run

      case Markup.Code(_, _) =>
        if markup.plain.contains(t"\n") then
          flush()
          blocks = Scala.highlight(stripAnsi(markup.plain), Scala.Context.Term).exhibit :: blocks
        else run = highlighted(markup.plain, Scala.Context.Term) :: run

      case typed: Markup.Typed                => run = highlighted(typeText(typed, reifier), Scala.Context.Type) :: run
      case Markup.Symbolic(_, _, _, children) => children.each(phrase)
      case Markup.Named(_, _, children)       => children.each(phrase)
      case Markup.Spanned(_, _, children)     => children.each(phrase)

    nodes.each(phrase)
    flush()
    blocks.reverse

  def stripAnsi(text: Text): Text = text.unstyled

