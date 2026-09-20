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
┃    Flame, version 0.2.0.                                                                         ┃
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
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import digression.*
import gossamer.*
import rudiments.*
import spectacular.*
import stratiform.*
import symbolism.*
import turbulence.*
import vacuous.*

import hieroglyph.charEncoders.utf8Encoder

import pyrocosm.{Block, Inline, Language, Token, Tone}
import pyrocosm.exhibit

// The REPL's replies as Pyrocosm's semantic blocks: tokens with their marks as code with
// notes, captured output by stream, the result line, a stack trace. What a reply *is*; how it
// looks belongs to the frontends.
object Blocks:
  // The model's accents are harlequin's, which the wire's accent names spell in lower case.
  def token(token: Repl.Token): Token =
    val accent: Token.Accent = token.accent.s match
      case "keyword"  => Token.Accent.Keyword
      case "modifier" => Token.Accent.Modifier
      case "typal"    => Token.Accent.Typal
      case "string"   => Token.Accent.String
      case "number"   => Token.Accent.Number
      case "symbol"   => Token.Accent.Symbol
      case "parens"   => Token.Accent.Parens
      case "error"    => Token.Accent.Error
      case "unparsed" => Token.Accent.Unparsed
      case "command"  => Token.Accent.Command
      case _          => Token.Accent.Term

    val role: Optional[Token.Role] = token.role.let: (role: Text) =>
      if role == t"binding" then Token.Role.Binding else Token.Role.Usage

    Token(token.text, accent, role)

  // A `/`-command line: the command word, then its parameters.
  def command(line: Text): List[Repl.Token] =
    line.where(_ == ' ').lay(List(Repl.Token(line, t"command", Unset))): space =>
      List(Repl.Token(line.keep(space.n0), t"command", Unset),
           Repl.Token(line.skip(space.n0), t"unparsed", Unset))

  // The tokens as a code block: lines split at the newline tokens `Repl.project` inserts, and
  // every marked token a note on its line (an error span erroneous, a warning cautionary).
  def code(tokens: List[Repl.Token]): Block.Code =
    // Three accumulators built in reverse and turned round as they are drained: the completed
    // lines, the notes gathered across all of them, and the tokens of the line in hand.
    var lines: List[Block.Line] = Nil
    var notes: List[Block.Note] = Nil
    var current: List[Token] = Nil
    var line: Int = 0
    var column: Int = 0

    def flush(): Unit =
      lines = Block.Line(current.reverse) :: lines
      current = Nil

    tokens.each: (token: Repl.Token) =>
      token.text.lines.each: ordinal ?=>
        part =>
          if ordinal != Prim then
            flush()
            line += 1
            column = 0

          if part != t"" then
            current = Blocks.token(token.copy(text = part)) :: current

            token.mark.let: (mark: Text) =>
              val style = if mark == Repl.warningMark then Block.Note.Style.Caution else Block.Note.Style.Erroneous
              notes = Block.Note(line, column, column + part.length, style) :: notes

            column += part.length

    flush()
    Block.Code(Language.Scala, lines.reverse, notes.reverse)

  // The code shifted right by `columns`: a run of spaces before every line, and its notes moved
  // with it. A transcript entry keeps the column its line was typed at, past the prompt marker.
  def indented(code: Block.Code, columns: Int): Block.Code =
    val space: Token = Token.plain(t" "*columns)
    val lines: List[Block.Line] = code.lines.map { (line: Block.Line) => Block.Line(space :: line.tokens) }

    val notes: List[Block.Note] = code.notes.map: (note: Block.Note) =>
      note.copy(start = note.start + columns, end = note.end + columns)

    Block.Code(code.language, lines, notes)

  // An import's confirmation, `imported foo.*`: the word as an information message, the clause
  // as code, behind a neutral band.
  def imported(clause: Text): Block =
    val word: Inline = Inline.Emphasis(List(Inline.Toned(Tone.Info, Inline.text(t"imported"))))
    val code: Inline = Inline.Code(Language.Scala, Repl.tokenize(clause).map(token))
    Block.Notice(Tone.Muted, Unset, List(Block.Paragraph(List(word, Inline.Textual(t" "), code))))

  // The marks alone, as notes by line, for a field's decoration.
  def marks(tokens: List[Repl.Token]): List[Block.Note] = code(tokens).notes

  // Captured output as blocks: each span of standard output or error verbatim behind its
  // gutter; whatever lies between the spans (the engine's own messages) as preformatted text,
  // stripped of ANSI when the medium cannot show it.
  def output(plain: Text, spans: List[Repl.OutputSpan], ansi: Boolean): List[Block] =
    var blocks: List[Block] = Nil
    var at: Int = 0

    // A message ends in a newline, which is not a line of its own. An import's confirmation is
    // a notice of its own; the lines between are preformatted text.
    def gap(text: Text): Unit =
      val shown: Text = if ansi then text else SemanticRender.stripAnsi(text)

      if shown.trim != t"" then
        val lines: List[Text] = shown.lines.skip(_ == t"", Rtl)
        var plain: List[Text] = Nil  // the preformatted lines in hand, in reverse

        def flushPlain(): Unit =
          if !plain.nil then
            val shown: List[Block.Line] = plain.reverse.map: (line: Text) =>
              Block.Line(if line == t"" then Nil else List(Token.plain(line)))

            blocks = Block.Code(Language.Plain, shown) :: blocks
            plain = Nil

        lines.each: (line: Text) =>
          val clause: Optional[Text] = Repl.messages.importedClause(line)

          if clause.absent then plain = line :: plain else clause.let: (clause: Text) =>
            flushPlain()
            blocks = imported(clause) :: blocks

        flushPlain()

    spans.each: (span: Repl.OutputSpan) =>
      if span.start > at then gap(plain.segment(at.z till span.start.z))
      blocks = Block.Output(plain.segment(span.start.z till (span.start + span.length).z), error = span.stream == Repl.stderrStream) :: blocks
      at = span.start + span.length

    if at < plain.length then gap(plain.skip(at))
    blocks.reverse

  // The result line, `name = value : Type <: base`, as code around the value's exhibit: one
  // paragraph when the value is phrasing, a group around it when it is flow.
  def result(name: Optional[Text], value: Optional[Inline | Block], tpe: Optional[Repl.TypeText]): List[Block] =
    if value.absent && tpe.absent then Nil else
      val binding: List[Inline] = name.lay(Nil: List[Inline]): (name: Text) =>
        List(Inline.Code(Language.Scala, List(Token(name, Token.Accent.Term, Token.Role.Binding), Token(t" ", Token.Accent.Unparsed), Token(t"=", Token.Accent.Parens), Token(t" ", Token.Accent.Unparsed))))

      val typed: List[Inline] = tpe.lay(Nil: List[Inline]): (tpe: Repl.TypeText) =>
        val widened: List[Token] = tpe.base.lay(Nil: List[Token]) { (base: Text) => List(Token(t" <: ", Token.Accent.Symbol), Token(base, Token.Accent.Typal)) }
        List(Inline.Code(Language.Scala, List(Token(t": ", Token.Accent.Symbol), Token(tpe.text, Token.Accent.Typal)) + widened))

      value match
        case block: Block =>
          List(Block.Group((if binding.nil then Nil else List(Block.Paragraph(binding))) + List(block) + (if typed.nil then Nil else List(Block.Paragraph(typed)))))

        case inline: Inline =>
          List(Block.Paragraph(binding + List(inline) + typed))

        case _ =>
          List(Block.Paragraph(binding + typed))

  // A stack trace, trimmed to the user's frames, through the model's own exhibition.
  def trace(trace: StackTrace): Block = trace.exhibit

  // Blocks on the wire: a TEL document of a group, empty for none (see `Repl.Reply`).
  // The wire form of a run of blocks: a product at the document's root, since a TEL document is
  // a record and a bare sum would lose its variant tag.
  object Payload:
    import contingency.strategies.throwUnsafely
    given telEncodable: Payload is Tel.Encodable = Tel.EncodableDerivation.derived[Payload]
    given telDecodable: Payload is Tel.Decodable =
      scala.caps.unsafe.unsafeAssumePure(Tel.DecodableDerivation.derived[Payload])

  case class Payload(content: List[Block])

  // No empty text may reach the document: stratiform writes an empty atom with the spacing
  // before it, which its own parser refuses as a trailing space (fixed upstream, pending a
  // release), so an empty line is a line of no tokens and an empty message is left out.
  def encode(blocks: List[Block]): Text = if blocks.nil then t"" else Payload(blocks).in[Tel].show

  def decode(text: Text): List[Block] =
    if text == t"" then Nil else safely(text.read[Tel].as[Payload]).lay(Nil: List[Block])(_.content)
