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
import contingency.*
import denominative.*
import digression.*
import gossamer.*
import rudiments.*
import spectacular.*
import stratiform.*
import symbolism.*
import turbulence.*
import vacuous.*

import contingency.strategies.throwUnsafely
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
    val space: Int = line.s.indexOf(' ')
    if space < 0 then List(Repl.Token(line, t"command", Unset))
    else List(Repl.Token(line.keep(space), t"command", Unset), Repl.Token(line.skip(space), t"unparsed", Unset))

  // The tokens as a code block: lines split at the newline tokens `Repl.project` inserts, and
  // every marked token a note on its line (an error span erroneous, a warning cautionary).
  def code(tokens: List[Repl.Token]): Block.Code =
    val lines = sci.List.newBuilder[Block.Line]
    val notes = sci.List.newBuilder[Block.Note]
    var current: sci.List[Token] = sci.Nil
    var line: Int = 0
    var column: Int = 0

    def flush(): Unit =
      lines += Block.Line(List.from(current.reverse))
      current = sci.Nil

    tokens.each: (token: Repl.Token) =>
      val parts = token.text.cut(t"\n").stdlib
      parts.zipWithIndex.foreach: (part, index) =>
        if index > 0 then
          flush()
          line += 1
          column = 0

        if part != t"" then
          current = Blocks.token(token.copy(text = part)) :: current

          token.mark.let: (mark: Text) =>
            val style = if mark == Repl.warningMark then Block.Note.Style.Caution else Block.Note.Style.Erroneous
            notes += Block.Note(line, column, column + part.length, style)

          column += part.length

    flush()
    Block.Code(Language.Scala, List.from(lines.result()), List.from(notes.result()))

  // The marks alone, as notes by line, for a field's decoration.
  def marks(tokens: List[Repl.Token]): List[Block.Note] = code(tokens).notes

  // Captured output as blocks: each span of standard output or error verbatim behind its
  // gutter; whatever lies between the spans (the engine's own messages) as preformatted text,
  // stripped of ANSI when the medium cannot show it.
  def output(plain: Text, spans: List[Repl.OutputSpan], ansi: Boolean): List[Block] =
    val blocks = sci.List.newBuilder[Block]
    var at: Int = 0

    // A message ends in a newline, which is not a line of its own.
    def gap(text: Text): Unit =
      val shown: Text = if ansi then text else SemanticRender.stripAnsi(text)
      if shown.trim != t"" then
        val lines: scala.List[Text] = shown.cut(t"\n").stdlib.reverse.dropWhile(_ == t"").reverse
        blocks += Block.Code(Language.Plain, List.from(lines.map { (line: Text) => Block.Line(if line == t"" then Nil else List(Token.plain(line))) }))

    spans.each: (span: Repl.OutputSpan) =>
      if span.start > at then gap(plain.skip(at).keep(span.start - at))
      blocks += Block.Output(plain.skip(span.start).keep(span.length), error = span.stream == Repl.stderrStream)
      at = span.start + span.length

    if at < plain.length then gap(plain.skip(at))
    List.from(blocks.result())

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
