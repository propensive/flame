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
import denominative.*
import escapade.*, termcapDefinitions.xtermTrueColorTermcap
import gossamer.*
import hellenism.*
import iridescence.*
import mandible.*
import prepositional.*
import pyrocosm.{Block, Bytecodes, Inline, Language, Token}
import rudiments.*
import vacuous.*
import FlameColours.srgb

// Renders the JVM bytecode of a compiled class for the REPL's `/bytecode` command. Uses Soundness
// `mandible`: `Classfile(resource)` reads the class BYTES through a `Classloader` (it does not
// load or initialise the class), and each `Method`'s disassembled `Bytecode` is shown as a table
// (Source | Offset | Opcode | Stack) — under `Rendering.Exhibit` as Pyrocosm's `Block.Table`
// (`blocks`), under `Rendering.Inspect` as a coloured, truecolor-ANSI escritoire table (`render`).
// Names are demangled by digression's rules, and the wrapper object by Flame's (`Names`).
object BytecodeRender:
  // Zed-palette colours (see `flame_client.scala`): opcodes peach like a term, the source column
  // subdued like a comment, and the stack-frame outline in the faint gutter grey.
  private given palette: BytecodePalette = new Palette:
    type Form = Srgb
    def background: Color in Srgb = srgb(FlameColours.background)
    def foreground: Color in Srgb = srgb(FlameColours.foreground)
    def bytecode:   Color in Srgb = srgb(FlameColours.term)
    def sourceCode: Color in Srgb = srgb(FlameColours.comment)
    def outline:    Color in Srgb = srgb(FlameColours.subdued)

  // Method names that are compiler scaffolding, never the user's code: the constructors and the
  // `writeReplace` the wrapper object gets for serialization. Filtered out so the listing shows
  // only the disassembled user methods.
  private val synthetic: Set[Text] = Set(t"<init>", t"<clinit>", t"writeReplace")

  // Renders every non-synthetic method of `resource` (a `<class>.class` resource name loadable by
  // `classloader`) under a bold header. `Unset` if the class can't be read; an empty `Text` if it
  // has no disassemblable user methods (the caller reports either).
  def render(resource: Text)(using Classloader): Optional[Text] =
    Classfile(resource).let: classfile =>
      classfile.methods
        .filter: method => !synthetic.has(method.name)
        .map: method => (method.name, method.bytecode)
        .sweep:
          case (name, code: Bytecode) => e"$Bold(${Names.rewrite(name, true)})\n${code.teletype}"
        .join(e"\n\n")
        .render(xtermTrueColorTermcap)
        .pipe(Names.wrapper(_))

  // The same methods as blocks: each a group of its demangled name, as a bound term, and its
  // bytecode's table. `Unset` if the class can't be read or has no disassemblable user method.
  def blocks(resource: Text)(using Classloader): Optional[Block] =
    Classfile(resource).let: classfile =>
      val groups: List[Block] =
        classfile.methods
          .filter: method => !synthetic.has(method.name)
          .map: method => (method.name, method.bytecode)
          .sweep { case (name, code: Bytecode) => group(name, code) }

      if groups.nil then Unset else Names.block(Block.Group(groups))

  private def group(name: Text, code: Bytecode): Block =
    val header: Token = Token(Names.rewrite(name, true), Token.Accent.Term, Token.Role.Binding)
    val snippet: Inline = Inline.Snippet(Language.Scala, List(header))
    val title: Block = Block.Paragraph(List(Inline.Strong(List(snippet))))
    Block.Group(List(title, Bytecodes.table(code)))
