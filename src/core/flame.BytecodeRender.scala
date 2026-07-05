                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃    Soundness, version 0.54.0. © Copyright 2021-25 Jon Pretty, Propensive OÜ.                     ┃
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
import escapade.*, termcapDefinitions.xtermTrueColorTermcap
import gossamer.*
import hellenism.*
import iridescence.*
import mandible.*
import prepositional.*
import vacuous.*

// Renders the JVM bytecode of a compiled class to a coloured, truecolor-ANSI listing, for the REPL's
// `/bytecode` command. Uses Soundness `mandible`: `Classfile(resource)` reads the class BYTES through
// a `Classloader` (it does not load or initialise the class), and each `Method`'s disassembled
// `Bytecode` renders as an escritoire table (Source | Offset | Opcode | Stack) via its `Teletypeable`.
object BytecodeRender:
  private def hex(rgb: Int): Color in Srgb =
    Srgb(((rgb >> 16) & 0xff)/255.0, ((rgb >> 8) & 0xff)/255.0, (rgb & 0xff)/255.0)

  // Zed-palette colours (see `flame_client.scala`): opcodes peach like a term, the source column
  // subdued like a comment, and the stack-frame outline in the faint gutter grey.
  private given palette: BytecodePalette = new Palette:
    type Form = Srgb
    def background: Color in Srgb = hex(0x000000)
    def foreground: Color in Srgb = hex(0xd4be98)
    def bytecode:   Color in Srgb = hex(0xffcc99)
    def sourceCode: Color in Srgb = hex(0x928374)
    def outline:    Color in Srgb = hex(0x5a524c)

  // Method names that are compiler scaffolding, never the user's code: the constructors and the
  // `writeReplace` the wrapper object gets for serialization. Filtered out so the listing shows only
  // the disassembled user methods.
  private val synthetic: Set[Text] = Set(t"<init>", t"<clinit>", t"writeReplace")

  // Renders every non-synthetic method of `resource` (a `<class>.class` resource name loadable by
  // `classloader`) under a bold header. `Unset` if the class can't be read; an empty `Text` if it
  // has no disassemblable user methods (the caller reports either).
  def render(resource: Text)(using Classloader): Optional[Text] =
    Classfile(resource).let: classfile =>
      classfile.methods
        .filter { method => !synthetic.contains(method.name) }
        .map { method => (method.name, method.bytecode) }
        .collect { case (name, code: Bytecode) => e"$Bold($name)\n${code.teletype}" }
        .join(e"\n\n")
        .render(xtermTrueColorTermcap)
