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
import hyperbole.*
import iridescence.*
import prepositional.*

// Renders a hyperbole `TastyTree` (the typed AST produced by the `syntax` macro) to a coloured,
// truecolor-ANSI table, for the REPL's `/tasty` command. Called by name from the generated probe
// object each `/tasty` line compiles — so it must be plain (non-`@experimental`) and reachable by
// the fully-qualified name `flame.TastyRender.render`, exactly as the REPL reaches `ReplBridge`.
object TastyRender:
  private def hex(rgb: Int): Color in Srgb =
    Srgb(((rgb >> 16) & 0xff)/255.0, ((rgb >> 8) & 0xff)/255.0, (rgb & 0xff)/255.0)

  // Colours matched to the CLI's Zed-derived syntax palette (see `flame_client.scala`): a type is
  // teal, a term is peach; definitions are shown at full strength and references a little muted, so
  // an introduced binding stands out from a use of it. `outline`/`accented` colour the `: Type`
  // column and the italic parameter tag.
  private given palette: TastyPalette = new Palette:
    type Form = Srgb
    def background:     Color in Srgb = hex(0x000000)
    def foreground:     Color in Srgb = hex(0xd4be98)
    def typeDefinition: Color in Srgb = hex(0x00cc99)
    def termDefinition: Color in Srgb = hex(0xffcc99)
    def typeReference:  Color in Srgb = hex(0x4ec9b0)
    def termReference:  Color in Srgb = hex(0xd4be98)
    def flagOn:         Color in Srgb = hex(0x00cc99)
    def flagOff:        Color in Srgb = hex(0x5a524c)
    def propertyOn:     Color in Srgb = hex(0xff6633)
    def propertyOff:    Color in Srgb = hex(0x5a524c)
    def outline:        Color in Srgb = hex(0x928374)
    def accented:       Color in Srgb = hex(0xff6633)

  // The tree's `Teletypeable` (from hyperbole, in `TastyTree`'s implicit scope) renders the escritoire
  // table; `render(termcap)` flattens the styled `Teletype` to truecolor ANSI text — which the CLI
  // prints verbatim through the REPL's `output` channel.
  def render(tree: TastyTree): Text = tree.teletype.render(xtermTrueColorTermcap)
