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

import scala.compiletime.summonFrom

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import spectacular.*
import stratiform.*
import turbulence.*
import vacuous.*

import pyrocosm.{Block, Inline, Presentable}

import contingency.strategies.throwUnsafely
import hieroglyph.charEncoders.utf8Encoder

// The typeclass cascade for a result value under `Repl.Rendering.Exhibit`, expanded INSIDE
// the compiled wrapper where the value's static type is known: Pyrocosm's `Presentable` (whose
// own fallbacks cover `Showable`, products and `toString`), else spectacular's `Inspectable`
// with any styling stripped, else `toString`. The exhibit crosses `ReplBridge` as text, so
// `Outcome` and the bridge keep their `Text`: TEL, tagged with whether it is phrasing or flow.
object ExhibitRender:
  inline def render[value](v: value): Text = summonFrom:
    case presentable: (`value` is Presentable) => encode(presentable.exhibit(v))
    case inspectable: (`value` is Inspectable) => encode(Inline.Textual(SemanticRender.stripAnsi(inspectable.text(v)).trim))
    case _                                     => encode(Inline.Textual(v.toString.tt))

  def encode(form: Inline | Block): Text = form match
    case inline: Inline => t"inline:${inline.in[Tel].show}"
    case block: Block   => t"block:${block.in[Tel].show}"

  def decode(text: Text): Inline | Block =
    if text.starts(t"block:") then text.s.substring(6).nn.tt.read[Tel].as[Block]
    else if text.starts(t"inline:") then unquoted(text.s.substring(7).nn.tt.read[Tel].as[Inline])
    else Inline.Textual(text)

  // An inspection that found no instance marks the `toString` it fell back on with curly
  // quotes; the exhibit shows that `toString` plainly.
  private def unquoted(inline: Inline): Inline = inline match
    case Inline.Textual(text0) =>
      val text = text0.trim
      if text.length > 1 && text.s.charAt(0) == '\u201c' && text.s.charAt(text.length - 1) == '\u201d'
      then Inline.Textual(text.s.substring(1, text.length - 1).nn.tt)
      else Inline.Textual(text0)

    case Inline.Phrase(content)      => Inline.Phrase(content.map(unquoted))
    case Inline.Toned(tone, content) => Inline.Toned(tone, content.map(unquoted))
    case other                       => other
