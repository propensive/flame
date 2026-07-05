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

import scala.compiletime.summonFrom
import scala.language.implicitConversions

import anticipation.*
import honeycomb.*
import prepositional.*
import spectacular.*

// Renders a REPL result value as an HTML string for the web front-end, choosing the renderer by what
// typeclass can be resolved for the value's (static) type — a cascade:
//   1. a honeycomb `Renderable` → render to an `Html` node;
//   2. else a spectacular `Showable` → its text;
//   3. else `toString`.
// The `Showable`/`toString` text is wrapped in a `#text` node and serialized, so honeycomb HTML-escapes
// it: EVERY branch yields HTML-safe `Text` (a value whose text contains `<`/`&` cannot break the DOM).
//
// `render` is inlined into the compiled REPL wrapper (where the value's static type — and its
// Renderable/Showable instances — are in scope). It binds each summoned given by name and calls its
// typeclass method directly (`renderable.render` / `showable.text`), and delegates serialization to the
// public `serialize`/`escape` helpers, so the wrapper needs no honeycomb/spectacular extension imports
// — only `flame.HtmlRender` on the classpath.
object HtmlRender:
  inline def render[value](v: value): Text = summonFrom:
    case renderable: (`value` is Renderable) => serialize(renderable.render(v))
    case showable:   (`value` is Showable)   => escape(showable.text(v))
    case _                                   => escape(v.toString.tt)

  // Serialize an `Html` node to a `Text` (honeycomb's `Html is Showable`, which HTML-escapes text).
  def serialize(node: Html of ?): Text = node.show

  // Wrap plain text as a `#text` node and serialize it, so it is HTML-escaped.
  def escape(content: Text): Text =
    val node: Html of "#text" = content
    node.show
