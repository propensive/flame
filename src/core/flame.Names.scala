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
import digression.*
import pyrocosm.{Block, Inline, Token}
import rudiments.*
import vacuous.*

// How the REPL's own names are shown. Each line is compiled into a wrapper object `rs$line$N`
// (`Repl.Layout.Standard.objectName`), whose name must stay as it is — the compiler recognises
// the `rs$line$` prefix and keeps it out of the types it prints — but which is session
// bookkeeping no user should read in a rendering. In a bytecode listing or a TASTy tree it is
// shown as `lineₙ`, in the subscripted style digression demangles with (`λ₁`, `δ₂`).
//
// The wrapper reaches a rendering in two spellings: raw, `rs$line$3` (a classfile's own names,
// a source line), and already demangled by digression, `rs#line₃` (what mandible and hyperbole
// emit, since `$line` matches none of digression's tokens and falls back to `#`). A trailing `#`
// or `$` after the number marks the module class, and is dropped; a `#` that introduces a nested
// class (`rs#line₃#Foo`) is kept, as digression's own separator.
object Names:
  private def subscript(digits: String): String =
    digits.map { (digit: Char) => ('₀' + (digit - '0')).toChar }

  // `java.util.regex`: the three rewrites are textual, and the stdlib's `Regex` takes a
  // replacement function, which a subscript conversion needs.
  private val raw = "rs\\$line\\$([0-9]+)".r
  private val demangled = "rs#line([₀-₉]+)".r
  private val module = "line([₀-₉]+)[#$](?=$|[\\s.)\\],;:])".r

  def wrapper(text: Text): Text =
    val step1: String = raw.replaceAllIn(text.s, { m => "line"+subscript(m.group(1).nn) })
    val step2: String = demangled.replaceAllIn(step1, { m => "line"+m.group(1).nn })
    module.replaceAllIn(step2, { m => "line"+m.group(1).nn }).tt

  // A JVM name by digression's rules, with the wrapper in Flame's. `method` appends `()`.
  def rewrite(name: Text, method: Boolean = false): Text =
    wrapper(StackTrace.rewrite(wrapper(name).s, method))

  // `wrapper` over every text of a block. The walk covers only what the bytecode and TASTy
  // exhibitions produce — a group, a paragraph, a table, a tree, and the phrasing within them —
  // and passes anything else through untouched.
  def block(block: Block): Block = block match
    case Block.Group(content)        => Block.Group(content.map(this.block))
    case Block.Paragraph(content)    => Block.Paragraph(inlines(content))
    case Block.Tree(roots)           => Block.Tree(roots.map(node))

    case Block.Table(columns, rows, caption) =>
      val columns2 = columns.map: (column: Block.Column) =>
        column.copy(title = inlines(column.title))

      val rows2 = rows.map: (row: Block.Row) =>
        row.copy(cells = row.cells.map { (cell: Block.Cell) => Block.Cell(inlines(cell.content)) },
                 note = inlines(row.note))

      Block.Table(columns2, rows2, caption.let(inlines(_)))

    case other                       => other

  private def node(node: Block.TreeNode): Block.TreeNode =
    node.copy(label = inlines(node.label), children = node.children.map(this.node))

  private def inlines(content: List[Inline]): List[Inline] = content.map(inline)

  private def inline(inline: Inline): Inline = inline match
    case Inline.Textual(text)          => Inline.Textual(wrapper(text))
    case Inline.Toned(tone, content)   => Inline.Toned(tone, inlines(content))
    case Inline.Strong(content)        => Inline.Strong(inlines(content))
    case Inline.Emphasis(content)      => Inline.Emphasis(inlines(content))

    case Inline.Snippet(language, tokens) =>
      val tokens2 = tokens.map { (token: Token) => token.copy(text = wrapper(token.text)) }
      Inline.Snippet(language, tokens2)

    case other                         => other
