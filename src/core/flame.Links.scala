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

import soundness.*

import pyrocosm.{Block, Inline, Token}

// A reply's names as links to their documentation. The engine, which knows what each name in
// a type or a line of code is, leaves that on the name's token as its hint: the address of
// what it names (`xylophone.Xml`). A front-end which has somewhere to send a reader — Fuel's
// pages, while Fuel is serving them (`FuelHelp.pages`) — gives each such token the link of its
// address's page; a name with no page, and every name while nothing is served, keeps its hint
// and no more. A token which already links is left as it is.
object Links:
  // Every address hinted at in `blocks`, each once.
  def addresses(blocks: List[Block]): List[Text] =
    var found: List[Text] = Nil

    def note(token: Token): Token =
      if token.link.absent then token.hint.let: (hint: Text) =>
        if !found.has(hint) then found = hint :: found

      token

    blocks.each { (block: Block) => walk(block, note) }
    found.reverse

  // `blocks`, with each token whose hint `pages` has a URL for linked to it.
  def link(blocks: List[Block], pages: Map[Text, Text]): List[Block] =
    def linked(token: Token): Token =
      if token.link.present then token
      else
        token.hint.let(pages.at(_)).lay(token): (url: Text) =>
          token.copy(link = Inline.Destination.External(url))

    blocks.map { (block: Block) => walk(block, linked) }

  // `blocks` linked through `lookup`, which is asked once, for every address at a time, and
  // not at all where nothing is hinted at.
  def resolve(blocks: List[Block], lookup: List[Text] -> Map[Text, Text]): List[Block] =
    val wanted: List[Text] = addresses(blocks)

    if wanted.nil then blocks else
      val pages: Map[Text, Text] = lookup(wanted)
      if wanted.all(pages.at(_).absent) then blocks else link(blocks, pages)

  // `block` with `change` made to every token of code in it: the walk covers what a reply is
  // made of — code, a paragraph, a group, a notice, a diagnostic, a table, a tree — and passes
  // anything else through untouched.
  private def walk(block: Block, change: Token => Token): Block =
    def blocks(content: List[Block]): List[Block] = content.map(walk(_, change))
    def inlines(content: List[Inline]): List[Inline] = content.map(inline)

    def inline(inline: Inline): Inline = inline match
      case Inline.Snippet(language, tokens) => Inline.Snippet(language, tokens.map(change))
      case Inline.Phrase(content)           => Inline.Phrase(inlines(content))
      case Inline.Toned(tone, content)      => Inline.Toned(tone, inlines(content))
      case Inline.Strong(content)           => Inline.Strong(inlines(content))
      case Inline.Emphasis(content)         => Inline.Emphasis(inlines(content))
      case other                            => other

    def recur(node: Block.TreeNode): Block.TreeNode =
      node.copy(label = inlines(node.label), children = node.children.map(recur(_)))

    block match
      case code: Block.Code =>
        code.copy(lines = code.lines.map: (line: Block.Line) =>
          Block.Line(line.tokens.map(change)))

      case Block.Paragraph(content)       => Block.Paragraph(inlines(content))
      case Block.Group(content)           => Block.Group(blocks(content))
      case Block.Notice(tone, title, content) => Block.Notice(tone, title, blocks(content))
      case Block.Tree(roots)              => Block.Tree(roots.map(recur(_)))

      case diagnostic: Block.Diagnostic =>
        diagnostic.copy
          ( message = blocks(diagnostic.message),
            excerpt = blocks(diagnostic.excerpt),
            detail  = blocks(diagnostic.detail) )

      case Block.Table(columns, rows, caption) =>
        val rows2 = rows.map: (row: Block.Row) =>
          row.copy(cells = row.cells.map: (cell: Block.Cell) =>
            Block.Cell(inlines(cell.content)))

        Block.Table(columns, rows2, caption)

      case other => other
