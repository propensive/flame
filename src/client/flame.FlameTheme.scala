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

import anticipation.Chroma
import pyrocosm.{TerminalTheme, Token, Tone}

// Flame's terminal theme: the Zed colours the REPL has always used (see `FlameColours`),
// mapped onto Pyrocosm's vocabulary.
object FlameTheme extends TerminalTheme:
  def foreground: Chroma = Chroma(FlameColours.foreground)
  def muted: Chroma = Chroma(FlameColours.subdued)
  def key: Chroma = Chroma(FlameColours.parameter)
  def reference: Chroma = Chroma(FlameColours.term)
  def figure: Chroma = Chroma(FlameColours.number)
  def units: Chroma = Chroma(FlameColours.comment)
  def link: Chroma = Chroma(FlameColours.string)
  def selection: Chroma = Chroma(FlameColours.selection)

  def tone(tone: Tone): Chroma = tone match
    case Tone.Success => Chroma(FlameColours.tpe)
    case Tone.Failure => Chroma(FlameColours.error)
    case Tone.Warning => Chroma(FlameColours.parameter)
    case Tone.Muted   => Chroma(FlameColours.comment)
    case Tone.Accent  => Chroma(FlameColours.term)
    case Tone.Info    => Chroma(FlameColours.string)

  def accent(accent: Token.Accent): Chroma = accent match
    case Token.Accent.Keyword  => Chroma(FlameColours.keyword)
    case Token.Accent.Modifier => Chroma(FlameColours.keyword)
    case Token.Accent.Command  => Chroma(FlameColours.keyword)
    case Token.Accent.String   => Chroma(FlameColours.string)
    case Token.Accent.Number   => Chroma(FlameColours.number)
    case Token.Accent.Term     => Chroma(FlameColours.term)
    case Token.Accent.Typal    => Chroma(FlameColours.tpe)
    case Token.Accent.Symbol   => Chroma(FlameColours.symbol)
    case Token.Accent.Parens   => Chroma(FlameColours.operator)
    case Token.Accent.Error    => Chroma(FlameColours.error)
    case Token.Accent.Unparsed => Chroma(FlameColours.foreground)
