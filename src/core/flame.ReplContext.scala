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

import java.lang as jl

import scala.collection.concurrent.TrieMap
import scala.collection.immutable as sci

import ambience.*
import anticipation.*
import gossamer.*
import vacuous.*

// The contextual values a REPL line sees by default — a `WorkingDirectory`, an `Environment` and a
// `System` — reflecting the CLIENT that launched (or joined) the session: the directory `flame` was
// run from and that shell's environment, not the background daemon's, which hosts every session
// from wherever it happened to start. Each connection reports its values (`Repl.Request.Context`),
// which the server files here against the session; a line's run sets the session in
// `ReplBridge`'s thread-local first, so the givens below look up the right connection's values at
// each use. A session nobody has reported for (a web session, or an async thread the user spawned)
// falls back to the daemon's own directory and environment.
//
// The givens are CONDITIONAL — each takes the `Ambient` marker, always available from the same
// import — because a conditional given yields to a plain one at the same nesting level: a user who
// imports `environments.javaBaseEnvironment` on a line, or defines a `given Environment` on an
// earlier one, gets theirs, never an ambiguity. (`Repl#ambientImports` injects the import.)
object ReplContext:
  case class Values(workingDirectory: Text, environment: sci.Map[Text, Text])

  private val registry: TrieMap[Long, Values] = TrieMap()

  def set(session: Long, values: Values): Unit = registry(session) = values
  def clear(session: Long): Unit = registry.remove(session)

  private def current: Optional[Values] = registry.get(ReplBridge.currentSession()) match
    case Some(values) => values
    case None         => Unset

  private def daemonDirectory: Text = Optional(jl.System.getProperty("user.dir")).let(_.tt).or(t"/")

  class Ambient()
  given ambient: Ambient = Ambient()

  // The client's working directory, or the daemon's when none was reported.
  given workingDirectory: Ambient => WorkingDirectory = () =>
    current.let(_.workingDirectory).or(daemonDirectory)

  // The client's environment; a variable it lacks is absent even if the daemon has it.
  given environment: Ambient => Environment = new Environment:
    def variable(name: Text): Optional[Text] = current match
      case values: Values => values.environment.get(name) match
        case Some(value) => value
        case None        => Unset
      case _ => Optional(jl.System.getenv(name.s)).let(_.tt)

  // The daemon JVM's properties (the client is a shell, with none of its own), except that
  // `user.dir` is the client's working directory — so everything derived from it (a
  // `systemWorkingDirectory`, a `TemporaryDirectory` read from the properties) follows the client.
  given system: Ambient => System = new System:
    def apply(name: Text): Optional[Text] =
      if name == t"user.dir" then current.let(_.workingDirectory).or(daemonDirectory)
      else Optional(jl.System.getProperty(name.s)).let(_.tt)
