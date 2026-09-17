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

import ambience.*
import anticipation.*
import gossamer.*
import proscenium.*
import rudiments.*
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
  case class Values(workingDirectory: Text, environment: Map[Text, Text])

  // An atomic cell holding an immutable map, rather than a mutable concurrent map: every update
  // installs a whole new map, so a reader either sees the session's values or does not see the
  // session at all, and `Map.at` answers with an `Optional` directly.
  private val registry: Atomic.Ref[Map[Long, Values]] = Atomic.Ref(Map())

  def set(session: Long, values: Values): Unit = registry.revise(_.define(session, values))
  def clear(session: Long): Unit = registry.revise(_.omit(session))

  private def current: Optional[Values] = registry().at(ReplBridge.currentSession())

  // The JVM's own view, through ambience rather than `java.lang.System` directly: these are the
  // FALLBACKS the givens below use when no client has reported values for the session.
  private def daemonProperty(name: Text): Optional[Text] =
    import systems.javaBaseSystem
    summon[System](name)

  private def daemonVariable(name: Text): Optional[Text] =
    import environments.javaBaseEnvironment
    summon[Environment].variable(name)

  private def daemonDirectory: Text = daemonProperty(t"user.dir").or(t"/")

  class Ambient()
  given ambient: Ambient = Ambient()

  // The client's working directory, or the daemon's when none was reported.
  given workingDirectory: Ambient => WorkingDirectory = () =>
    current.let(_.workingDirectory).or(daemonDirectory)

  // The client's environment; a variable it lacks is absent even if the daemon has it.
  given environment: Ambient => Environment = new Environment:
    def variable(name: Text): Optional[Text] = current.lay(daemonVariable(name)):
      values => values.environment.at(name)

  // The daemon JVM's properties (the client is a shell, with none of its own), except that
  // `user.dir` is the client's working directory — so everything derived from it (a
  // `systemWorkingDirectory`, a `TemporaryDirectory` read from the properties) follows the client.
  given system: Ambient => System = new System:
    def apply(name: Text): Optional[Text] =
      if name == t"user.dir" then current.let(_.workingDirectory).or(daemonDirectory)
      else daemonProperty(name)
