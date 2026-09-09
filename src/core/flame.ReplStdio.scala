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

import java.io as ji
import java.lang as jl

import anticipation.*
import turbulence.*
import vacuous.*

// The ambient `Stdio` of a REPL line, so that `Out.println` works as `println` does, with no
// import. It is supplied NOT as a `Stdio` given but as a `Stdio.Provider`: turbulence's
// companion-scope `Stdio.provided` derives a `Stdio` from a provider only when the lexical scope
// offers no `Stdio` at all, so a user's own `import stdios.javaLangSystemStdio` or `given Stdio`
// — at any level — takes over without ever being ambiguous with this one. `Repl` injects
// `import flame.ReplStdio.provider` into every line's context (see `Repl#ambientImports`).
//
// The streams resolve `System.out`/`System.err` at each WRITE, not when the `Stdio` is built:
// the REPL routes a run's `System.out` to the submission's capture per thread
// (`Repl.CapturedOut`), and a stream captured once would be bound to whichever stream was
// current at class-initialisation. The termcap is likewise resolved per thread at each query,
// set by the REPL around a run: ANSI for the terminal front-end, plain for the web page, whose
// captured output is shown as text.
object ReplStdio:
  private val termcaps: ThreadLocal[Termcap] = ThreadLocal()

  // Runs `body` with `termcap` as the ambient `Stdio`'s termcap on this thread.
  def withTermcap[result](termcap: Termcap)(body: => result): result =
    val previous: Termcap | Null = termcaps.get
    termcaps.set(termcap)
    try body finally termcaps.set(previous)

  private object currentTermcap extends Termcap:
    private def current: Termcap = Optional(termcaps.get).or(termcapDefinitions.basicTermcap)
    def ansi: Boolean = current.ansi
    def color: ColorDepth = current.color

  // Plain objects rather than closures over a by-name target: a by-name stream is a tracked
  // capability the `OutputStream` would capture, which its pure declared type forbids.
  private object outStream extends ji.OutputStream:
    def write(byte: Int): Unit = jl.System.out.nn.write(byte)
    override def write(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Unit =
      jl.System.out.nn.write(bytes, offset, length)
    override def flush(): Unit = jl.System.out.nn.flush()

  private object errStream extends ji.OutputStream:
    def write(byte: Int): Unit = jl.System.err.nn.write(byte)
    override def write(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Unit =
      jl.System.err.nn.write(bytes, offset, length)
    override def flush(): Unit = jl.System.err.nn.flush()

  val stdio: Stdio =
    Stdio
     ( ji.PrintStream(outStream, true, "UTF-8"),
       ji.PrintStream(errStream, true, "UTF-8"),
       jl.System.in.nn,
       currentTermcap )

  given provider: Stdio.Provider = new Stdio.Provider:
    val stdio: Stdio = ReplStdio.stdio
