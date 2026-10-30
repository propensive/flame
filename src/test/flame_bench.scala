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

import classloaders.threadContextClassloader
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import threads.platformThreads

// A measurement, not a test: how long a session's FIRST lines take against the same lines
// repeated, in a JVM that has compiled nothing yet. `make bench` runs it; each run is one fresh
// JVM, since what it measures is spent only once per process or per session.
//
//   --wait <ms>   idle between opening a session and its first line: the time a user spends
//                 typing, which is what a background warm-up has to work with
//   --settle      wait, before a session's first line, until its warm-up is over, and print when
//                 each step of it was reported (with `--sessions`)
//   --probes <f>  submit the lines of the file `f` instead of the built-in ones
//   --inspect     render as text, and print why a line was rejected
//   --sessions    open the session through `Sessions` and a `Connection`, as a front-end does,
//                 rather than as a bare `Repl`
//
// Session A submits every probe twice (first, then repeat); session B, opened afterwards in the
// same JVM, shows how much of a first line's cost is the session's own rather than the JVM's.
@main
def firstLine(args: String*): Unit =
  given Scalac[3.9, Universe.Classfile] = Scalac(Nil)

  val arguments: List[Text] = List.from(args.map(_.tt))
  val viaSessions: Boolean = arguments.has(t"--sessions")
  val settle: Boolean = arguments.has(t"--settle")
  val began: Long = java.lang.System.nanoTime

  val warming: java.util.concurrent.ConcurrentLinkedQueue[String] =
    java.util.concurrent.ConcurrentLinkedQueue()

  val out = java.lang.System.out.nn

  val waitMillis: Long =
    val index: Int = args.indexOf("--wait")
    if index < 0 || index + 1 >= args.length then 0L else safely(args(index + 1).toLong).or(0L)

  def option(name: String): Optional[String] =
    val index: Int = args.indexOf(name)
    if index < 0 || index + 1 >= args.length then Unset else args(index + 1)

  val inspect: Boolean = arguments.has(t"--inspect")

  val rendering: Repl.Rendering =
    if inspect then Repl.Rendering.Inspect else Repl.Rendering.Exhibit(true)

  val builtin: List[Text] =
    List
      ( t"1 + 1",
        t"List(1, 2, 3).map(_ + 1)",
        t"\"hello\".length",
        t"case class Point(x: Int, y: Int)",
        t"Point(1, 2)",
        t"def twice(n: Int): Int = n*2",
        t"val wrong: Int = \"text\"",
        t"sys.error(\"thrown\")",
        t"/set experimental",
        t"import soundness.*",
        t"t\"hello\"" )

  val probes: List[Text] = option("--probes").lay(builtin): file =>
    val text: String = java.nio.file.Files.readString(java.nio.file.Path.of(file)).nn
    text.tt.cut(t"\n").filter(_.trim != t"")

  def millis(block: => Unit): Long =
    val start = java.lang.System.nanoTime
    block
    (java.lang.System.nanoTime - start)/1000000L

  def kind(reply: Optional[Repl.Reply]): Text = reply match
    case _: Repl.Reply.Ran => t"ran"

    case Repl.Reply.Rejected(_, diagnostics, _, _) =>
      if inspect then t"rejected: ${diagnostics.keep(300).sub(t"\n", t" / ")}" else t"rejected"

    case _: Repl.Reply.Threw    => t"threw"
    case _: Repl.Reply.Crashed  => t"crashed"
    case _: Repl.Reply.Failed   => t"failed"
    case _                      => t"none"

  def row(line: Text, first: Long, repeat: Optional[Long], outcome: Text): Unit =
    val again: Text = repeat.let(_.toString.tt).or(t"-")
    out.println(String.format("%-64s %8s %8s  %s", line.s, first.toString, again.s, outcome.s))

  supervise:
    // Opens a session and answers with the way to submit a line to it.
    def open(): Text => Optional[Repl.Reply] =
      if viaSessions then
        val sessions = Sessions(rendering, frontend = true)

        val connection = sessions.connection:
          case Repl.Reply.Warming(_, done, total) =>
            val elapsed: Long = (java.lang.System.nanoTime - began)/1000000L
            warming.add(s"  warming $done/$total at ${elapsed}ms")
            ()

          case _ => ()

        connection.respond(Repl.Request.Session(0, t""))

        if settle then
          Thread.sleep(50)
          while connection.name.let(sessions.session(_)).lay(false)(_.priming) do Thread.sleep(10)
          while !warming.isEmpty do out.println(warming.poll())

        line => connection.respond(Repl.Request.Submit(1, line))
      else
        val repl = Repl.make[3.9](Repl.Prelude.empty, rendering)
        line => repl.react(1, line)

    val via: Text = if viaSessions then t"through Sessions" else t"bare Repl"
    out.println(t"wait ${waitMillis}ms, $via".s)
    out.println(String.format("%-64s %8s %8s  %s", "session A", "first", "repeat", ""))

    val opened: Long = java.lang.System.nanoTime
    val submitA = open()
    row(t"(open)", (java.lang.System.nanoTime - opened)/1000000L, Unset, t"")
    Thread.sleep(waitMillis)

    probes.each: probe =>
      var reply: Optional[Repl.Reply] = Unset
      val first: Long = millis { reply = submitA(probe) }
      val repeat: Long = millis(submitA(probe))
      row(probe, first, repeat, kind(reply))

    out.println(String.format("%-64s %8s", "session B", "first"))
    val submitB = open()
    Thread.sleep(waitMillis)

    List(t"1 + 1", t"List(1, 2, 3).map(_ + 1)").each: probe =>
      var reply: Optional[Repl.Reply] = Unset
      val first: Long = millis { reply = submitB(probe) }
      row(probe, first, Unset, kind(reply))

  java.lang.System.exit(0)
