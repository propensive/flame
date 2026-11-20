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
import filesystemBackends.javaBaseFilesystem
import logging.silentLogging
import pathInterfaces.pathOnLinux
import probates.awaitProbate
import strategies.throwUnsafely
import pyrocosm.Shares
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
def firstLine(args0: String*): Unit =
  given Scalac[3.9, Toolchain.Universe.Classfile] = Scalac(Nil)

  // The options are read by position, which the stdlib's `Seq` gives directly.
  val args: scala.collection.immutable.Seq[String] = args0.stdlib
  val arguments: List[Text] = args0.map(_.tt)
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
        t"undefinedName.size",
        t"val bad: Int = undefinedName",
        t"def bad: Int = undefinedName",
        t"sys.error(\"thrown\")",
        t"/set experimental",
        t"import soundness.*",
        t"t\"hello\"",
        t"summon[rudiments.DecimalConverter]" )

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

// A measurement, not a test: what it costs to find Fuel's line about a name in the share Fuel
// publishes (`FuelHelp`), against the same lookup in a `Map` held in memory, which is what a
// copy of the index fetched whole used to be. `make bench-help` runs it; ARGS is the number of
// entries (10000 by default).
//
// Printed: publishing the index; the first lookup, which maps the file; a lookup of a name
// which is there and of one which is not; a completion's worth (40 names, one in four
// documented), one at a time and as the REPL asks, in one reading; the same against the `Map`;
// and a lookup just after a newer index is published.
@main
def helpLookup(args0: String*): Unit =
  val out = java.lang.System.out.nn
  val count: Int = args0.stdlib.headOption.flatMap(_.toIntOption).getOrElse(10000)

  def address(number: Int): Text = t"example.module$number.Type$number#member$number"

  def entries(suffix: Text): List[FuelEntry] =
    List.range(0, count).map: number =>
      FuelEntry(address(number), t"the member numbered $number of its type$suffix")

  // The median of `runs` timings of `action`, in nanoseconds, after as many again to warm up.
  def median(runs: Int)(action: => Unit): Long =
    def time(): Long =
      val start: Long = java.lang.System.nanoTime
      action
      java.lang.System.nanoTime - start

    List.range(0, runs).each { _ => time() }
    List.range(0, runs).map { _ => time() }.stdlib.sorted.apply(runs/2)

  def micros(nanos: Long): String = "%.1f µs".format(nanos/1000.0)

  pyrocosm.Filing.temporary: directory =>
    val file: Path on Linux = directory / "share" / "help.ratel"
    val index: FuelIndex = FuelIndex(entries(t""))
    val started: Long = java.lang.System.nanoTime
    Shares.publish(file, index)
    val published: Long = java.lang.System.nanoTime - started
    val size: Long = java.nio.file.Files.size(java.nio.file.Path.of(file.encode.s))
    out.println(s"$count entries, ${size/1024} KiB; published in ${published/1000000L} ms")

    val reader: Shares.Reader = Shares.Reader(file)
    val names: List[Text] = List.range(0, 40).map { number => address(number*(count/10)) }
    val present: Text = address(count/2)
    val absent: Text = address(count + 7)

    val began: Long = java.lang.System.nanoTime
    val first: Optional[Text] = FuelHelp.info(reader, present)
    out.println(s"first lookup (maps the file): ${micros(java.lang.System.nanoTime - began)}")
    out.println(s"  found: ${first.or(t"nothing")}")

    out.println(s"share, a name it has:    ${micros(median(2000)(FuelHelp.info(reader, present)))}")
    out.println(s"share, a name it lacks:  ${micros(median(2000)(FuelHelp.info(reader, absent)))}")

    out.println:
      s"share, 40 candidates:    ${micros(median(500)(names.each(FuelHelp.info(reader, _))))}"

    out.println(s"share, 40 in one read:   ${micros(median(500)(FuelHelp.infos(reader, names)))}")

    val map: Map[Text, Text] = Map(index.entry.map { entry => entry.address -> entry.info }*)
    out.println(s"Map, a name it has:      ${micros(median(2000)(map.at(present)))}")
    out.println(s"Map, 40 candidates:      ${micros(median(500)(names.each(map.at(_))))}")

    val rebuilt: Long = median(5):
      Map(index.entry.map { entry => entry.address -> entry.info }*)

    out.println(s"Map, built from the list: ${micros(rebuilt)} (the cost of each fetch before)")

    Shares.publish(file, FuelIndex(entries(t", revised")))
    Thread.sleep(Shares.interval + 5L)
    val again: Long = java.lang.System.nanoTime
    val revised: Optional[Text] = FuelHelp.info(reader, present)
    out.println(s"lookup after a new index: ${micros(java.lang.System.nanoTime - again)}")
    out.println(s"  found: ${revised.or(t"nothing")}")
    reader.close()
