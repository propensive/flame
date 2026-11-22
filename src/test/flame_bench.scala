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

// A measurement, not a test: whether a line costs more the more lines its session already holds,
// and what a session that is opened and then abandoned leaves behind. `make bench-long` runs it,
// in one fresh JVM.
//
//   --lines <n>    grow the session to `n` lines (300)
//   --every <k>    take the measurements every `k` lines (25)
//   --warm <w>     first submit `w` lines to a scratch session, which is then closed, so that the
//                  JVM's own warming-up is not mistaken for a young session's cost (60)
//   --cpu          time by the processor time of the submitting and compiler threads rather than
//                  by the wall clock, which is what to compare two builds by on a busy machine
//   --abandon <m>  instead: open `m` sessions as a front-end does, one after another, submit a
//                  line to each and leave it unclosed, as a client that exited once did
//   --close        with `--abandon`, end each connection as the server does when its client has
//                  gone, which closes the session that was started for it
//
// A session is grown with filler lines — a `val`, a `def`, a `case class` and a `given` in turn —
// and at each checkpoint the same few lines are timed, as the median of five submissions: an
// expression (`expr`), a use of two early definitions (`early`), a Soundness line that needs a
// given (`given`), and a line that is rejected (`reject`); then a completion at a bare position
// (`compl`) and a scope probe of a multi-line definition (`scope`, once: the first after a line,
// which is the one that opens an inspector). `fill` is the mean cost of the filler lines since
// the last checkpoint. The lines timed are themselves lines of the session, and `lines` counts
// them. The heap is what remains in use after a collection.
@main
def longSession(args0: String*): Unit =
  given Scalac[3.9, Toolchain.Universe.Classfile] = Scalac(Nil)

  val args: scala.collection.immutable.Seq[String] = args0.stdlib
  val out = java.lang.System.out.nn

  def number(name: String, default: Int): Int =
    val index: Int = args.indexOf(name)

    if index < 0 || index + 1 >= args.length then default
    else args(index + 1).toIntOption.getOrElse(default)

  val lines: Int = number("--lines", 300)
  val every: Int = number("--every", 25).max(1)
  val warm: Int = number("--warm", 60)
  val abandon: Int = number("--abandon", 0)
  val runs: Int = 5

  // The clock: the wall's, or with `--cpu` the processor time used by the thread that asks and by
  // the sessions' compiler threads, which is where a line's cost is spent. A busy machine does not
  // stretch that as it does the wall's, and neither the JVM's own compiler nor its collector is
  // counted in it, so it is what to compare two builds by.
  val clock: () => Long =
    if !args.contains("--cpu") then () => java.lang.System.nanoTime else
      val bean = java.lang.management.ManagementFactory.getThreadMXBean.nn

      () =>
        val ids: scala.Array[Long] = bean.getAllThreadIds.nn
        var total: Long = bean.getCurrentThreadCpuTime
        var index: Int = 0

        while index < ids.length do
          val info: java.lang.management.ThreadInfo | Null = bean.getThreadInfo(ids(index))

          if info != null && info.getThreadName.nn.startsWith("flame-compiler")
          then total = total + bean.getThreadCpuTime(ids(index)).max(0L)

          index += 1

        total

  def millis(block: => Unit): Long =
    val start = clock()
    block
    (clock() - start)/1000000L

  def median(block: => Unit): Long =
    List.range(0, runs).map { _ => millis(block) }.stdlib.sorted.apply(runs/2)

  def heap(): Long =
    java.lang.System.gc()
    val runtime = Runtime.getRuntime.nn
    (runtime.totalMemory - runtime.freeMemory)/(1024L*1024L)

  def collecting(): Long =
    val beans = java.lang.management.ManagementFactory.getGarbageCollectorMXBeans.nn
    var total: Long = 0L
    var index: Int = 0

    while index < beans.size do
      total = total + beans.get(index).nn.getCollectionTime
      index += 1

    total

  def classes(): Int =
    java.lang.management.ManagementFactory.getClassLoadingMXBean.nn.getLoadedClassCount

  def threads(): Int = java.lang.management.ManagementFactory.getThreadMXBean.nn.getThreadCount

  def filler(index: Int): Text = index%4 match
    case 0 => t"val v$index = $index"
    case 1 => t"def f$index(n: Int): Int = n + $index"
    case 2 => t"case class C$index(x: Int)"
    case _ => t"given g$index: scala.math.Ordering[C${index - 1}] = scala.math.Ordering.by(_.x)"

  val samples: List[Text] =
    List(t"1 + 1", t"f1(v0)", t"t\"hello\".length", t"val bad: Int = undefinedName")

  supervise:
    // A session, and how many lines it holds: every line it did not refuse.
    class Aged():
      val repl = Repl.make[3.9](Repl.Prelude.empty, Repl.Rendering.Inspect)
      var size: Int = 0
      var next: Int = 0

      def submit(line: Text): Boolean = repl.react(1, line) match
        case Repl.Reply.Rejected(_, diagnostics, _, _) => false
        case _: Repl.Reply.Crashed                     => false
        case _: Repl.Reply.Failed                      => false
        case _                                         => (size += 1) yet true

      // Filler lines until the session holds `target`; the mean cost of one, in milliseconds.
      def fill(target: Int): Long =
        var count: Int = 0
        var refused: Int = 0
        val start: Long = clock()

        while size < target && refused < 20 do
          if !submit(filler(next)) then
            refused += 1
            out.println(s"  refused: ${filler(next).s}")

          next += 1
          count += 1

        if refused >= 20 then
          out.println("the filler lines are being refused; giving up")
          java.lang.System.exit(1)

        if count == 0 then 0L else (clock() - start)/1000000L/count

      def measure(filled: Long): Unit =
        val at: Int = size
        val scope: Long = millis(repl.scopeAt(t"def probe$at(a: Int) =\n  a"))
        val timed = samples.map { line => median(submit(line)) }.stdlib
        val completion: Long = median(repl.completionsAt(t"v", 1))

        out.println:
          String.format
            ( "%6s %6s %6s %6s %6s %6s %6s %6s %8s %8s %8s %8s",
              at.toString,
              filled.toString,
              timed(0).toString,
              timed(1).toString,
              timed(2).toString,
              timed(3).toString,
              completion.toString,
              scope.toString,
              heap().toString,
              collecting().toString,
              classes().toString,
              threads().toString )

    def aged(): Unit =
      if warm > 0 then
        val scratch = Aged()
        scratch.submit(t"/set experimental")
        scratch.submit(t"import soundness.*")
        scratch.fill(warm)
        samples.each { line => scratch.submit(line) }
        scratch.repl.completionsAt(t"v", 1)
        scratch.repl.close()
        out.println(s"warmed up with $warm lines in a scratch session")

      out.println:
        String.format
          ( "%6s %6s %6s %6s %6s %6s %6s %6s %8s %8s %8s %8s",
            "lines",
            "fill",
            "expr",
            "early",
            "given",
            "reject",
            "compl",
            "scope",
            "heap MB",
            "gc ms",
            "classes",
            "threads" )

      val session = Aged()
      session.submit(t"/set experimental")
      session.submit(t"import soundness.*")
      session.measure(session.fill(4))

      while session.size < lines do
        session.measure(session.fill((session.size/every + 1)*every))

    // What a launch costs the process it is served by: each is a connection that opens a session
    // of its own, is warmed up as a front-end's is, has a line or two submitted to it, and is
    // then let go — without the session being closed, unless `--close` ends the connection.
    def abandoned(): Unit =
      val sessions = Sessions(Repl.Rendering.Inspect, frontend = true)

      out.println:
        String.format
          ( "%8s %6s %6s %8s %8s %8s %8s",
            "sessions",
            "first",
            "expr",
            "heap MB",
            "gc ms",
            "classes",
            "threads" )

      List.range(0, abandon).each: count =>
        val connection = sessions.connection(_ => ())
        connection.respond(Repl.Request.Session(0, t""))
        Thread.sleep(50)
        while connection.name.let(sessions.session(_)).lay(false)(_.priming) do Thread.sleep(10)
        val first: Long = millis(connection.respond(Repl.Request.Submit(1, t"1 + 1")))
        val again: Long = median(connection.respond(Repl.Request.Submit(1, t"1 + 1")))
        if args.contains("--close") then connection.close()

        out.println:
          String.format
            ( "%8s %6s %6s %8s %8s %8s %8s",
              (count + 1).toString,
              first.toString,
              again.toString,
              heap().toString,
              collecting().toString,
              classes().toString,
              threads().toString )

    if abandon > 0 then abandoned() else aged()

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
