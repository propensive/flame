                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
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

import _root_.java.io as ji
import _root_.java.net as jn
import _root_.java.nio.channels as jnc

import soundness.*

import classloaders.threadContextClassloader
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.overwritePreexisting.disabled
import interfaces.paths.pathOnLinux
import probates.awaitProbate
import logging.silentLogging
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading

// Sends `request` to `output` as a length-prefixed BinTEL frame — the on-wire protocol
// the server speaks (a 4-byte big-endian length, then that many BinTEL body bytes).
private def send(output: ji.OutputStream, request: Repl.Request): Unit =
  val body = request.bintel
  val out  = ji.DataOutputStream(output)
  out.writeInt(body.length)
  out.write(body.mutable(using Unsafe))
  out.flush()

private def send(socket: jn.Socket, request: Repl.Request): Unit =
  send(socket.getOutputStream.nn, request)

// Sends `request` and reads the framed BinTEL reply, decoding it to a typed `Reply`.
private def exchange(input: ji.InputStream, output: ji.OutputStream, request: Repl.Request)
:   Repl.Reply =
  send(output, request)
  val in     = ji.DataInputStream(input)
  val length = in.readInt()
  val bytes  = new Array[Byte](length)
  in.readFully(bytes)
  Bintel.read[Repl.Reply](bytes.immutable(using Unsafe))

private def exchange(socket: jn.Socket, request: Repl.Request): Repl.Reply =
  exchange(socket.getInputStream.nn, socket.getOutputStream.nn, request)

// Mimics a standard-REPL session: `size` references `greeting`, a field of the
// enclosing object, by simple name (as `var name = …` would be in the Scala REPL).
// `session` reads `size` once, mutates `greeting`, then reads it again — the
// second read should track the change (a live binding), not the captured value.
object ReplFixture:
  var greeting: String = "hello"

  def session(using Scalac[3.8], Classloader, TemporaryDirectory, Monitor, System, Probate)
  :   (Repl.Outcome, Repl.Outcome) =
    val repl = Repl[3.8]:
      def size: Int = greeting.length

    val before: Repl.Outcome = repl.interpret(t"size")
    greeting = "changed"
    (before, repl.interpret(t"size"))

object Tests extends Suite(m"Flame Tests"):
  def run(): Unit =
    suite(m"REPL tests"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"a definition is visible on a later line"):
        supervise:
          val repl = Repl()
          repl.interpret(t"val x = 40")
          repl.interpret(t"println(x + 2)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

      test(m"an import persists to a later line"):
        supervise:
          val repl = Repl()
          repl.interpret(t"import scala.collection.mutable.ListBuffer")
          repl.interpret(t"ListBuffer(1, 2, 3)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

      test(m"/unimport removes an import so it is no longer in scope"):
        supervise:
          val repl = Repl()
          repl.interpret(t"import scala.collection.mutable.ListBuffer")
          val before = repl.interpret(t"ListBuffer(1, 2, 3)")
          repl.interpret(t"/unimport scala.collection.mutable.ListBuffer")
          (before, repl.interpret(t"ListBuffer(1, 2, 3)"))
      . assert:
          case (Repl.Outcome.Ran(_, _, _, _, _), Repl.Outcome.Rejected(_)) => true
          case _                                                     => false

      test(m"/unimport with no argument lists the removable imports"):
        supervise:
          val repl = Repl()
          repl.interpret(t"import scala.collection.mutable.ListBuffer")
          repl.interpret(t"/unimport")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"scala.collection.mutable.ListBuffer")
          case _                              => false

      test(m"/unimport reports when no import matches the given tokens"):
        supervise:
          Repl().interpret(t"/unimport nonsense.does.not.exist")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"no matching import")
          case _                              => false

      test(m"a given declared on one line is in scope on a later line"):
        supervise:
          val repl = Repl()
          repl.interpret(t"given Int = 42")
          repl.interpret(t"summon[Int]")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value == t"42"
          case _                             => false

      test(m"a persisted given resolves a later `using` parameter"):
        supervise:
          val repl = Repl()
          repl.interpret(t"given Int = 7")
          repl.interpret(t"def double(using n: Int) = n*2")
          repl.interpret(t"double")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value == t"14"
          case _                             => false

      test(m"a `val` definition shows its name, value and type"):
        supervise:
          Repl().interpret(t"val x = 40 + 2")
      . assert:
          case Repl.Outcome.Ran(_, value, _, name, tpe) =>
            value == t"42" && name == t"x" && tpe == t"scala.Int"
          case _ => false

      test(m"a `var` definition shows its name, value and type"):
        supervise:
          Repl().interpret(t"var y = List(1, 2, 3)")
      . assert:
          case Repl.Outcome.Ran(_, value, _, name, _) => value == t"[1, 2, 3]" && name == t"y"
          case _                                       => false

      test(m"a `val` definition still persists for later lines"):
        supervise:
          val repl = Repl()
          repl.interpret(t"val x = 40 + 2")
          repl.interpret(t"x + 1")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value == t"43"
          case _                                    => false

      test(m"a `lazy val` shows its type without being forced"):
        supervise:
          val repl = Repl()
          repl.interpret(t"var forced = false")
          repl.interpret(t"lazy val z = { forced = true; 99 }")
          repl.interpret(t"forced")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value == t"false"
          case _                                    => false

      test(m"a `def` shows its signature with the written return type, uninvoked"):
        supervise:
          Repl().interpret(t"def f(a: Int, b: String): String = b*a")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.trim == t"def f(a: Int, b: String): String"
          case _                                     => false

      test(m"a `def` with an omitted return type shows the inferred one"):
        supervise:
          Repl().interpret(t"def g(n: Int) = n + 1")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.trim == t"def g(n: Int): scala.Int"
          case _                                     => false

      test(m"/set experimental enables experimental definitions"):
        supervise:
          val repl = Repl()
          repl.interpret(t"@scala.annotation.experimental def ex = 1")
          val before = repl.interpret(t"ex")
          repl.interpret(t"/set experimental")
          (before, repl.interpret(t"ex"))
      . assert:
          case (Repl.Outcome.Rejected(_), Repl.Outcome.Ran(_, _, _, _, _)) => true
          case _                                                     => false

      test(m"completions are offered for a member prefix"):
        supervise:
          val code = t"List(1, 2, 3).ma"
          Repl().completionsAt(code, code.length).map(_.name)
      . assert(_.contains(t"map"))

      test(m"completions are offered inside a definition's right-hand side"):
        supervise:
          val code = t"def foo() = System.o"
          Repl().completionsAt(code, code.length).map(_.name)
      . assert(_.contains(t"out"))

      test(m"completions are offered inside a val's right-hand side"):
        supervise:
          val code = t"val xs = List(1, 2, 3).ma"
          Repl().completionsAt(code, code.length).map(_.name)
      . assert(_.contains(t"map"))

      test(m"completions see the session's imports for a first-token prefix"):
        supervise:
          val repl = Repl()
          repl.interpret(t"import scala.collection.mutable.ListBuffer")
          repl.completionsAt(t"ListB", 5).map(_.name)
      . assert(_.contains(t"ListBuffer"))

      test(m"completions work in import position"):
        supervise:
          val code = t"import sca"
          Repl().completionsAt(code, code.length).map(_.name)
      . assert(_.contains(t"scala"))

      test(m"slash-command lines complete against the engine's commands"):
        supervise:
          val code = t"/set ex"
          Repl().completionsAt(code, code.length).map(_.name)
      . assert(_.contains(t"/set experimental"))

      test(m"a new definition invalidates the cached member completions"):
        supervise:
          val repl = Repl()
          repl.completionsAt(t"z9.le", 5)        // z9 undefined: caches an empty member list
          repl.interpret(t"val z9 = \"hi\"")      // defining z9 must drop the stale cache
          repl.completionsAt(t"z9.le", 5).map(_.name)
      . assert(_.contains(t"length"))

      // Keyword completion is position-classified (`Repl.keywordCompletions`, no compiler).
      test(m"`val`/`var` are keyword completions at a statement start"):
        Repl.keywordCompletions(t"va", 2).map(_.name)
      . assert(_ == List(t"val", t"var"))

      test(m"`import` is a keyword completion at the start of a line"):
        Repl.keywordCompletions(t"imp", 3).map(_.name)
      . assert(_.contains(t"import"))

      test(m"no keywords are offered after a member-selection dot"):
        Repl.keywordCompletions(t"List(1).ma", 10)
      . assert(_ == Nil)

      test(m"no keyword (in particular `match`) is offered after a value with no space"):
        Repl.keywordCompletions(t"List(1)ma", 9)
      . assert(_ == Nil)

      test(m"a value followed by a space is an infix receiver"):
        Repl.infixBase(t"List(1) ma", 10)._1
      . assert(_ == t"List(1).")

      test(m"a value with no trailing space is not an infix receiver"):
        Repl.infixBase(t"List(1)ma", 9)._1
      . assert(_ == Unset)

      test(m"a name after `val` is not an infix receiver"):
        Repl.infixBase(t"val x ", 6)._1
      . assert(_ == Unset)

      test(m"a soft modifier offers a curated follow-set, not the whole definition list"):
        Repl.keywordCompletions(t"transparent ", 12).map(_.name)
      . assert(_ == List(t"inline", t"trait"))

      test(m"`inline` offers def/given/val"):
        Repl.keywordCompletions(t"inline ", 7).map(_.name)
      . assert(_ == List(t"def", t"given", t"val"))

      test(m"`with` is not offered after a type ascription `:`"):
        Repl.keywordCompletions(t"val x: w", 8)
      . assert(_ == Nil)

      test(m"`with` is offered in a template header after `extends`"):
        Repl.keywordCompletions(t"class A extends w", 17).map(_.name)
      . assert(_ == List(t"with"))

      test(m"definition keywords are not offered in expression position"):
        Repl.keywordCompletions(t"1 + va", 6)
      . assert(_ == Nil)

      test(m"`new` is offered in expression position"):
        Repl.keywordCompletions(t"1 + n", 5).map(_.name)
      . assert(_.contains(t"new"))

      test(m"`using` is offered inside a parameter list"):
        Repl.keywordCompletions(t"def f(u", 7).map(_.name)
      . assert(_ == List(t"using"))

      test(m"a method-call argument is not a parameter position"):
        Repl.keywordCompletions(t"foo(v", 5)
      . assert(_ == Nil)

      test(m"the Scala 2 `implicit` keyword is not offered"):
        Repl.keywordCompletions(t"impl", 4)
      . assert(_ == Nil)

      test(m"the dropped do-while `do` keyword is not offered"):
        Repl.keywordCompletions(t"1 + d", 5)
      . assert(_ == Nil)

      // `incomplete` decides whether Enter continues onto a new line or submits. The
      // verdict is reliable for well-formed input (the cases below); malformed, non-prefix
      // input may go either way (Shift+Enter always submits).
      test(m"a trailing operator is an incomplete continuation"):
        Repl.incomplete(t"1 +")
      . assert(_ == true)

      test(m"an unclosed brace is an incomplete continuation"):
        Repl.incomplete(t"List(1).map { x =>")
      . assert(_ == true)

      test(m"a binding with no right-hand side is an incomplete continuation"):
        Repl.incomplete(t"val x =")
      . assert(_ == true)

      test(m"a complete expression is ready to submit"):
        Repl.incomplete(t"List(1).map(_ + 1)")
      . assert(_ == false)

      test(m"a complete definition is ready to submit"):
        Repl.incomplete(t"val x = 40")
      . assert(_ == false)

      test(m"blank input is not a continuation"):
        Repl.incomplete(t"   ")
      . assert(_ == false)

      test(m"an expression result carries its value and type"):
        supervise:
          Repl().react(0, t"1 + 1")
      . assert:
          case Repl.Reply.Ran(_, value, _, tpe, _, _, _) => value == t"2" && tpe.present
          case _                                       => false

      test(m"a Unit result shows neither a value nor a type"):
        supervise:
          Repl().react(0, t"println(\"hi\")")
      . assert:
          case Repl.Reply.Ran(_, value, _, tpe, _, _, _) => value.absent && tpe.absent
          case _                                       => false

      test(m"an import produces no result value"):
        supervise:
          Repl().react(0, t"import scala.collection.mutable.*")
      . assert:
          case Repl.Reply.Ran(_, value, _, tpe, _, _, _) => value.absent && tpe.absent
          case _                                       => false

      test(m"a type error is reported as Rejected with notices"):
        supervise:
          Repl().interpret(t"val n: Int = \"forty\"")
      . assert:
          case Repl.Outcome.Rejected(notices) => notices.nonEmpty
          case _                              => false

      test(m"a runtime exception is reported as Threw"):
        supervise:
          Repl().interpret(t"throw new RuntimeException(\"boom\")")
      . assert:
          case Repl.Outcome.Threw(_, _, _) => true
          case _                           => false

      test(m"a thrown exception's reply renders its stack trace"):
        supervise:
          Repl().react(0, t"throw new RuntimeException(\"boom\")")
      . assert:
          case Repl.Reply.Threw(_, _, diagnostics, _) =>
            diagnostics.contains(t"RuntimeException") && diagnostics.contains(t"boom")
            && diagnostics.contains(t".scala")
          case _ => false

    suite(m"REPL binding tests"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"captured values and a lifted definition are usable in the REPL"):
        supervise:
          val greeting: String = "hello"
          var counter:  Int    = 5

          val repl = Repl[3.8]:
            val text  = greeting
            val count = counter
            def total: Int = text.length + count

          repl.interpret(t"println(total)")     // "hello".length + 5
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

      test(m"a lifted import is in scope for REPL lines"):
        supervise:
          // the lifted import is consumed by the macro, so it reads as unused here
          @annotation.nowarn val repl = Repl[3.8]:
            import scala.collection.mutable.ListBuffer

          repl.interpret(t"println(ListBuffer(1, 2, 3).sum)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

      test(m"a captured value persists across several lines"):
        supervise:
          val secret: Int = 42

          val repl = Repl[3.8]:
            val seed = secret

          repl.interpret(t"val doubled = seed*2")
          repl.interpret(t"println(doubled + seed)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

    suite(m"REPL result rendering"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"an expression's value is rendered via Inspectable"):
        supervise:
          Repl().interpret(t"21 * 2")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"42").or(false)
          case _                             => false

      test(m"a type/class definition renders no value"):
        supervise:
          Repl().interpret(t"class C(n: Int)")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.absent
          case _                             => false

      test(m"a result is bound to a type-derived name, usable on a later line"):
        supervise:
          val repl = Repl()
          val first = repl.interpret(t"List(1, 2, 3)")
          (first, repl.interpret(t"list.size"))
      . assert:
          case (Repl.Outcome.Ran(_, _, _, name, _), Repl.Outcome.Ran(_, value, _, _, _)) =>
            name == t"list" && value.let(_ == t"3").or(false)
          case _ => false

      test(m"a repeated result type gets a numbered name to avoid a collision"):
        supervise:
          val repl = Repl()
          repl.interpret(t"List(1)")
          repl.interpret(t"List(2)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, name, _) => name == t"list2"
          case _                                  => false

      test(m"stdout printed while a line runs is captured"):
        supervise:
          Repl().interpret(t"println(7)")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"7")
          case _                              => false

      test(m"multi-line code keeps its newlines when tokenized"):
        Repl.tokenize(t"val x = 1\nval y = 2").map(_.text).join
      . assert(_.contains(t"\n"))

    suite(m"REPL TCP server"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"a reply carries the value, type and highlighting"):
        supervise:
          val tcpPort = Port[Tcp]()
          val service = Sessions().serve(tcpPort)
          val socket  = jn.Socket("localhost", tcpPort.number)

          try exchange(socket, Repl.Request.Submit(1, t"1 + 1"))
          finally
            socket.close()
            service.stop()
      . assert:
          case Repl.Reply.Ran(_, value, _, tpe, _, _, _) =>
            value.let(_ == t"2").or(false) && tpe.let(_.contains(t"Int")).or(false)

          case _ =>
            false

      test(m"a quit request fulfils the server's quit signal"):
        supervise:
          val tcpPort  = Port[Tcp]()
          val sessions = Sessions()
          val service  = sessions.serve(tcpPort)
          val socket   = jn.Socket("localhost", tcpPort.number)

          try
            send(socket, Repl.Request.Quit(0))

            // Wait (bounded) for the quit signal rather than blocking forever.
            val runnable: Runnable = () => sessions.awaitQuit()
            val waiter = Thread(runnable)
            waiter.start()
            waiter.join(5000L)
            !waiter.isAlive
          finally
            socket.close()
            service.stop()
      . assert(_ == true)

      test(m"a message sent over a UNIX domain socket is answered"):
        supervise:
          val directory: Path on Linux = temporaryDirectory/Uuid()
          directory.create[Directory]()
          val socketPath: Text = (directory/t"repl.sock").encode
          val service      = Sessions().serve(socketPath)
          val address      = jn.UnixDomainSocketAddress.of(socketPath.s).nn
          val channel      = jnc.SocketChannel.open(address).nn

          try
            exchange
             (jnc.Channels.newInputStream(channel).nn, jnc.Channels.newOutputStream(channel).nn,
              Repl.Request.Submit(3, t"6 * 7"))
          finally
            channel.close()
            service.stop()
      . assert:
          case Repl.Reply.Ran(_, value, _, _, _, _, _) => value.let(_ == t"42").or(false)
          case _                                    => false

      test(m"sessions are independent and each has a distinct name"):
        supervise:
          val sessions = Sessions()
          val a = sessions.create()
          val b = sessions.create()
          sessions.session(a).vouch.interpret(t"val marker = 111")
          ( a != b && sessions.names.contains(a) && sessions.names.contains(b),
            sessions.session(a).vouch.interpret(t"marker"),
            sessions.session(b).vouch.interpret(t"marker") )
      . assert:
          case (true, Repl.Outcome.Ran(_, _, _, _, _), Repl.Outcome.Rejected(_)) => true
          case _                                                                  => false

      test(m"a Session request switches the connection and lists sessions"):
        supervise:
          val directory: Path on Linux = temporaryDirectory/Uuid()
          directory.create[Directory]()
          val socketPath: Text = (directory/t"repl2.sock").encode
          val sessions = Sessions()
          val service  = sessions.serve(socketPath)
          val address  = jn.UnixDomainSocketAddress.of(socketPath.s).nn
          val channel  = jnc.SocketChannel.open(address).nn
          val in  = jnc.Channels.newInputStream(channel).nn
          val out = jnc.Channels.newOutputStream(channel).nn

          try
            // Connecting auto-starts a session; querying reports it plus the list.
            exchange(in, out, Repl.Request.Session(1, t""))
          finally
            channel.close()
            service.stop()
      . assert:
          case Repl.Reply.Session(_, name, names) => name != t"" && names.contains(name)
          case _                                  => false

      test(m"a completion request returns matching completions"):
        supervise:
          val tcpPort = Port[Tcp]()
          val service = Sessions().serve(tcpPort)
          val socket  = jn.Socket("localhost", tcpPort.number)

          try exchange(socket, Repl.Request.Complete(1, t"List(1, 2, 3).m", 15))
          finally
            socket.close()
            service.stop()
      . assert:
          case Repl.Reply.Completed(_, items) => items.exists(_.name.contains(t"map"))
          case _                              => false

    suite(m"REPL block captures outside references"):
      given Scalac[3.8] = Scalac(Nil)

      test(m"a lifted def can reference a value from the enclosing scope"):
        supervise:
          val base = 100

          val repl = Repl[3.8]:
            def shifted(n: Int): Int = n + base

          repl.interpret(t"shifted(5)")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"105").or(false)
          case _                             => false

      test(m"a lifted def captures an enclosing method parameter"):
        def session(base: Int): Repl.Outcome = supervise:
          val repl = Repl[3.8]:
            def plus(n: Int): Int = n + base

          repl.interpret(t"plus(2)")

        session(40)
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"42").or(false)
          case _                             => false

      test(m"a lifted def can reference both a block binding and an outside value"):
        supervise:
          val base = 100

          val repl = Repl[3.8]:
            val offset = 5
            def total: Int = offset + base

          repl.interpret(t"total")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"105").or(false)
          case _                             => false

      test(m"a lifted def references a field of an enclosing object, tracking changes"):
        supervise:
          ReplFixture.greeting = "hi"     // length 2; then mutated to "changed" (7)
          ReplFixture.session
      . assert:
          case (Repl.Outcome.Ran(_, before, _, _, _), Repl.Outcome.Ran(_, after, _, _, _)) =>
            before.let(_ == t"2").or(false) && after.let(_ == t"7").or(false)
          case _ =>
            false

      test(m"a lifted def can write back to a host var"):
        supervise:
          var tally = 1

          val repl = Repl[3.8]:
            def bump(): Unit = tally = tally + 10

          repl.interpret(t"bump()")
          repl.interpret(t"bump()")
          tally
      . assert(_ == 21)

      test(m"a lifted import is in scope for a lifted def and for later lines"):
        supervise:
          val repl = Repl[3.8]:
            import scala.collection.mutable.ListBuffer
            def make: ListBuffer[Int] = ListBuffer(1, 2, 3)

          repl.interpret(t"make.sum")               // lifted def uses the import
          repl.interpret(t"ListBuffer(9, 9).sum")   // a later line uses it directly
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"18").or(false)
          case _                             => false

      test(m"a block-local var can be reassigned from a REPL line"):
        supervise:
          val repl = Repl[3.8]:
            var counter = 10

          repl.interpret(t"counter = counter + 5")
          repl.interpret(t"counter")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"15").or(false)
          case _                             => false
