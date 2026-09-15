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

import scala.caps

import soundness.*

import pyrocosm.{Block, Event, Inline, Token, Tone}


import classloaders.threadContextClassloader
import dysasymptotics.{linearSize, linearAccess}
import internetAccess.online
import socketBackends.javaBaseSockets
import filesystemBackends.javaBaseFilesystem
import pathInterfaces.pathOnLinux
import probates.awaitProbate
import logging.silentLogging
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading

// One framed connection to a REPL server, as the client speaks it: each request and reply is a
// record framed by its four-byte big-endian length (`obligatory.LengthPrefix`) carrying a BinTEL
// body. The reply frames are pulled from one iterator held for the connection's lifetime, so a
// test may exchange several requests over the same connection.
// A capability: the connection and its reply iterator are live resources, so a `Wire` carries
// them in its capture set rather than laundering them away.
class Wire(duplex: Duplex) extends caps.ExclusiveCapability:
  private lazy val replies: Iterator[Data]^ = duplex.source.chunks.frames[LengthPrefix]

  def send(request: Repl.Request): Unit =
    duplex.send(zephyrine.Stream(LengthPrefix.encode(request.bintel)))

  def exchange(request: Repl.Request): Repl.Reply =
    send(request)
    // `hasNext` is what pulls the next frame from the connection; `next()` hands back what it
    // read.
    if !replies.hasNext then panic(m"the server closed the connection without replying")
    Bintel.read[Repl.Reply](replies.next())

  def close(): Unit = safely(duplex.close())

object Wire:
  // The connection is made through the `Connectable` instance directly rather than the `duplex`
  // loan, so it outlives a single block and the tests keep their `try`/`finally` shape.
  def tcp(port: Port over Tcp)(using Online): Wire^ =
    val endpoint: Endpoint[Tcp.Port] = t"localhost".as[Hostname] on port
    Wire(caps.unsafe.unsafeAssumePure(Connectable.tcpEndpoint).connect(endpoint, Unset))

  def domain(path: Text): Wire^ =
    Wire(Connectable.domainSocket.connect(DomainSocket(path), Unset))

// Every `Repl` and `Sessions` a test makes, registered as it is made (`Opened(Opened(Repl()))`) so that
// `isolated` can close it when the test's block ends. Each keeps a warm compiler alive until it is
// closed (see `Repl#close`), so a suite that merely dropped them would exhaust any heap.
object Opened:
  private val opened: java.util.ArrayDeque[Repl[?] | Sessions[?]] = java.util.ArrayDeque()

  def apply[value <: Repl[?] | Sessions[?]](value: value): value =
    opened.synchronized(opened.push(value))
    value

  def depth: Int = opened.synchronized(opened.size)

  // Closes, newest first, everything opened since `depth` was read.
  def closeTo(depth: Int): Unit =
    val next: Optional[Repl[?] | Sessions[?]] =
      opened.synchronized(if opened.size > depth then Optional(opened.pop()) else Unset)

    next.let: value =>
      value match
        case repl: Repl[?]         => repl.close()
        case sessions: Sessions[?] => sessions.close()

      closeTo(depth)

// `supervise`, closing whatever the block opened (see `Opened`) once it has finished.
def isolated[result](block: Monitor ?=> result)(using Threading, Codepoint): result =
  val depth = Opened.depth
  try supervise(block) finally Opened.closeTo(depth)

// Mimics a standard-REPL session: `size` references `greeting`, a field of the
// enclosing object, by simple name (as `var name = …` would be in the Scala REPL).
// `session` reads `size` once, mutates `greeting`, then reads it again — the
// second read should track the change (a live binding), not the captured value.
object ReplFixture:
  var greeting: String = "hello"

  def session(using Scalac[3.9, Universe.Classfile], Classloader, TemporaryDirectory, Monitor, System, Probate)
  :   (Repl.Outcome, Repl.Outcome) =
    val repl = Repl[3.9]:
      def size: Int = greeting.length
    Opened(repl)

    val before: Repl.Outcome = repl.interpret(t"size")
    greeting = "changed"
    (before, repl.interpret(t"size"))

object Tests extends Suite(m"Flame Tests"):
  def run(): Unit =
    suite(m"REPL tests"):
      given Scalac[3.9, Universe.Classfile] = Scalac(Nil)

      test(m"a definition is visible on a later line"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"val x = 40")
          repl.interpret(t"println(x + 2)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

      test(m"an import persists to a later line"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"import scala.collection.mutable.ListBuffer")
          repl.interpret(t"ListBuffer(1, 2, 3)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

      test(m"an import of a session definition persists to a later line"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"object Foo { object Bar { def hi = 42 } }")
          repl.interpret(t"import Foo.Bar")
          repl.interpret(t"Bar.hi")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                               => false

      test(m"a wildcard import of a session definition persists to a later line"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"object Foo { val n = 7 }")
          repl.interpret(t"import Foo.*")
          repl.interpret(t"n")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                               => false

      test(m"/unimport removes an import so it is no longer in scope"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"import scala.collection.mutable.ListBuffer")
          val before = repl.interpret(t"ListBuffer(1, 2, 3)")
          repl.interpret(t"/unimport scala.collection.mutable.ListBuffer")
          (before, repl.interpret(t"ListBuffer(1, 2, 3)"))
      . assert:
          case (Repl.Outcome.Ran(_, _, _, _, _), Repl.Outcome.Rejected(_)) => true
          case _                                                     => false

      test(m"/unimport with no argument lists the removable imports"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"import scala.collection.mutable.ListBuffer")
          repl.interpret(t"/unimport")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"scala.collection.mutable.ListBuffer")
          case _                              => false

      test(m"/unimport reports when no import matches the given tokens"):
        isolated:
          Opened(Repl()).interpret(t"/unimport nonsense.does.not.exist")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"No matching import")
          case _                              => false

      test(m"/classpath lists the current classpath"):
        isolated:
          Opened(Repl()).interpret(t"/classpath")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"Classpath (")
          case _                              => false

      // The scope an UNFINISHED line has opened (see `ScopeInspector`): what its next statement
      // would see. Rendered by name (where the user could write one) and type.
      def scopeText(bindings: List[Repl.ScopeBinding]): Text =
        bindings.map { b => t"${b.name.or(t"_")}: ${b.tpe}" }.join(t", ")

      test(m"an unfinished `unsafely:` block reports the contextual values it has introduced"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"import soundness.*")
          scopeText(repl.scopeAt(t"unsafely:\n  "))
      . assert { scope => scope.contains(t"Unsafe") && scope.contains(t"CanThrow[Exception]") }

      test(m"an unfinished lambda reports its parameter by name and type"):
        isolated:
          Opened(Repl()).scopeAt(t"scala.List(1, 2).map { x =>\n  ")
      . assert:
          case Repl.ScopeBinding(name, tpe, false, _) :: Nil => name == t"x" && tpe == t"Int"
          case _                                          => false

      test(m"nested scopes report the innermost block's contextual values first"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"import soundness.*")
          repl.scopeAt(t"unsafely:\n  safely:\n    ").map(_.tpe)
      . assert { types => types.prim.let(_.contains(t"Diagnostics")).or(false) && types.exists(_.contains(t"Unsafe")) }

      test(m"a block's own definitions are in scope for its next statement"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"import soundness.*")
          scopeText(repl.scopeAt(t"unsafely:\n  val n = 42\n  "))
      . assert { scope => scope.contains(t"n: Int") && scope.contains(t"Unsafe") }

      test(m"a complete single line opens no scope"):
        isolated:
          Opened(Repl()).scopeAt(t"1 + 2")
      . assert(_ == Nil)

      // `Out.println` needs a `Stdio`; the REPL supplies one ambiently (see `ReplStdio`), writing
      // to the run's captured stdout exactly as `println` does. Experimental mode, as any use of a
      // Soundness definition needs.
      test(m"Out.println prints without a Stdio import"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"/set experimental")
          repl.react(0, t"import soundness.*")
          repl.interpret(t"Out.println(t\"ambient stdio\")")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"ambient stdio")
          case _                                    => false

      // The ambient one is a `Stdio.Provider`, found only when no `Stdio` is in lexical scope, so
      // a user's own import is never ambiguous with it and simply takes over.
      test(m"a user-imported Stdio takes precedence over the ambient one"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"/set experimental")
          repl.react(0, t"import soundness.*")
          repl.react(1, t"import stdios.muteStdio")
          repl.interpret(t"Out.println(t\"silenced\")")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => !output.contains(t"silenced")
          case _                                    => false

      test(m"a user-defined Stdio on an earlier line takes precedence over the ambient one"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"/set experimental")
          repl.react(0, t"import soundness.*")
          repl.react(1, t"given quiet: Stdio = stdios.muteStdio")
          repl.interpret(t"Out.println(t\"silenced\")")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => !output.contains(t"silenced")
          case _                                    => false

      def diagnostics(reply: Repl.Reply): Text = reply match
        case r: Repl.Reply.Rejected => SemanticRender.stripAnsi(r.diagnostics)
        case _                      => t""

      test(m"a type-error diagnostic renders types via stenography, abbreviated to `Int`"):
        isolated:
          diagnostics(Opened(Repl()).react(0, t"val n: Int = \"hello\""))
      . assert { diag => diag.contains(t"Int") && !diag.contains(t"scala.Int") }

      test(m"a type-error diagnostic abbreviates a session-imported type to its simple name"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"import scala.collection.mutable.ListBuffer")
          diagnostics(repl.react(1, t"val b: ListBuffer[Int] = \"no\""))
      . assert { diag => diag.contains(t"ListBuffer[Int]") && !diag.contains(t"mutable.ListBuffer") }

      // `soundness.Json` is `export jacinta.Json`, so the compiler names the type `jacinta.Json`; the
      // reifier resolves the prelude's exports so it abbreviates as the user wrote it.
      test(m"a type reached through a prelude's export abbreviates to its leaf name"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"import soundness.*")
          diagnostics(repl.react(1, t"val j: Json = 1"))
      . assert { diag => diag.contains(t"Json") && !diag.contains(t"jacinta.Json") }

      // Soundness's missing-given advice (frontier's `explainMissingContext`, reached through
      // `import soundness.*`) replaces the compiler's own "no given instance" message only when
      // the line is compiled with the fork's `-Zdiagnostic-givens`, and only when frontier is on
      // the session classpath — so this guards both.
      test(m"a missing given is explained with Soundness's advice, naming candidate givens"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"import soundness.*")
          diagnostics(repl.react(1, t"summon[rudiments.DecimalConverter]"))
      . assert { diag => diag.contains(t"decimalConverters.javaDecimalConverter") }

      // The catch-all must not intrude on ordinary code: implicit searches the compiler retries
      // after inference (`join`'s element type, `flatMap`'s reshaping) must still resolve.
      // ASPIRATIONAL: with frontier on the classpath, a bare `join` (whose `element`/`textual`
      // type parameters are fixed by the `Joinable.Source` given it finds) resolves that given at
      // `Any` and then fails on `Any is Textual` — with or without `-Zdiagnostic-givens`. This is
      // frontier's catch-all satisfying a search made before inference has run; reported upstream.
      test(m"a bare join over mapped elements still resolves with the advice in scope"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"/set experimental")
          repl.react(1, t"import soundness.*")
          repl.interpret(t"List(t\"a\", t\"b\").map(_.upper).join")
      . aspire:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_.contains(t"AB")).or(false)
          case _                                   => false

      test(m"flatMap still resolves with the advice in scope"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"/set experimental")
          repl.react(1, t"import soundness.*")
          repl.interpret(t"List(1, 2).flatMap { n => List(n, n) }")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_.contains(t"List(1, 1, 2, 2)")).or(false)
          case _                                   => false

      // The wrapper objects (`rs$line$N`, in the empty package) are session bookkeeping: no rendering
      // the user sees may name them — not the result line's type, a `def`'s signature, a diagnostic,
      // nor a completion signature.
      test(m"a session-defined type renders as the user wrote it, without the wrapper object"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"object Foo { object Bar }")
          repl.interpret(t"Foo.Bar")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, tpe) => tpe.let(_.text) == t"Foo.Bar.type"
          case _                                 => false

      test(m"an object's result binds a name from the object, not from the `.type` suffix"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"object Foo { object Bar }")
          repl.interpret(t"Foo.Bar")
      . assert:
          case Repl.Outcome.Ran(_, _, _, name, _) => name == t"bar"
          case _                                  => false

      // A refinement, like a type argument or a capture set, decorates a base type without changing
      // which type it is, so the binding is still named after what it refines.
      test(m"a refined type binds a name from the type it refines"):
        isolated:
          Opened(Repl()).interpret(t"\"\".asInstanceOf[String { type Foo = Any }]")
      . assert:
          case Repl.Outcome.Ran(_, _, _, name, _) => name == t"string"
          case _                                  => false

      // An infix type ALIAS does not name a type — Soundness's `X is Y` expands to
      // `Y { type Self = X }`, so the binding is named after the trait on the right, not after `is`.
      test(m"an infix `is` type binds a name from the trait, not from the alias"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"infix type is[S, T] = T { type Self = S }")
          repl.interpret(t"trait Barable { type Self }")
          repl.interpret(t"null.asInstanceOf[Int is Barable]")
      . assert:
          case Repl.Outcome.Ran(_, _, _, name, _) => name == t"barable"
          case _                                  => false

      // `by`/`in` qualify what is already to their left, so they step left and reach the same `is`.
      test(m"a qualified infix type binds a name from the trait it qualifies"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"infix type is[S, T] = T { type Self = S }")
          repl.interpret(t"infix type by[T, R] = T { type Operand = R }")
          repl.interpret(t"trait Barable { type Self; type Operand }")
          repl.interpret(t"null.asInstanceOf[Int is Barable by String]")
      . assert:
          case Repl.Outcome.Ran(_, _, _, name, _) => name == t"barable"
          case _                                  => false

      // An expression line is bound as a `final val`, so a constant keeps its singleton type rather
      // than being widened by the REPL's own binding — `42` is a `42`, not an `Int`.
      test(m"an integer literal reports its singleton type"):
        isolated:
          Opened(Repl()).interpret(t"42")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, tpe) => tpe.let(_.text) == t"42"
          case _                                 => false

      test(m"a string literal reports its singleton type"):
        isolated:
          Opened(Repl()).interpret(t"\"hello\"")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, tpe) => tpe.let(_.text) == t"\"hello\""
          case _                                 => false

      // A singleton also reports the type it widens to, which the front-ends show dimmed after a
      // `<:` — the result IS a `42`, and a `42` is an `Int`.
      test(m"a singleton type carries the base type it widens to"):
        isolated:
          Opened(Repl()).interpret(t"42")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, tpe) => tpe.let(_.base) == t"Int"
          case _                                 => false

      test(m"a non-singleton type carries no base type"):
        isolated:
          Opened(Repl()).interpret(t"List(1, 2)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, tpe) => tpe.let(_.base).absent
          case _                                 => false

      // …but the NAME comes from the widened type, since `42` is not one an identifier can be made
      // from — so precision in the type costs nothing in the naming.
      test(m"a singleton-typed result is still named from its widened base type"):
        isolated:
          Opened(Repl()).interpret(t"42")
      . assert:
          case Repl.Outcome.Ran(_, _, _, name, _) => name == t"int"
          case _                                  => false

      // The REPL reports only the widening the USER's code performs: an ordinary `val` widens, so
      // this stays `Int` — it is the probe's own widening that was misinformation.
      test(m"a plain `val` of a constant still reports the widened type"):
        isolated:
          Opened(Repl()).interpret(t"val n = 40 + 2")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, tpe) => tpe.let(_.text) == t"Int"
          case _                                 => false

      test(m"a `val` annotated with a singleton type reports that type"):
        isolated:
          Opened(Repl()).interpret(t"val n: 42 = 42")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, tpe) => tpe.let(_.text) == t"42"
          case _                                 => false

      test(m"a `def` returning a session-defined type shows no wrapper object in its signature"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"object Foo { class Baz }")
          repl.interpret(t"def make = new Foo.Baz")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) =>
            output.contains(t"def make: Foo.Baz") && !output.contains(t"rs$$line$$")
          case _ => false

      test(m"a diagnostic naming a session-defined type shows no wrapper object"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"object Foo { object Bar }")
          diagnostics(repl.react(1, t"val n: Int = Foo.Bar"))
      . assert { diag => diag.contains(t"Foo.Bar.type") && !diag.contains(t"rs$$line$$") }

      test(m"a completion signature shows no wrapper object"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"object Foo { object Bar { def hi = 42 }; def make = Bar }")
          repl.completionsAt(t"Foo.mak", 7)
      . assert: items =>
          items.exists(_.name == t"make") && items.all(!_.signature.contains(t"rs$$line$$"))

      test(m"/classload reports a nonexistent path"):
        isolated:
          Opened(Repl()).interpret(t"/classload /no/such/directory/lib.jar")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"No such file or directory")
          case _                              => false

      test(m"/classload adds an entry that /classpath then shows"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"/classload /usr")
          repl.interpret(t"/classpath")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"/usr")
          case _                              => false

      test(m"/classload tab-completion lists filesystem entries as whole-line candidates"):
        isolated:
          val partial: Text = t"/classload /"
          Opened(Repl()).completionsAt(partial, partial.length)
      . assert: items =>
          !items.nil
          && items.all(_.name.starts(t"/classload /"))  // whole-line `/classload <path>` candidates
          && items.exists(_.name.ends(t"/"))            // at least one directory (e.g. /usr/, /bin/)

      test(m"a keyword tab-completion ends with a space"):
        isolated:
          Opened(Repl()).completionsAt(t"va", 2)
      . assert: items =>
          val keywords = items.filter(_.kind == t"keyword")
          keywords.exists(_.name == t"val ") && keywords.all(_.name.ends(t" "))

      test(m"a given declared on one line is in scope on a later line"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"given Int = 42")
          repl.interpret(t"summon[Int]")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value == t"42"
          case _                             => false

      test(m"a persisted given resolves a later `using` parameter"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"given Int = 7")
          repl.interpret(t"def double(using n: Int) = n*2")
          repl.interpret(t"double")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value == t"14"
          case _                             => false

      test(m"a `val` definition shows its name, value and type"):
        isolated:
          Opened(Repl()).interpret(t"val x = 40 + 2")
      . assert:
          case Repl.Outcome.Ran(_, value, _, name, tpe) =>
            value == t"42" && name == t"x" && tpe.let(_.text) == t"Int"
          case _ => false

      // `List(1, 2, 3)` in a REPL line is the STDLIB list (the REPL compiles user code with the
      // ordinary predef), which has no `Inspectable` instance since Soundness #1693 moved the
      // collection instances onto the prelude's opaque types — so it renders through
      // `InspectRender`'s `toString` fallback, as `List(1, 2, 3)` rather than the old `[1, 2, 3]`.
      test(m"a `var` definition shows its name, value and type"):
        isolated:
          Opened(Repl()).interpret(t"var y = List(1, 2, 3)")
      . assert:
          case Repl.Outcome.Ran(_, value, _, name, _) => value == t"List(1, 2, 3)" && name == t"y"
          case _                                       => false

      test(m"a `val` definition still persists for later lines"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"val x = 40 + 2")
          repl.interpret(t"x + 1")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value == t"43"
          case _                                    => false

      test(m"a `lazy val` shows its type without being forced"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"var forced = false")
          repl.interpret(t"lazy val z = { forced = true; 99 }")
          repl.interpret(t"forced")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value == t"false"
          case _                                    => false

      test(m"a `def` shows its signature with the written return type, uninvoked"):
        isolated:
          Opened(Repl()).interpret(t"def f(a: Int, b: String): String = b*a")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.trim == t"def f(a: Int, b: String): String"
          case _                                     => false

      test(m"a `def` with an omitted return type shows the inferred one"):
        isolated:
          Opened(Repl()).interpret(t"def g(n: Int) = n + 1")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.trim == t"def g(n: Int): Int"
          case _                                     => false

      test(m"/set experimental enables experimental definitions"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"@scala.annotation.experimental def ex = 1")
          val before = repl.interpret(t"ex")
          repl.interpret(t"/set experimental")
          (before, repl.interpret(t"ex"))
      . assert:
          case (Repl.Outcome.Rejected(_), Repl.Outcome.Ran(_, _, _, _, _)) => true
          case _                                                     => false

      test(m"completions are offered for a member prefix"):
        isolated:
          val code = t"List(1, 2, 3).ma"
          Opened(Repl()).completionsAt(code, code.length).map(_.name)
      . assert(_.has(t"map"))

      test(m"completions are offered inside a definition's right-hand side"):
        isolated:
          val code = t"def foo() = System.o"
          Opened(Repl()).completionsAt(code, code.length).map(_.name)
      . assert(_.has(t"out"))

      test(m"completions are offered inside a val's right-hand side"):
        isolated:
          val code = t"val xs = List(1, 2, 3).ma"
          Opened(Repl()).completionsAt(code, code.length).map(_.name)
      . assert(_.has(t"map"))

      test(m"completions see the session's imports for a first-token prefix"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"import scala.collection.mutable.ListBuffer")
          repl.completionsAt(t"ListB", 5).map(_.name)
      . assert(_.has(t"ListBuffer"))

      test(m"completions work in import position"):
        isolated:
          val code = t"import sca"
          Opened(Repl()).completionsAt(code, code.length).map(_.name)
      . assert(_.has(t"scala"))

      test(m"slash-command lines complete against the engine's commands"):
        isolated:
          val code = t"/set ex"
          Opened(Repl()).completionsAt(code, code.length).map(_.name)
      . assert(_.has(t"/set experimental"))

      test(m"/s offers /set once, not one entry per setting"):
        isolated:
          val code = t"/s"
          Opened(Repl()).completionsAt(code, code.length).map(_.name)
      . assert { names => names.has(t"/set ") && !names.has(t"/set async") }

      test(m"/set followed by a space offers its subcommands as settings"):
        isolated:
          val code = t"/set "
          Opened(Repl()).completionsAt(code, code.length)
      . assert: items =>
          items.map(_.name).has(t"/set async") && items.map(_.name).has(t"/set experimental")
          && items.all(_.kind == t"setting")

      test(m"a new definition invalidates the cached member completions"):
        isolated:
          val repl = Opened(Repl())
          repl.completionsAt(t"z9.le", 5)        // z9 undefined: caches an empty member list
          repl.interpret(t"val z9 = \"hi\"")      // defining z9 must drop the stale cache
          repl.completionsAt(t"z9.le", 5).map(_.name)
      . assert(_.has(t"length"))

      // Keyword completion is position-classified by prophesy's corpus-derived pattern tree
      // (`Repl.keywordCompletions` via `Lexis.context`/`ScalaKeywords.pattern` — no compiler).
      test(m"`val`/`var` are keyword completions at a statement start"):
        Repl.keywordCompletions(t"va", 2)._1.map(_.name)
      . assert(_ == List(t"val", t"var"))

      test(m"`import` is a keyword completion at the start of a line"):
        Repl.keywordCompletions(t"imp", 3)._1.map(_.name)
      . assert(_.has(t"import"))

      test(m"`match` is offered after a member-selection dot (`expr.match` is valid Scala 3)"):
        Repl.keywordCompletions(t"List(1).ma", 10)._1.map(_.name)
      . assert(_ == List(t"match"))

      test(m"`match` is offered directly after a value (`List(1)match` tokenizes as a keyword)"):
        Repl.keywordCompletions(t"List(1)ma", 9)._1.map(_.name)
      . assert(_ == List(t"match"))

      test(m"a value followed by a space is an infix receiver"):
        Repl.infixBase(t"List(1) ma", 10)._1
      . assert(_ == t"List(1).")

      test(m"a value with no trailing space is not an infix receiver"):
        Repl.infixBase(t"List(1)ma", 9)._1
      . assert(_ == Unset)

      test(m"a name after `val` is not an infix receiver"):
        Repl.infixBase(t"val x ", 6)._1
      . assert(_ == Unset)

      test(m"a soft modifier offers a follow-set, not the whole definition list"):
        Repl.keywordCompletions(t"transparent ", 12)._1.map(_.name)
      . assert { names => names.has(t"inline") && names.has(t"trait") && !names.has(t"import") }

      test(m"`inline` offers def/given/val (and inline-if/match)"):
        Repl.keywordCompletions(t"inline ", 7)._1.map(_.name)
      . assert { names => List(t"def", t"given", t"val", t"if", t"match").all { (word: Text) => names.has(word) } }

      test(m"`with` is not offered after a type ascription `:`"):
        Repl.keywordCompletions(t"val x: w", 8)._1
      . assert(_ == Nil)

      test(m"`with` is offered in a template header after the parent type"):
        Repl.keywordCompletions(t"class A extends B w", 19)._1.map(_.name)
      . assert(_ == List(t"with"))

      test(m"definition keywords are not offered in expression position"):
        Repl.keywordCompletions(t"1 + va", 6)._1
      . assert(_ == Nil)

      test(m"`new` is offered at a statement start"):
        Repl.keywordCompletions(t"n", 1)._1.map(_.name)
      . assert(_.has(t"new"))

      test(m"`using` is offered inside a parameter list"):
        Repl.keywordCompletions(t"def f(u", 7)._1.map(_.name)
      . assert(_ == List(t"using"))

      test(m"a parameter list expects a fresh name, suppressing member completions"):
        Repl.keywordCompletions(t"def f(u", 7)._2
      . assert(_ == true)

      test(m"a binding position (`val x`) expects a fresh name"):
        Repl.keywordCompletions(t"val x", 5)._2
      . assert(_ == true)

      test(m"a method-call argument is not a parameter position"):
        Repl.keywordCompletions(t"foo(v", 5)._1
      . assert(_ == Nil)

      test(m"the Scala 2 `implicit` keyword is not offered"):
        Repl.keywordCompletions(t"impl", 4)._1
      . assert(_ == Nil)

      test(m"`do` is offered after a `while` condition (Scala 3 while-do)"):
        Repl.keywordCompletions(t"while true d", 12)._1.map(_.name)
      . assert(_ == List(t"do"))

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
        isolated:
          Opened(Repl()).react(0, t"1 + 1")
      . assert:
          case Repl.Reply.Ran(_, value, _, tpe, _, _, _, _, _) => value == t"2" && tpe.present
          case _                                       => false

      test(m"a Unit result shows neither a value nor a type"):
        isolated:
          Opened(Repl()).react(0, t"println(\"hi\")")
      . assert:
          case Repl.Reply.Ran(_, value, _, tpe, _, _, _, _, _) => value.absent && tpe.absent
          case _                                       => false

      test(m"an import produces no result value"):
        isolated:
          Opened(Repl()).react(0, t"import scala.collection.mutable.*")
      . assert:
          case Repl.Reply.Ran(_, value, _, tpe, _, _, _, _, _) => value.absent && tpe.absent
          case _                                       => false

      test(m"a type error is reported as Rejected with notices"):
        isolated:
          Opened(Repl()).interpret(t"val n: Int = \"forty\"")
      . assert:
          case Repl.Outcome.Rejected(notices) => !notices.nil
          case _                              => false

      test(m"a runtime exception is reported as Threw"):
        isolated:
          Opened(Repl()).interpret(t"throw new RuntimeException(\"boom\")")
      . assert:
          case Repl.Outcome.Threw(_, _, _) => true
          case _                           => false

      test(m"a thrown exception's reply renders its stack trace"):
        isolated:
          Opened(Repl()).react(0, t"throw new RuntimeException(\"boom\")")
      . assert:
          case Repl.Reply.Threw(_, _, diagnostics, _, _, _) =>
            diagnostics.contains(t"RuntimeException") && diagnostics.contains(t"boom")
            && diagnostics.contains(t".scala")
          case _ => false

    suite(m"REPL binding tests"):
      given Scalac[3.9, Universe.Classfile] = Scalac(Nil)

      test(m"captured values and a lifted definition are usable in the REPL"):
        isolated:
          val greeting: String = "hello"
          var counter:  Int    = 5

          val repl = Repl[3.9]:
            val text  = greeting
            val count = counter
            def total: Int = text.length + count
          Opened(repl)

          repl.interpret(t"println(total)")     // "hello".length + 5
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

      test(m"a lifted import is in scope for REPL lines"):
        isolated:
          // the lifted import is consumed by the macro, so it reads as unused here
          @scala.annotation.nowarn val repl = Repl[3.9]:
            import scala.collection.mutable.ListBuffer
          Opened(repl)

          repl.interpret(t"println(ListBuffer(1, 2, 3).sum)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

      test(m"a captured value persists across several lines"):
        isolated:
          val secret: Int = 42

          val repl = Repl[3.9]:
            val seed = secret
          Opened(repl)

          repl.interpret(t"val doubled = seed*2")
          repl.interpret(t"println(doubled + seed)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, _, _) => true
          case _                         => false

    suite(m"REPL result rendering"):
      given Scalac[3.9, Universe.Classfile] = Scalac(Nil)

      test(m"an expression's value is rendered via Inspectable"):
        isolated:
          Opened(Repl()).interpret(t"21 * 2")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"42").or(false)
          case _                             => false

      test(m"a type/class definition renders no value"):
        isolated:
          Opened(Repl()).interpret(t"class C(n: Int)")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.absent
          case _                             => false

      test(m"a result is bound to a type-derived name, usable on a later line"):
        isolated:
          val repl = Opened(Repl())
          val first = repl.interpret(t"List(1, 2, 3)")
          (first, repl.interpret(t"list.size"))
      . assert:
          case (Repl.Outcome.Ran(_, _, _, name, _), Repl.Outcome.Ran(_, value, _, _, _)) =>
            name == t"list" && value.let(_ == t"3").or(false)
          case _ => false

      test(m"a repeated result type gets a numbered name to avoid a collision"):
        isolated:
          val repl = Opened(Repl())
          repl.interpret(t"List(1)")
          repl.interpret(t"List(2)")
      . assert:
          case Repl.Outcome.Ran(_, _, _, name, _) => name == t"list2"
          case _                                  => false

      test(m"stdout printed while a line runs is captured"):
        isolated:
          Opened(Repl()).interpret(t"println(7)")
      . assert:
          case Repl.Outcome.Ran(_, _, output, _, _) => output.contains(t"7")
          case _                              => false

      test(m"multi-line code keeps its newlines when tokenized"):
        Repl.tokenize(t"val x = 1\nval y = 2").map(_.text).join
      . assert(_.contains(t"\n"))

    // `classify` decides whether a line is handed to the compiler or (in a front-end) to an
    // assistant. Everything ambiguous defaults to `Code`, so misclassifying code as language is
    // the costly direction: the code corpus therefore leans on typo'd and malformed lines, and on
    // the Soundness infix idioms built from English prepositions. No compiler session is needed —
    // classification uses only the parser.
    suite(m"code/language classification"):
      def misclassified(expected: Repl.Verdict, samples: List[Text]): List[Text] =
        samples.filter { code => Repl.classify(code) != expected }

      test(m"well-formed Scala classifies as code"):
        misclassified(Repl.Verdict.Code, List
          ( t"val x = 40 + 2",
            t"var counter = 0",
            t"def double(n: Int): Int = n * 2",
            t"def factorial(n: Int): Int = if n <= 1 then 1 else n * factorial(n - 1)",
            t"List(1, 2, 3).map(_ + 1)",
            t"xs.foldLeft(0)(_ + _)",
            t"println(\"hello world\")",
            t"import scala.collection.mutable.ListBuffer",
            t"case class Point(x: Int, y: Int)",
            t"object Config { val port = 8080 }",
            t"trait Monoid[T] { def empty: T }",
            t"for i <- 1 to 10 yield i * i",
            t"val s = \"the quick brown fox jumps over the lazy dog\"",
            t"List('a', 'b', 'c')",
            t"enum Colour { case Red, Green, Blue }",
            t"given Ordering[Int] = Ordering.Int.reverse",
            t"extension (s: String) def shout: String = s.toUpperCase",
            t"lazy val heavy = compute()",
            t"throw new RuntimeException(\"boom\")",
            t"while x < 10 do x += 1",
            t"count += 1",
            t"x match { case Some(y) => y; case None => 0 }" ))
      . assert(_ == List())

      test(m"Soundness infix idioms classify as code"):
        misclassified(Repl.Verdict.Code, List
          ( t"x is Positive",
            t"2 of 5",
            t"path in directory",
            t"sorted by name",
            t"value or default",
            t"input as Text",
            t"x max y min z" ))
      . assert(_ == List())

      test(m"typo'd or malformed Scala still classifies as code"):
        misclassified(Repl.Verdict.Code, List
          ( t"val x = 5 pritnln(x)",
            t"prinltn(\"hello\")",
            t"List(1, 2, 3).mpa(_ + 1)",
            t"if x then y esle z",
            t"def f(x: Int = x * 2",
            t"val n: Int = \"forty\"",
            t"impotr scala.collection",
            t"List(1, 2, 3) foldLeft 0 (_ + _)" ))
      . assert(_ == List())

      test(m"natural-language input classifies as language"):
        misclassified(Repl.Verdict.Language, List
          ( t"what is the type of x",
            t"how do I reverse a list",
            t"please show me the last result",
            t"why does this fail to compile",
            t"can you explain the previous error",
            t"what does this method do",
            t"how are you",
            t"what went wrong",
            t"show me all the methods on a list",
            t"is there a way to undo the last import",
            t"why is this list empty",
            t"please write a function that reverses a string",
            t"what's the difference between a list and a vector",
            t"how can I make this compile",
            t"don't evaluate this",
            t"what time is it",
            t"explain the last error message",
            t"why is x not defined",
            t"what does the compiler mean by this",
            t"how much memory does this session use" ))
      . assert(_ == List())

      test(m"short or ambiguous input defaults to code"):
        misclassified(Repl.Verdict.Code, List
          ( t"help",
            t"exit",
            t"list",
            t"thanks",
            t"hello",
            t"x",
            t"res1",
            t"why?",
            t"1 + 1",
            t"val x =" ))
      . assert(_ == List())

      test(m"a leading space forces a prose-shaped line to classify as code"):
        Repl.classify(t" what is the type of x")
      . assert(_ == Repl.Verdict.Code)

      test(m"a slash-command line classifies as code"):
        Repl.classify(t"/set experimental")
      . assert(_ == Repl.Verdict.Code)

    // The client derives one `--flag` per setting from `Repl.settings`, so the two cannot drift; the
    // flags the user asked for by name are the ones this list must keep providing.
    suite(m"settings flags"):
      // Asserted as a SUPERSET, so that adding a setting is not a test failure — only LOSING one is.
      // (Written the other way round, this failed the moment `jsr45` was added, which is exactly the
      // change it should have been indifferent to.)
      test(m"the documented settings are all addressable by name"):
        Repl.settings.map(_.name).to[Set]
      . assert: (names: Set[Text]) =>
          List(t"experimental", t"explain", t"explicit-nulls", t"deprecation", t"feature",
              t"new-syntax", t"postfixOps", t"implicitConversions", t"reflectiveCalls", t"dynamics",
              t"existentials", t"strictEquality", t"adhocExtensions", t"unsafeNulls",
              t"captureChecking", t"saferExceptions", t"pureFunctions", t"namedTuples", t"modularity",
              t"betterFors", t"erasedDefinitions", t"genericNumberLiterals")
          . all { (name: Text) => names.has(name) }

      test(m"`experimental` and `feature` are compiler settings, so `--experimental`/`--feature` map to /set"):
        Repl.settings.filter { s => s.name == t"experimental" || s.name == t"feature" }
         . map(_.kind)
      . assert(_ == List(Repl.Kind.Set, Repl.Kind.Set))

      test(m"a setting name is always a usable flag name"):
        Repl.settings.map(_.name).filter: name =>
          name == t"" || name.s.exists { char => char.isWhitespace || char == '=' }
      . assert(_ == List())

      // `-Xjsr45` is what lets `StackTraceRender`'s resolver expand a REPL line's inline chain, and
      // it is a compiler option, so it must be a `/set` (not a `/language`) setting.
      test(m"`jsr45` is a compiler setting"):
        Repl.settings.filter(_.name == t"jsr45").map(_.kind)
      . assert(_ == List(Repl.Kind.Set))

      test(m"no setting name collides with an existing flag"):
        Repl.settings.map(_.name).filter: name =>
          Set(t"port", t"host", t"session", t"set", t"language", t"basic").has(name)
      . assert(_ == List())

    suite(m"Exhibiting replies"):
      given Scalac[3.9, Universe.Classfile] = Scalac(Nil)

      // Every piece of text in a run of blocks, for checks that do not care about structure.
      def texts(blocks: List[Block]): Text =
        def inline(node: Inline): Text = node match
          case Inline.Textual(text)   => text
          case Inline.Code(_, tokens) => tokens.map(_.text).join
          case Inline.Phrase(content) => content.map(inline).join
          case Inline.Emphasis(content) => content.map(inline).join
          case Inline.Toned(_, content) => content.map(inline).join
          case Inline.Figure(value, _)  => value.toString.tt
          case _                      => t""

        def block(node: Block): Text = node match
          case Block.Paragraph(content)    => content.map(inline).join
          case Block.Group(content)        => texts(content)
          case Block.Notice(_, _, content) => texts(content)
          case Block.Output(text, _)       => text
          case Block.Code(_, lines, _)     => lines.map(_.tokens.map(_.text).join).join(t"\n")
          case Block.Table(_, rows, _)     => rows.map(_.cells.map(_.content.map(inline).join).join(t" ")).join(t"\n")
          case _                           => t""

        blocks.map(block).join(t"\n")

      def exhibiting(using Monitor, System, Probate): Repl[3.9] =
        Opened(Repl.make[3.9](Repl.Prelude.empty, Repl.Rendering.Exhibit(false)))

      test(m"a result is exhibited as blocks, with no text rendering"):
        isolated(exhibiting.react(0, t"val n = 1 + 1"))
      . assert:
          case Repl.Reply.Ran(_, value, _, _, _, diagnostics, _, _, blocks) =>
            val text = texts(Blocks.decode(blocks))
            value.absent && diagnostics == t"" && text.contains(t"n") && text.contains(t"2") && text.contains(t"Int")
          case _ => false

      test(m"captured output becomes output blocks by stream"):
        isolated(exhibiting.react(0, t"java.lang.System.out.println(\"hi\"); java.lang.System.err.println(\"oh\")"))
      . assert:
          case Repl.Reply.Ran(_, _, _, _, _, _, _, _, blocks) =>
            Blocks.decode(blocks).sweep { case Block.Output(text, error) => (text.trim, error) } == List((t"hi", false), (t"oh", true))
          case _ => false

      test(m"a rejection's diagnostics are notices of failure"):
        isolated(exhibiting.react(0, t"val n: Int = \"hello\""))
      . assert:
          case Repl.Reply.Rejected(_, diagnostics, tokens, blocks) =>
            val decoded = Blocks.decode(blocks)
            diagnostics == t"" && decoded.exists { case Block.Notice(Tone.Failure, _, _) => true; case _ => false }
              && texts(decoded).contains(t"Int") && tokens.exists(_.mark.present)
          case _ => false

      test(m"a thrown exception is exhibited as a stack trace"):
        isolated(exhibiting.react(0, t"throw new java.lang.RuntimeException(\"boom\")"))
      . assert:
          case Repl.Reply.Threw(_, _, diagnostics, _, _, blocks) =>
            diagnostics == t"" && texts(Blocks.decode(blocks)).contains(t"boom")
          case _ => false

      test(m"a diagnostic with a blank line inside survives the wire"):
        isolated:
          val repl = exhibiting
          repl.react(0, t"/set experimental")
          repl.react(0, t"import soundness.*")
          repl.react(0, t"delay(3*Second) yet println(\"hello\")")
      . assert:
          case Repl.Reply.Rejected(_, _, _, blocks) =>
            val decoded = Blocks.decode(blocks)
            !decoded.nil && texts(decoded).contains(t"Monitor")
          case _ => false

      test(m"a connection answers a submission and pushes an asynchronous one"):
        isolated:
          val sessions = Sessions(Repl.Rendering.Exhibit(true))
          val lock = Mutex()
          var sent: List[Repl.Reply] = Nil
          val connection = sessions.connection { reply => lock { sent = reply :: sent } }
          val first = connection.respond(Repl.Request.Submit(1, t"1 + 1"))
          connection.respond(Repl.Request.Submit(2, t"/set async"))
          val third = connection.respond(Repl.Request.Submit(3, t"6 * 7"))

          // The asynchronous submission answers on another strand, so the replies are awaited
          // rather than assumed: two of them, or a generous deadline, whichever comes first.
          var waited: Int = 0

          while lock(sent.size) < 2 && waited < 20000 do
            snooze(50*Milli(Second))
            waited += 50

          (first, third, lock(sent.reverse))
      . assert:
          case (Repl.Reply.Ran(1, _, _, _, _, _, _, _, _), Unset, replies) =>
            replies.exists { case Repl.Reply.Pending(3) => true; case _ => false }
              && replies.exists { case Repl.Reply.Ran(3, _, _, _, _, _, _, _, blocks) => texts(Blocks.decode(blocks)).contains(t"42"); case _ => false }
          case _ => false

    suite(m"REPL interface"):
      // An engine whose replies the test supplies, recording every request it is given.
      class FakeEngine extends Engine:
        // Recorded newest-first and reversed when read, so appending is a cons.
        var recorded: List[Repl.Request] = Nil
        var callbacks: Map[Int, Repl.Reply -> Unit] = Map()
        var sink: Repl.Reply -> Unit = _ => ()
        var next: Int = 0
        var closed: Boolean = false

        def requests: List[Repl.Request] = recorded.reverse

        def request(request: Int => Repl.Request)(reply: Repl.Reply => Unit): Unit =
          next += 1
          recorded = request(next) :: recorded
          callbacks = callbacks.define(next, scala.caps.unsafe.unsafeAssumePure(reply))

        def send(request: Int => Repl.Request): Unit =
          next += 1
          recorded = request(next) :: recorded

        def pushed(sink: Repl.Reply => Unit): Unit = this.sink = scala.caps.unsafe.unsafeAssumePure(sink)
        def close(): Unit = closed = true
        def answer(id: Int, reply: Repl.Reply): Unit = callbacks.at(id).let(_(reply))
        def push(reply: Repl.Reply): Unit = sink(reply)

        def submissions: List[Repl.Request.Submit] =
          requests.sweep { case submit: Repl.Request.Submit => submit }

      def blocksOf(text: Text): Text = Blocks.encode(List(Block.paragraph(text)))

      def pendingEntry(block: Block): Boolean = block match
        case Block.Group(content) => content.last.lay(false) { case Block.Gauge(_, _) => true; case _ => false }
        case _                    => false

      def texts(blocks: List[Block]): Text =
        def inline(node: Inline): Text = node match
          case Inline.Textual(text)     => text
          case Inline.Code(_, tokens)   => tokens.map(_.text).join
          case Inline.Phrase(content)   => content.map(inline).join
          case Inline.Emphasis(content) => content.map(inline).join
          case Inline.Toned(_, content) => content.map(inline).join
          case Inline.Figure(value, _)  => value.toString.tt
          case _                        => t""

        def block(node: Block): Text = node match
          case Block.Paragraph(content)    => content.map(inline).join
          case Block.Group(content)        => texts(content)
          case Block.Notice(_, _, content) => texts(content)
          case Block.Output(text, _)       => text
          case _                           => t""

        blocks.map(block).join(t"\n")

      def fresh(options: ReplInterface.Options = ReplInterface.Options())(using Monitor, Probate): (FakeEngine, ReplInterface) =
        val engine = FakeEngine()
        (engine, scala.caps.unsafe.unsafeAssumePure(ReplInterface(engine, options)))

      test(m"a submission becomes a pending entry and a request"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Submitted(interface.field.input, t"1 + 1"))
          (engine.submissions, interface.transcript())
      . assert: (submissions, entries) =>
          submissions.prim.lay(false)(_.code == t"1 + 1") && submissions.size == 1
          && entries.size == 1 && entries.prim.lay(false)(pendingEntry)

      test(m"a trailing blank line is stripped from a submission"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Submitted(interface.field.input, t"val x = 1\n  x\n"))
          engine.submissions.map(_.code)
      . assert(_ == List(t"val x = 1\n  x"))

      test(m"an engine answering before the request returns still settles the entry"):
        isolated:
          val engine = FakeEngine()
          val interface: ReplInterface = scala.caps.unsafe.unsafeAssumePure(ReplInterface(engine, ReplInterface.Options()))
          val immediate: Engine = new Engine:
            def request(request: Int => Repl.Request)(reply: Repl.Reply => Unit): Unit =
              request(7) match
                case Repl.Request.Submit(id, _) => reply(Repl.Reply.Ran(id, Unset, t"", Unset, Unset, t"", Nil, Nil, blocksOf(t"at once")))
                case _                          => ()
            def send(request: Int => Repl.Request): Unit = ()
            def pushed(sink: Repl.Reply => Unit): Unit = ()
            def close(): Unit = ()
          val prompt: ReplInterface = scala.caps.unsafe.unsafeAssumePure(ReplInterface(immediate, ReplInterface.Options()))
          prompt.handle(Event.Submitted(prompt.field.input, t"1 + 1"))
          prompt.transcript()
      . assert: entries =>
          entries.size == 1 && !entries.prim.lay(false)(pendingEntry) && texts(entries).contains(t"at once")

      test(m"the reply settles the entry in place"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Submitted(interface.field.input, t"1 + 1"))
          val id = engine.submissions.prim.let(_.id).or(0)
          engine.answer(id, Repl.Reply.Ran(id, Unset, t"", Unset, Unset, t"", Nil, Nil, blocksOf(t"res0: Int = 2")))
          interface.transcript()
      . assert: entries =>
          entries.size == 1 && !entries.prim.lay(false)(pendingEntry) && texts(entries).contains(t"res0: Int = 2")

      test(m"an asynchronous run streams its output before its reply fills the entry"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Submitted(interface.field.input, t"slow()"))
          val id = engine.submissions.prim.let(_.id).or(0)
          engine.answer(id, Repl.Reply.Pending(id))
          engine.push(Repl.Reply.Output(id, t"hi\n", Repl.stdoutStream))
          val streaming = interface.transcript()
          engine.push(Repl.Reply.Ran(id, Unset, t"hi\n", Unset, Unset, t"", Nil, List(Repl.OutputSpan(Repl.stdoutStream, 0, 3)), t""))
          (streaming, interface.transcript())
      . assert:
          case (streaming, settled) =>
            // The streamed chunk shows while the run is pending; the reply then carries the whole
            // output itself, shown once.
            streaming.prim.lay(false)(pendingEntry) && texts(streaming).contains(t"hi")
              && !settled.prim.lay(false)(pendingEntry) && texts(settled).cut(t"hi").size == 2

      test(m"editing decorates the prompt at once and asks the engine for the scope"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Edited(interface.field.input, t"val x = (", 9))
          (engine.requests, interface.field.decoration())
      . assert: (requests, decoration) =>
          requests.size == 1
          && requests.prim.lay(false) { case Repl.Request.Tokenize(_, t"val x = (") => true; case _ => false }
          && decoration.incomplete && decoration.tokens.exists(_.text == t"val")

      test(m"a multi-line entry submits only after a blank line"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Edited(interface.field.input, t"val x = 1\nx", 11))
          val open = interface.field.decoration().incomplete
          interface.handle(Event.Edited(interface.field.input, t"val x = 1\nx\n", 12))
          (open, interface.field.decoration().incomplete)
      . assert(_ == (true, false))

      test(m"a command is never incomplete"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Edited(interface.field.input, t"/set async", 10))
          interface.field.decoration()
      . assert: decoration =>
          !decoration.incomplete && decoration.tokens.prim.lay(false)(_.accent == Token.Accent.Command)

      test(m"/clear empties the transcript"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Submitted(interface.field.input, t"1 + 1"))
          interface.handle(Event.Submitted(interface.field.input, t"/clear"))
          (interface.transcript(), engine.submissions.size)
      . assert(_ == (Nil, 1))

      test(m"/session asks the engine and notes the answer"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Submitted(interface.field.input, t"/session other"))
          val id = engine.requests.sweep { case Repl.Request.Session(id, t"other") => id }.prim.or(0)
          engine.answer(id, Repl.Reply.Session(id, t"other", List(t"default", t"other"), Repl.SessionOutcome.Joined))
          texts(interface.transcript())
      . assert(_.contains(Repl.messages.switched(t"other")))

      test(m"submissions join the history and are persisted"):
        isolated:
          // Recorded newest-first, as elsewhere in this harness, and reversed when read.
          var persisted: List[Text] = Nil
          val (engine, interface) = fresh(ReplInterface.Options(history = List(t"earlier"), persist = line => persisted = line :: persisted))
          interface.handle(Event.Submitted(interface.field.input, t"1 + 1"))
          interface.handle(Event.Submitted(interface.field.input, t"1 + 1"))
          (interface.field.history(), persisted.reverse)
      . assert(_ == (List(t"earlier", t"1 + 1"), List(t"1 + 1")))

      test(m"an unknown command is refused without troubling the engine"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Submitted(interface.field.input, t"/nonsense"))
          (engine.submissions.size, texts(interface.transcript()))
      . assert:
          case (0, text) => text.contains(Repl.messages.unknownCommand(t"/nonsense"))
          case _ => false

      test(m"closing the interface closes the engine"):
        isolated:
          val (engine, interface) = fresh()
          interface.handle(Event.Closed)
          engine.closed
      . assert(_ == true)

    suite(m"REPL TCP server"):
      given Scalac[3.9, Universe.Classfile] = Scalac(Nil)

      test(m"a reply carries the value, type and highlighting"):
        isolated:
          val tcpPort = Port[Tcp]()
          val service = Opened(Sessions()).serve(tcpPort)
          val socket  = Wire.tcp(tcpPort)

          try socket.exchange(Repl.Request.Submit(1, t"1 + 1"))
          finally
            socket.close()
            service.stop()
      // The type is the SINGLETON `2`, not `Int`: an expression line is bound as a `final val`, so a
      // constant expression keeps the constant type the compiler folds it to (see `Repl.resultType`).
      . assert:
          case Repl.Reply.Ran(_, value, _, tpe, _, _, _, _, _) =>
            value.let(_ == t"2").or(false) && tpe.let(_.text == t"2").or(false)

          case _ =>
            false

      test(m"a quit request fulfils the server's quit signal"):
        isolated:
          val tcpPort  = Port[Tcp]()
          val sessions = Opened(Sessions())
          val service  = sessions.serve(tcpPort)
          val socket   = Wire.tcp(tcpPort)

          try
            socket.send(Repl.Request.Quit(0))

            // Wait (bounded) for the quit signal rather than blocking forever: the wait runs on
            // its own strand, and the test passes if it finishes within the timeout.
            val waiter = async(sessions.awaitQuit())
            safely(waiter.await(5*Second)).present
          finally
            socket.close()
            service.stop()
      . assert(_ == true)

      test(m"a message sent over a UNIX domain socket is answered"):
        isolated:
          val directory: Path on Linux = temporaryDirectory/Uuid()
          directory.create[Directory]()
          val socketPath: Text = (directory/t"repl.sock").encode
          val service = Opened(Sessions()).serve(socketPath)
          val socket  = Wire.domain(socketPath)

          try socket.exchange(Repl.Request.Submit(3, t"6 * 7"))
          finally
            socket.close()
            service.stop()
      . assert:
          case Repl.Reply.Ran(_, value, _, _, _, _, _, _, _) => value.let(_ == t"42").or(false)
          case _                                    => false

      test(m"sessions are independent and each has a distinct name"):
        isolated:
          val sessions = Opened(Sessions())
          val a = sessions.create()
          val b = sessions.create()
          def session(name: Text) =
            sessions.session(name).or(panic(m"the session just created is missing"))

          session(a).interpret(t"val marker = 111")
          ( a != b && sessions.names.has(a) && sessions.names.has(b),
            session(a).interpret(t"marker"),
            session(b).interpret(t"marker") )
      . assert:
          case (true, Repl.Outcome.Ran(_, _, _, _, _), Repl.Outcome.Rejected(_)) => true
          case _                                                                  => false

      test(m"a Session request switches the connection and lists sessions"):
        isolated:
          val directory: Path on Linux = temporaryDirectory/Uuid()
          directory.create[Directory]()
          val socketPath: Text = (directory/t"repl2.sock").encode
          val sessions = Opened(Sessions())
          val service = sessions.serve(socketPath)
          val socket  = Wire.domain(socketPath)

          try
            // Connecting auto-starts a session; querying reports it plus the list.
            socket.exchange(Repl.Request.Session(1, t""))
          finally
            socket.close()
            service.stop()
      . assert:
          case Repl.Reply.Session(_, name, names, Repl.SessionOutcome.Created) =>
            name != t"" && names.has(name)

          case _ =>
            false

      // Naming a session that does not exist STARTS it under that name (`/session work`), reported
      // as `Created`; naming it again merely switches (`Joined`).
      test(m"a Session request naming an unknown session starts it under that name"):
        isolated:
          val tcpPort = Port[Tcp]()
          val service = Opened(Sessions()).serve(tcpPort)
          val socket  = Wire.tcp(tcpPort)

          try
            val first  = socket.exchange(Repl.Request.Session(1, t"work"))
            val second = socket.exchange(Repl.Request.Session(2, t"work"))
            (first, second)
          finally
            socket.close()
            service.stop()
      . assert:
          case ( Repl.Reply.Session(1, t"work", names, Repl.SessionOutcome.Created),
                 Repl.Reply.Session(2, t"work", _, Repl.SessionOutcome.Joined) ) =>
            names.has(t"work")

          case _ =>
            false

      // `--create work` starts a new session; a second `--create work` must FAIL (name taken),
      // reported as `Exists` without switching.
      test(m"a Create request starts a new session, and fails if the name is taken"):
        isolated:
          val tcpPort = Port[Tcp]()
          val service = Opened(Sessions()).serve(tcpPort)
          val socket  = Wire.tcp(tcpPort)

          try
            val first  = socket.exchange(Repl.Request.Create(1, t"work"))
            val second = socket.exchange(Repl.Request.Create(2, t"work"))
            (first, second)
          finally
            socket.close()
            service.stop()
      . assert:
          case ( Repl.Reply.Session(1, t"work", _, Repl.SessionOutcome.Created),
                 Repl.Reply.Session(2, _, _, Repl.SessionOutcome.Exists) ) =>
            true

          case _ =>
            false

      // `--join work` joins an existing session; joining one that does not exist must FAIL
      // (`Missing`) and create nothing.
      test(m"a Join request joins an existing session, and fails if there is none"):
        isolated:
          val tcpPort = Port[Tcp]()
          val service = Opened(Sessions()).serve(tcpPort)
          val socket  = Wire.tcp(tcpPort)

          try
            val missing = socket.exchange(Repl.Request.Join(1, t"work"))
            socket.exchange(Repl.Request.Create(2, t"work"))
            val joined  = socket.exchange(Repl.Request.Join(3, t"work"))
            val names   = socket.exchange(Repl.Request.SessionList(4))
            (missing, joined, names)
          finally
            socket.close()
            service.stop()
      . assert:
          case ( Repl.Reply.Session(1, _, _, Repl.SessionOutcome.Missing),
                 Repl.Reply.Session(3, t"work", _, Repl.SessionOutcome.Joined),
                 Repl.Reply.SessionList(4, sessions) ) =>
            sessions == List(t"work")

          case _ =>
            false

      // The reifier (one dotc context) is shared by a submission's diagnostic rendering and a
      // completion's signature rendering, which the socket server runs concurrently. Unserialised,
      // two first-time `semanticImports` builds corrupted a package scope and one thread looped
      // forever: this drives both at once, repeatedly, and must return.
      test(m"a concurrent completion and diagnostic on one session both return"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"import soundness.*")
          var stuck: Boolean = false

          (1 to 3).each: round =>
            val complete: Runnable = caps.unsafe.unsafeAssumePure(() => { repl.completionsAt(t"List(1, 2, 3).m", 14); () })
            val submit:   Runnable = caps.unsafe.unsafeAssumePure(() => { repl.react(round, t"val n: Int = \"hello\""); () })
            val completing = new Thread(complete)
            val submitting = new Thread(submit)
            completing.setDaemon(true); submitting.setDaemon(true)
            completing.start(); submitting.start()
            completing.join(120000L); submitting.join(120000L)
            if completing.isAlive || submitting.isAlive then stuck = true

          !stuck
      . assert(_ == true)

      // A session's ambient `WorkingDirectory`/`Environment`/`System` (see `ReplContext`) reflect the
      // values filed for it — the client's — and `System`'s `user.dir` follows the working directory.
      test(m"the ambient WorkingDirectory, Environment and System are the client's"):
        isolated:
          val repl = Opened(Repl())
          ReplContext.set(repl.session, ReplContext.Values(t"/tmp/flame-client",
            Map(t"FLAME_TEST_VARIABLE" -> t"present")))
          repl.react(0, t"/set experimental")
          repl.react(0, t"import soundness.*")
          repl.interpret(t"(summon[WorkingDirectory].directory(), summon[Environment].variable(t\"FLAME_TEST_VARIABLE\"), summon[System](t\"user.dir\"))")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) =>
            value.let(_.contains(t"/tmp/flame-client")).or(false) && value.let(_.contains(t"present")).or(false)

          case _ =>
            false

      // The ambient givens are conditional, so a user's own `given` on an earlier line (in scope
      // through the history import, an inner level) — or an import of Soundness's — wins outright.
      test(m"a user-defined WorkingDirectory overrides the ambient one"):
        isolated:
          val repl = Opened(Repl())
          ReplContext.set(repl.session, ReplContext.Values(t"/tmp/flame-client", Map()))
          repl.react(0, t"/set experimental")
          repl.react(0, t"import soundness.*")
          repl.react(1, t"given mine: WorkingDirectory = () => t\"/mine\"")
          repl.interpret(t"summon[WorkingDirectory].directory()")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_.contains(t"/mine")).or(false)
          case _                                   => false

      test(m"an imported Soundness Environment given overrides the ambient one without ambiguity"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"/set experimental")
          repl.react(0, t"import soundness.*")
          repl.react(1, t"import environments.emptyEnvironment")
          repl.interpret(t"summon[Environment].variable(t\"HOME\")")
      // `emptyEnvironment` answers `Unset` (rendered `○`) where the ambient one would give a path;
      // and the line ran — no ambiguity between the two givens.
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_.contains(t"○")).or(false)
          case _                                   => false

      // Over the wire: a `Context` request files the connecting client's values on its session.
      test(m"a Context request makes the client's working directory ambient in its session"):
        isolated:
          val tcpPort = Port[Tcp]()
          val service = Opened(Sessions()).serve(tcpPort)
          val socket  = Wire.tcp(tcpPort)

          try
            socket.exchange(Repl.Request.Session(1, t""))
            socket.send(Repl.Request.Context(2, t"/tmp/flame-over-the-wire", List(Repl.Pair(t"K", t"v"))))
            socket.exchange(Repl.Request.Submit(3, t"/set experimental"))
            socket.exchange(Repl.Request.Submit(4, t"import soundness.*"))
            socket.exchange(Repl.Request.Submit(5, t"summon[WorkingDirectory].directory()"))
          finally
            socket.close()
            service.stop()
      . assert:
          case Repl.Reply.Ran(_, value, _, _, _, _, _, _, _) => value.let(_.contains(t"/tmp/flame-over-the-wire")).or(false)
          case _                                          => false

      // stdout and stderr are both captured, in the order they interleaved, and a reply locates each
      // run by stream (`spans`) within the plain `output`, so the front-ends can shade them apart.
      test(m"stdout and stderr are captured in order, as separate spans"):
        isolated:
          Opened(Repl()).react(0, t"println(\"a\"); System.err.println(\"b\"); println(\"c\")")
      . assert:
          case Repl.Reply.Ran(_, _, output, _, _, _, _, spans, _) =>
            output == t"a\nb\nc\n"
            && spans == List(Repl.OutputSpan(t"out", 0, 2), Repl.OutputSpan(t"err", 2, 2), Repl.OutputSpan(t"out", 4, 2))

          case _ =>
            false

      // The ambient `Stdio` (see `ReplStdio`) routes `Err` to the captured stderr as well.
      test(m"Err.println is captured as stderr"):
        isolated:
          val repl = Opened(Repl())
          repl.react(0, t"/set experimental")
          repl.react(0, t"import soundness.*")
          repl.react(1, t"Err.println(t\"oops\")")
      . assert:
          case Repl.Reply.Ran(_, _, output, _, _, _, _, spans, _) =>
            output.contains(t"oops") && spans.exists(_.stream == t"err") && !spans.exists(_.stream == t"out")

          case _ =>
            false

      // A rejected line's highlight carries the error's span, mapped from the wrapped source back
      // into the line's own coordinates (`Repl.userSpan`) and split into marked tokens
      // (`Repl.mark`): here exactly the offending `"hello"`.
      test(m"a type error's span is marked on the rejected line's tokens"):
        isolated:
          Opened(Repl()).react(0, t"val n: Int = \"hello\"")
      . assert:
          case Repl.Reply.Rejected(_, _, tokens, _) =>
            val marked = tokens.filter(_.mark.present)
            marked.map(_.text).join == t"\"hello\"" && !marked.exists(_.mark != Repl.errorMark)

          case _ =>
            false

      // A span on the SECOND line of a multi-line submission lands on that line's tokens.
      test(m"a span on a later line of a multi-line submission is marked there"):
        isolated:
          Opened(Repl()).react(0, t"def f: Int =\n  1 + \"x\"")
      . assert:
          case Repl.Reply.Rejected(_, _, tokens, _) =>
            val marked = tokens.filter(_.mark.present).map(_.text).join
            marked.contains(t"\"x\"") && !marked.contains(t"def")

          case _ =>
            false

      // `userSpan` arithmetic: the first user line is shifted by the indent AND the binding prefix,
      // later lines by the indent alone; a span before the body, or past it, is dropped.
      test(m"userSpan maps wrapped coordinates back to the user's line"):
        val body = 5
        val onFirst  = Repl.userSpan(Span.area(Ordinal.zerary(5), Ordinal.zerary(2 + 8 + 3), Ordinal.zerary(5), Ordinal.zerary(2 + 8 + 7)), body, 2, 8, 2)
        val onSecond = Repl.userSpan(Span.area(Ordinal.zerary(6), Ordinal.zerary(4), Ordinal.zerary(6), Ordinal.zerary(6)), body, 2, 8, 2)
        val before   = Repl.userSpan(Span.area(Ordinal.zerary(1), Ordinal.zerary(0), Ordinal.zerary(1), Ordinal.zerary(3)), body, 2, 8, 2)
        val after    = Repl.userSpan(Span.area(Ordinal.zerary(9), Ordinal.zerary(0), Ordinal.zerary(9), Ordinal.zerary(3)), body, 2, 8, 2)
        ( onFirst.let(s => (s.startLine.let(_.n0), s.startColumn.let(_.n0), s.endColumn.let(_.n0))),
          onSecond.let(s => (s.startLine.let(_.n0), s.startColumn.let(_.n0), s.endColumn.let(_.n0))),
          before.present, after.present )
      . assert(_ == ((0, 3, 7), (1, 2, 4), false, false))

      // A `HistoryEntry` — the record the client appends to `.pyrocosm/flame/history` for each
      // submitted prompt — round-trips through BinTEL (a bare `Text` will not encode at the top
      // level, so the line is wrapped in this struct).
      test(m"a HistoryEntry round-trips through BinTEL"):
        isolated:
          val data: Data = Repl.HistoryEntry(t"import soundness.*").bintel
          Bintel.read[Repl.HistoryEntry](data)
      . assert:
          case Repl.HistoryEntry(t"import soundness.*") => true
          case _                                        => false

      // A `SessionList` request only reports; it must not create a session (as an empty `Session`
      // request would through the connection's lazy current session), so a `--session` completion
      // can enumerate the joinable sessions without leaving throwaways behind.
      test(m"a SessionList request lists sessions without creating one"):
        isolated:
          val tcpPort = Port[Tcp]()
          val service = Opened(Sessions()).serve(tcpPort)
          val socket  = Wire.tcp(tcpPort)

          try
            val before = socket.exchange(Repl.Request.SessionList(1))
            val second = socket.exchange(Repl.Request.SessionList(2))
            socket.exchange(Repl.Request.Session(3, t"work"))
            val after  = socket.exchange(Repl.Request.SessionList(4))
            (before, second, after)
          finally
            socket.close()
            service.stop()
      . assert:
          case ( Repl.Reply.SessionList(1, Nil),
                 Repl.Reply.SessionList(2, Nil),
                 Repl.Reply.SessionList(4, names) ) =>
            names == List(t"work")

          case _ =>
            false

      test(m"a completion request returns matching completions"):
        isolated:
          val tcpPort = Port[Tcp]()
          val service = Opened(Sessions()).serve(tcpPort)
          val socket  = Wire.tcp(tcpPort)

          try socket.exchange(Repl.Request.Complete(1, t"List(1, 2, 3).m", 15))
          finally
            socket.close()
            service.stop()
      . assert:
          case Repl.Reply.Completed(_, items) => items.exists(_.name.contains(t"map"))
          case _                              => false

      // A tokenize reply carries the scope the unfinished line has opened — computed on the
      // connection's session (created by the first submission), so it sees that session's history.
      test(m"a tokenize request reports the scope an unfinished line has opened"):
        isolated:
          val tcpPort = Port[Tcp]()
          val service = Opened(Sessions()).serve(tcpPort)
          val socket  = Wire.tcp(tcpPort)

          try
            socket.exchange(Repl.Request.Submit(1, t"val xs = scala.List(1, 2)"))
            socket.exchange(Repl.Request.Tokenize(2, t"xs.map { y =>\n  "))
          finally
            socket.close()
            service.stop()
      . assert:
          case Repl.Reply.Tokenized(2, _, true, false, Repl.ScopeBinding(name, tpe, false, _) :: Nil) =>
            name == t"y" && tpe == t"Int"

          case _ =>
            false

    suite(m"REPL block captures outside references"):
      given Scalac[3.9, Universe.Classfile] = Scalac(Nil)

      test(m"a lifted def can reference a value from the enclosing scope"):
        isolated:
          val base = 100

          val repl = Repl[3.9]:
            def shifted(n: Int): Int = n + base
          Opened(repl)

          repl.interpret(t"shifted(5)")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"105").or(false)
          case _                             => false

      test(m"a lifted def captures an enclosing method parameter"):
        def session(base: Int): Repl.Outcome = isolated:
          val repl = Repl[3.9]:
            def plus(n: Int): Int = n + base
          Opened(repl)

          repl.interpret(t"plus(2)")

        session(40)
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"42").or(false)
          case _                             => false

      test(m"a lifted def can reference both a block binding and an outside value"):
        isolated:
          val base = 100

          val repl = Repl[3.9]:
            val offset = 5
            def total: Int = offset + base
          Opened(repl)

          repl.interpret(t"total")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"105").or(false)
          case _                             => false

      test(m"a lifted def references a field of an enclosing object, tracking changes"):
        isolated:
          ReplFixture.greeting = "hi"     // length 2; then mutated to "changed" (7)
          ReplFixture.session
      . assert:
          case (Repl.Outcome.Ran(_, before, _, _, _), Repl.Outcome.Ran(_, after, _, _, _)) =>
            before.let(_ == t"2").or(false) && after.let(_ == t"7").or(false)
          case _ =>
            false

      test(m"a lifted def can write back to a host var"):
        isolated:
          var tally = 1

          val repl = Repl[3.9]:
            def bump(): Unit = tally = tally + 10
          Opened(repl)

          repl.interpret(t"bump()")
          repl.interpret(t"bump()")
          tally
      . assert(_ == 21)

      test(m"a lifted import is in scope for a lifted def and for later lines"):
        isolated:
          val repl = Repl[3.9]:
            import scala.collection.mutable.ListBuffer
            def make: ListBuffer[Int] = ListBuffer(1, 2, 3)
          Opened(repl)

          repl.interpret(t"make.sum")               // lifted def uses the import
          repl.interpret(t"ListBuffer(9, 9).sum")   // a later line uses it directly
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"18").or(false)
          case _                             => false

      test(m"a block-local var can be reassigned from a REPL line"):
        isolated:
          val repl = Repl[3.9]:
            var counter = 10
          Opened(repl)

          repl.interpret(t"counter = counter + 5")
          repl.interpret(t"counter")
      . assert:
          case Repl.Outcome.Ran(_, value, _, _, _) => value.let(_ == t"15").or(false)
          case _                             => false
