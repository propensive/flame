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

import java.net as jn
import java.util.concurrent as juc

import scala.caps

// Like `ReplModuleCompiler`, this file drives the compiler API directly, which speaks stdlib
// collections; `sci` names them explicitly, since `scala` is out of `-Yimports`.
import scala.collection.immutable as sci

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.NoContext
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.quoted.QuotesCache
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans

import anticipation.*
import gossamer.*
import rudiments.*
import stenography.Syntax
import vacuous.*

// Answers "what has the line introduced into scope so far?" for an UNFINISHED line: the bindings
// — contextual (`using`/`given`) and plain — that would be visible to the statement the user is
// about to type next, so a front-end can show that `unsafely:` has made an `Unsafe` (and a
// `ThrowTactic`, and a `CanThrow`) available, or that `xs.map { x =>` has bound an `x: Int`.
//
// Nothing is EVALUATED: the line is extended with a marker expression where the next statement
// would go (see `Repl.scopeProbe`), and the result is TYPECHECKED ONLY, through Dotty's
// `InteractiveDriver` — the typer-only driver the IDE uses. That is enough: the bindings a scope
// introduces come from the PARAMETER TYPES of the call that opens it (a `Unsafe ?=> T` block
// becomes a synthetic lambda with a `using Unsafe` parameter; an `Int => R` block binds its `x`),
// and the typer enters them before inlining or macro expansion — so the answer is the same whether
// the opener is a plain method, an `inline def`, or a macro, and a side-effecting opener
// (`withResource(r):`) never runs. What it cannot know is a type the BODY would have determined
// (a `[T](block: T ?=> R)` opener with `T` unconstrained typechecks the marker at `Nothing`).
//
// From the marker's position, `Interactive.contextOfPath` rebuilds the typer's context, and its
// chain of scopes — one per enclosing lambda, block or method — is walked outwards until the
// wrapper object's own scope, collecting every term binding on the way: innermost first, so the
// newest scope's contributions lead. Everything the line's own scopes hold is reported, not only
// the innermost, since all of it is new relative to the session, and a `val` declared inside the
// block is as much in scope as the block's parameters are.
//
// The driver is persistent — it keeps the classpath's symbol table across runs, so after a first
// run of well under a second, a probe costs tens of milliseconds — and, like the warm compiler
// session (`Repl.Warm`), lives on ONE dedicated thread with a generous stack: dotc pins a context
// base to the thread of its first run, and typing against the whole Soundness classpath recurses
// deeply. A caller hands over the source and waits for the answer, bounded by `timeout`; any
// failure is simply "no information". One limit of the persistence: the driver enumerates a
// package's classes once, so a wrapper object compiled AFTER it opened (the next submitted line)
// is invisible to it, and its history import fails — which is why `Repl#scopeAt` keys the
// inspector on the session's line index and opens a fresh one per submitted line.
object ScopeInspector:
  // The expression appended where the next statement would go. Fully-qualified, so it resolves
  // under any predef the session compiles with; typed `Nothing`, so it conforms to any expected
  // type and never fixes an inference variable the body would have.
  val marker: Text = t"scala.Predef.???"

  val stackSize: Long = 64L*1024*1024
  val timeout: Long = 5000L

class ScopeInspector(arguments: List[Text]):
  private val executor: juc.ExecutorService =
    juc.Executors.newSingleThreadExecutor: (runnable: Runnable | Null) =>
      val thread = Thread(null, runnable, "flame-scope", ScopeInspector.stackSize)
      thread.setDaemon(true)
      thread
    . nn

  // Created on first use, ON the executor thread (only `run` touches it), so its context base is
  // pinned to the thread that will drive it thereafter.
  private lazy val driver: InteractiveDriver = InteractiveDriver(arguments.map(_.s).stdlib)

  // The bindings in scope at `offset` (where `ScopeInspector.marker` sits) in `source`, types
  // rendered against `imports`. Empty on any failure or timeout.
  def inspect(source: Text, offset: Int, imports: stenography.Imports): List[Repl.ScopeBinding] =
    val task: juc.Callable[List[Repl.ScopeBinding]] =
      caps.unsafe.unsafeAssumePure(() => run(source, offset, imports))

    try executor.submit(task).nn.get(ScopeInspector.timeout, juc.TimeUnit.MILLISECONDS).nn
    catch case _: Throwable => Nil

  def retire(): Unit = executor.shutdownNow()

  private def run(source: Text, offset: Int, imports: stenography.Imports): List[Repl.ScopeBinding] =
    val uri = jn.URI("file:///flame-scope.scala")
    driver.run(uri, source.s)
    val unit = driver.compilationUnits(uri)

    given context: Context = QuotesCache.init(driver.currentCtx.fresh.setCompilationUnit(unit))
    given quotes: scala.quoted.Quotes = scala.quoted.runtime.impl.QuotesImpl()(using context)

    val span     = Spans.Span(offset, offset + ScopeInspector.marker.length)
    val position = SourcePosition(unit.source, span)
    val path     = Interactive.pathTo(driver.openedTrees(uri), position)

    var scope: Context = Interactive.contextOfPath(path)
    var seen: sci.Set[Text] = sci.Set()
    var bindings: sci.List[Repl.ScopeBinding] = sci.Nil
    var depth: Int = 0

    // Outwards from the marker to the wrapper object (the first CLASS owner): each context's scope
    // holds the bindings that level introduced. A binding of the same name and type at two levels
    // (each of two nested `unsafely` blocks supplies a `CanThrow[Exception]`) is reported once.
    while (scope ne NoContext) && !scope.owner.isClass && depth < 64 do
      scope.scope.toList.foreach: symbol =>
        if relevant(symbol) then
          val binding = render(symbol, imports)
          val key = t"${binding.name.or(t"")}:${binding.tpe}"
          if !seen.contains(key) then
            seen += key
            bindings = bindings :+ binding

      scope = scope.outer
      depth += 1

    List.from(bindings)

  // A term binding the user could refer to (or that a `using` clause could pick up): a parameter
  // or a local value — not the synthetic `$anonfun` method a context function desugars into, and
  // not a nested `def`.
  private def relevant(symbol: Symbol)(using Context): Boolean =
    symbol.isTerm && !symbol.is(Flags.Method) && !symbol.name.toString.startsWith("$anonfun")

  // A context function's parameters are synthetic (`x$1`, `contextual$1`, `evidence$1`): only a
  // name the user could write is reported; the rest are shown by type alone, as a `given` would be.
  private def render(symbol: Symbol, imports: stenography.Imports)
      (using context: Context, quotes: scala.quoted.Quotes)
  :   Repl.ScopeBinding =

    val raw: String = symbol.name.toString
    val name: Optional[Text] = if raw.contains("$") then Unset else raw.tt
    val tpe = symbol.info.widen

    val text: Text =
      try Syntax(tpe.asInstanceOf[quotes.reflect.TypeRepr]).text(using imports)
      catch case _: Throwable => tpe.show.tt

    Repl.ScopeBinding(name, text, symbol.is(Flags.Given) || symbol.is(Flags.Implicit))
