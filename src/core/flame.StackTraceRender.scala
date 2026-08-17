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
┃    Flame, version 0.1.0.                                                                         ┃
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

import proscenium.compat.*

import anticipation.*
// `stackTraceTeletype` must be named: it is a given, and a wildcard import does not bring givens into
// scope. Without it, escapade's generic `Showable`-based `Teletypeable` still applies, so the trace
// renders — silently unstyled, with no compile error. It lives under `digression.teletypeables`,
// where the API-nesting drive homed it (it was a top-level given in `digression` before, and in
// escapade before that), so the named import is what keeps that move honest: a relocation breaks the
// build here rather than quietly costing the trace its colour.
import digression.*, digression.teletypeables.stackTraceTeletype
import escapade.*, termcapDefinitions.xtermTrueColorTermcap
import hellenism.Classloader
import hieroglyph.*, textMetrics.uniformMetric
// Resolve each frame against the TASTy and SMAP of the class it names, rather than taking the JVM's
// erased view of it. `StackTrace(throwable)` takes a `StackTrace.Resolver`, and digression's companion
// supplies a no-op one — digression deliberately has no classpath to read — so WITHOUT this import a
// trace still renders, just as bare mangled class·method names with no source definitions, no source
// lines and no inline chains. Reading is not free (one TASTy file per top-level class named in the
// trace), but a trace is only rendered when a submission has already thrown.
import hyperbole.stackResolutions.tastyStackResolution

// Renders the stack trace of an exception thrown by user code (under `strategies.throwUnsafely`) to a
// coloured, truecolor-ANSI listing, for the REPL's `Threw` reply. Uses Soundness `digression`:
// `StackTrace(throwable)` captures the frames — resolved through hyperbole, so each names the source
// definition it was compiled from and carries the chain of inlines it came through — and escapade
// renders `StackTrace` to a `Teletype` (its exception class + message, then each frame's
// class·method and file:line, with a `↳` sub-row per inline level) via digression's default
// `StackTrace.Palette`. The internal REPL/JVM plumbing above the user's wrapper object is trimmed off.
//
// An inline chain is only recoverable where the frame's own classfile carries a JSR-45 SMAP: the
// Soundness jars ship one, flame's own classes get one from `-Xjsr45` in its build, and a user's REPL
// line does only under `/set jsr45` (off by default — see `Repl.settings`).
object StackTraceRender:
  // The index of the OUTERMOST frame belonging to the user's code: their code is compiled into the
  // wrapper objects `rs$line$N`, so this is the last frame whose (raw) class name starts with
  // `rs$line$`. Read from the raw `StackTraceElement`s (whose names are unmangled, unlike the frames
  // digression rewrites), but the frame ORDER matches `StackTrace`'s, so the index carries over.
  private def lastUserFrame(error: Throwable): Int =
    val raw:  scala.Array[jl.StackTraceElement | Null] = error.getStackTrace.nn
    var last: Int                                = -1
    var i:    Int                                = 0

    while i < raw.length do
      if raw(i).nn.getClassName.nn.startsWith("rs$line$") then last = i
      i += 1

    last

  // The `Classloader` is the resolver's: it reads each frame's classfile and TASTy through it, so it
  // must be the REPL's own loader — the one the session's compiled lines were loaded from — for a
  // user's frames to resolve at all.
  def render(error: Throwable)(using Classloader): Text =
    val trace: StackTrace = StackTrace(error)
    val last:  Int        = lastUserFrame(error)

    // Drop the machinery above the user's outermost frame (reflection, the classloader, the engine,
    // the worker thread); keep the whole trace if no user frame is identifiable.
    val trimmed: StackTrace =
      if last < 0 then trace else trace.dropRight(trace.frames.length - 1 - last)

    trimmed.teletype.render(xtermTrueColorTermcap)
