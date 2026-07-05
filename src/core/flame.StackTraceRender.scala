                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃    Soundness, version 0.54.0. © Copyright 2021-25 Jon Pretty, Propensive OÜ.                     ┃
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

import anticipation.*
import digression.*
import escapade.*, termcapDefinitions.xtermTrueColorTermcap
import hieroglyph.*, textMetrics.uniformMetric

// Renders the stack trace of an exception thrown by user code (under `strategies.throwUnsafely`) to a
// coloured, truecolor-ANSI listing, for the REPL's `Threw` reply. Uses Soundness `digression`:
// `StackTrace(throwable)` captures the frames, and escapade renders `StackTrace` to a `Teletype` (its
// exception class + message, then each frame's class·method and file:line) via digression's default
// `StackTrace.Palette`. The internal REPL/JVM plumbing above the user's wrapper object is trimmed off.
object StackTraceRender:
  // The index of the OUTERMOST frame belonging to the user's code: their code is compiled into the
  // wrapper objects `rs$line$N`, so this is the last frame whose (raw) class name starts with
  // `rs$line$`. Read from the raw `StackTraceElement`s (whose names are unmangled, unlike the frames
  // digression rewrites), but the frame ORDER matches `StackTrace`'s, so the index carries over.
  private def lastUserFrame(error: Throwable): Int =
    val raw:  Array[jl.StackTraceElement | Null] = error.getStackTrace.nn
    var last: Int                                = -1
    var i:    Int                                = 0

    while i < raw.length do
      if raw(i).nn.getClassName.nn.startsWith("rs$line$") then last = i
      i += 1

    last

  def render(error: Throwable): Text =
    val trace: StackTrace = StackTrace(error)
    val last:  Int        = lastUserFrame(error)

    // Drop the machinery above the user's outermost frame (reflection, the classloader, the engine,
    // the worker thread); keep the whole trace if no user frame is identifiable.
    val trimmed: StackTrace =
      if last < 0 then trace else trace.dropRight(trace.frames.length - 1 - last)

    trimmed.teletype.render(xtermTrueColorTermcap)
