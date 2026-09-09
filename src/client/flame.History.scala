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

// This module drives BinTEL and java.io over stdlib lists, so `List`/`Nil` here are the stdlib
// ones (the client's prelude `List` is proscenium's opaque collection, whose API differs).
import scala.collection.immutable.{List, Nil}
import scala.collection.mutable.ArrayBuffer

import soundness.*

// The persistent prompt history for a project: the lines submitted at the REPL, stored as a
// sequence of length-prefixed BinTEL `Repl.HistoryEntry` frames in `.pyrocosm/flame/history` (see
// `Workspace.historyPath`). One record is APPENDED per submission — cheap, and crash-safe, since a
// half-written trailing frame is simply ignored on read — and the log is COMPACTED (rewritten) to
// the most recent `limit` entries once it is full, so it stays append-mostly yet never exceeds it.
// Loaded and prepended to the in-memory command history at startup, so the arrow keys walk up into
// earlier sessions.
object History:
  import strategies.throwUnsafely

  // Every entry in `file`, oldest first; empty when the file is absent or unreadable. A trailing
  // frame that does not fully read (an interrupted append) ends the scan without failing.
  def load(file: Text): List[Text] =
    val handle: ji.File = ji.File(file.s)
    if !handle.exists.nn then Nil else
      val entries: ArrayBuffer[Text] = ArrayBuffer()

      val in: ji.DataInputStream = ji.DataInputStream(ji.BufferedInputStream(ji.FileInputStream(handle)))

      try
        var continue: Boolean = true
        while continue do
          val length: Int = try in.readInt() catch case _: ji.IOException => -1
          if length < 0 then continue = false
          else
            val bytes: scala.Array[Byte] = new scala.Array[Byte](length)
            try
              in.readFully(bytes)
              safely(Bintel.read[Repl.HistoryEntry](bytes.immutable(using Unsafe)))
                .let { entry => entries += entry.line; () }
            catch case _: ji.IOException => continue = false
      catch case _: Throwable => ()
      finally try in.close() catch case _: Throwable => ()

      entries.toList

  // Appends one framed `HistoryEntry` record to `file` (created if absent). Best-effort: a failure
  // to persist a line never disrupts the session.
  def append(file: Text, line: Text): Unit =
    write(file, List(line), appending = true)

  // Rewrites `file` to hold exactly `lines` (used to compact the log). Best-effort.
  def replace(file: Text, lines: List[Text]): Unit =
    write(file, lines, appending = false)

  private def write(file: Text, lines: List[Text], appending: Boolean): Unit =
    val out: ji.DataOutputStream =
      ji.DataOutputStream(ji.BufferedOutputStream(ji.FileOutputStream(file.s, appending)))

    try
      lines.foreach: line =>
        val body: Data = unsafely(Repl.HistoryEntry(line).bintel)
        out.writeInt(body.length)
        out.write(body.mutable(using Unsafe))
      out.flush()
    catch case _: Throwable => ()
    finally try out.close() catch case _: Throwable => ()
