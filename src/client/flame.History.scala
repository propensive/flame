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
  import filesystemBackends.javaBaseFilesystem

  // Every entry in `file`, oldest first; empty when the file is absent or unreadable. A trailing
  // frame that does not fully read (an interrupted append) ends the scan without failing: the
  // framing raises, and everything read before it is kept.
  def load(file: Text): List[Text] =
    safely:
      val path: Path on Linux = file.as[Path on Linux]

      if !path.existent() then Nil else
        var entries: List[Text] = Nil

        // `safely` around the whole walk, not around each record: a truncated final frame raises
        // out of the iterator, and the entries gathered before it are what the history holds.
        safely:
          val frames = Iterator(path.read[Data]).frames[LengthPrefix]

          while frames.hasNext do
            safely(Bintel.read[Repl.HistoryEntry](frames.next())).let: entry =>
              entries = entry.line :: entries

        entries.reverse

    . or(Nil)

  // Appends one framed `HistoryEntry` record to `file` (created if absent). Best-effort: a failure
  // to persist a line never disrupts the session.
  def append(file: Text, line: Text): Unit = write(file, List(line), appending = true)

  // Rewrites `file` to hold exactly `lines` (used to compact the log). Best-effort.
  def replace(file: Text, lines: List[Text]): Unit = write(file, lines, appending = false)

  private def write(file: Text, lines: List[Text], appending: Boolean): Unit =
    safely:
      val path: Path on Linux = file.as[Path on Linux]
      val records: Chain[Data] = Chain(lines.map { line => frame(line) }*)

      // `Eof(path)` is galilei's append mode: the same file, opened positioned at its end.
      // `Eof(path)` is galilei's append mode: the same file, opened positioned at its end.
      // `Create` as well, since the first line of a session appends to a file that does not exist
      // yet — without it the append is a no-op and no history is ever persisted.
      if appending then Eof(path).open(Write, OpenFlag.Create) { handle ?=> handle.write(records) }
      else path.open[File](Write, OpenFlag.Create, OpenFlag.Truncate) { handle ?=> handle.write(records) }

    ()

  private def frame(line: Text): Data =
    LengthPrefix.encode(unsafely(Repl.HistoryEntry(line).bintel))
