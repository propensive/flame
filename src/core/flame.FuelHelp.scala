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

import scala.collection.concurrent.TrieMap

import soundness.*

import environments.javaBaseEnvironment
import pyrocosm.Shares

// Fuel's documentation of a project, read here for completions: each documented entity's
// address (`gossamer.Text#length`) and its one-line `info`, which Fuel publishes as a share
// (`pyrocosm.Shares`) — a RaTEL image in the project's cache — whenever it compiles. The file
// is mapped into memory, and attaching a line to a candidate descends the image's index to
// that one entry: the rest is not read, nothing waits on another process, and Fuel need not be
// running.
//
// It is always the project's current index. The reader asks the filesystem whether Fuel has
// published a newer file — one `stat`, at most every `Shares.interval` — and maps it if so; a
// project Fuel has not compiled yet has no file, which is looked for again at the next lookup.
//
// The image carries its schema, so nothing of Fuel's types is repeated here: only the keywords
// `entry`, `address` (its key) and `info`.
object FuelHelp:
  private val tool: Text = t"fuel"
  private val share: Text = t"help"

  // One reader for each project's file, kept for the life of the process.
  private val readers: TrieMap[Text, Shares.Reader] = TrieMap()

  // The info line of the entry at `address` in the image `reader` holds, if there is one.
  def info(reader: Shares.Reader, address: Text): Optional[Text] =
    reader.read((ratel.root / t"entry").find(address).let(_.text(t"info")))

  // The info lines of those of `addresses` which have one, found in one reading of the image:
  // what a list of completions asks, all its candidates at once, since opening the image costs
  // more than finding an entry in it.
  def infos(reader: Shares.Reader, addresses: List[Text]): Map[Text, Text] =
    val found: Optional[List[(Text, Text)]] =
      reader.read:
        val entries = ratel.root / t"entry"

        addresses.bind: (address: Text) =>
          entries.find(address).let(_.text(t"info")).lay(Nil): info => List(address -> info)

    found.lay(Map())(Map(_*))

  // Where Fuel publishes the index of the project `directory` is in: Fuel's project, the one
  // whose `.pyrocosm/fuel/config.tel` is nearest above, which need not be Flame's.
  def file(directory: Text): Optional[Path on Linux] =
    Shares.path(tool, Shares.root(tool, directory), share)

  // The lookup for the project `directory` is in: the info lines Fuel has for some qualified
  // names; none is the usual answer.
  def of(directory: Text): List[Text] -> Map[Text, Text] =
    val reader: Optional[Shares.Reader] =
      file(directory).let: file => readers.getOrElseUpdate(file.encode, Shares.Reader(file))

    (addresses: List[Text]) => reader.lay(Map[Text, Text]())(infos(_, addresses))

  // The lookup for the directory this process runs in, for a front-end which serves no one
  // invocation.
  def here: List[Text] -> Map[Text, Text] = of(java.lang.System.getProperty("user.dir").nn.tt)

  // ── Pages ───────────────────────────────────────────────────────────────────────────────
  // Fuel's index also has a `page` for every entity it documents, whether or not it has an
  // info line: its `address` (the key) and its `url`, the page's path beneath the root of
  // Fuel's site. Where that root is, Fuel says in a second share, `site`, for as long as it is
  // serving the pages and no longer: its `address`, and the `pid` of the process serving. So
  // a name is linked to its page only while there is a page to open.
  private val site: Text = t"site"

  // The page paths of those of `addresses` which have a page, found in one reading.
  def urls(reader: Shares.Reader, addresses: List[Text]): Map[Text, Text] =
    val found: Optional[List[(Text, Text)]] =
      reader.read:
        val pages = ratel.root / t"page"

        addresses.bind: (address: Text) =>
          pages.find(address).let(_.text(t"url")).lay(Nil): url => List(address -> url)

    found.lay(Map())(Map(_*))

  // Whether the process which announced the site is still running: one which was killed
  // leaves its announcement behind.
  private def running(pid: Text): Boolean =
    safely(pid.s.toLong).lay(false): (pid: Long) =>
      val handle: java.util.Optional[ProcessHandle] = ProcessHandle.of(pid).nn
      handle.isPresent && handle.get.nn.isAlive

  // The root of Fuel's site, if Fuel is serving it: the announcement is withdrawn by deleting
  // its file, which a reader that has it mapped would not see, so the file is asked for first.
  def root(file: Path on Linux, reader: Shares.Reader): Optional[Text] =
    if !java.nio.file.Files.exists(java.nio.file.Path.of(file.encode.s)) then Unset else
      val read: Optional[Optional[(Text, Text)]] =
        reader.read:
          ratel.root.text(t"address").let: (address: Text) =>
            ratel.root.text(t"pid").let: (pid: Text) => (address, pid)

      read.let(identity(_)).let: (address: Text, pid: Text) =>
        if running(pid) then address else Unset

  // The lookup of pages for the project `directory` is in: the URL of the page of each of
  // some addresses which Fuel documents, and none at all unless Fuel is serving them.
  def pages(directory: Text): List[Text] -> Map[Text, Text] =
    val project: Text = Shares.root(tool, directory)

    val index: Optional[Shares.Reader] =
      Shares.path(tool, project, share).let: file =>
        readers.getOrElseUpdate(file.encode, Shares.Reader(file))

    val announced: Optional[(Path on Linux, Shares.Reader)] =
      Shares.path(tool, project, site).let: file =>
        (file, readers.getOrElseUpdate(file.encode, Shares.Reader(file)))

    (addresses: List[Text]) =>
      if addresses.nil then Map[Text, Text]() else
        val served: Optional[Text] = announced.let { (file, reader) => root(file, reader) }

        served.lay(Map[Text, Text]()): (root: Text) =>
          val found: Map[Text, Text] = index.lay(Map[Text, Text]())(urls(_, addresses))

          val located: List[(Text, Text)] = addresses.bind: (address: Text) =>
            found.at(address).lay(Nil: List[(Text, Text)]): url => List(address -> t"$root$url")

          Map(located*)

  def pagesHere: List[Text] -> Map[Text, Text] =
    pages(java.lang.System.getProperty("user.dir").nn.tt)
