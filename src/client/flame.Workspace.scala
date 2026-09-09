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

import scala.collection.concurrent.TrieMap

import soundness.*

import filesystemBackends.javaBaseFilesystem
import pathInterfaces.pathOnLinux

// The per-project configuration: a `.pyrocosm/flame/config.tel` file in the invocation's working
// directory or the nearest ancestor holding one — resolved upwards exactly like `.git` (and exactly
// as fume resolves its own `.pyrocosm/fume/config.tel`), so `flame` can be launched from anywhere
// inside a project. `.pyrocosm` is the project's shared directory, one subdirectory per tool.
//
// The file is a TEL document whose keywords mirror the command line, so anything a project wants
// every session to start with can live here instead of being retyped per launch:
//
//   tel 1.0
//   set experimental            # a `/set` setting, as `--experimental` would enable it
//   language captureChecking    # a `/language` feature, as `--captureChecking` would enable it
//   classpath lib/extra.jar     # a JAR or directory `/classload`ed on startup; a RELATIVE path
//   classpath out/classes       # resolves against the PROJECT root (the directory holding
//                               # `.pyrocosm`), wherever `flame` is launched; one entry per line
//   port 4319                   # `--port`
//   host build.example.com      # `--host`
//   join shared                 # `--join`: join this session if it exists
//   create scratch              # `--create`: start a new session with this name
//   history 1000                # keep up to this many prompt-history entries (default 100)
//
// `set`, `language` and `classpath` are ADDITIVE with the flags (a flag and a line naming the same
// setting enable it once); `port`, `host`, `join` and `create` are defaults a flag overrides (and
// naming both a `join` and a `create` is an error, exactly as passing both flags is). A file that
// fails to parse is treated as absent — `flame` never requires one to exist — and, since `flame`
// runs as a daemon, a parsed file is cached against its modification time and size, so an edit is
// honoured by the very next launch.
object Workspace:
  case class Config
    ( sets:      List[Text] = Nil,
      languages: List[Text] = Nil,
      classpath: List[Text] = Nil,
      port:      Optional[Int]  = Unset,
      host:      Optional[Text] = Unset,
      join:      Optional[Text] = Unset,
      create:    Optional[Text] = Unset,
      history:   Optional[Int]  = Unset )

  val empty: Config = Config()

  private case class Cached(modified: Long, size: Long, config: Optional[Tel])

  private val cache: TrieMap[Text, Cached] = TrieMap()

  // The nearest `.pyrocosm/flame/config.tel` at or above `directory`, or `Unset` if no ancestor
  // has one. The FILE is what is sought: a `.pyrocosm` holding only other tools' directories does
  // not end the search.
  def locate(directory: Text): Optional[Path on Linux] =
    safely:
      def recur(dir: Path on Linux): Optional[Path on Linux] =
        val candidate =
          dir / Name[Linux](t".pyrocosm") / Name[Linux](t"flame") / Name[Linux](t"config.tel")
        if candidate.existent() then candidate else dir.parent.let(recur(_))

      recur(directory.as[Path on Linux])

  // The prompt-history configuration governing `directory`: the `history` FILE (only when a
  // `.pyrocosm/flame` directory already exists at or above `directory` — flame never creates it),
  // and the entry `limit` (the config's `history` value, or 100). `flame` appends to and loads from
  // the file (see `flame.History`); when there is no such directory, history is not persisted.
  case class HistoryConfig(file: Optional[Text], limit: Int)

  // The `.pyrocosm/flame/history` path if a `.pyrocosm/flame` directory exists at or above
  // `directory`, resolved upwards exactly as `locate` finds `config.tel`. The FILE itself need not
  // exist yet; the DIRECTORY must (its presence is how a project opts into history persistence).
  def historyPath(directory: Text): Optional[Text] =
    safely:
      def recur(dir: Path on Linux): Optional[Path on Linux] =
        val candidate = dir / Name[Linux](t".pyrocosm") / Name[Linux](t"flame")
        if candidate.existent() then candidate else dir.parent.let(recur(_))

      recur(directory.as[Path on Linux]).let { dir => (dir / Name[Linux](t"history")).encode }

  // The history configuration for `directory`, combining the resolved file path with the config's
  // entry limit (default 100).
  def historyConfig(directory: Text): HistoryConfig =
    HistoryConfig(historyPath(directory), config(directory).history.or(100))

  // Two separately-scoped `safely` regions (the filesystem read, then the TEL parse), as fume's
  // reader does: one region's tactic would be captured by both the path reader and the TEL
  // aggregator, which separation checking rejects.
  private def parse(file: Path on Linux): Optional[Tel] =
    safely(file.read[Data]).let { data => safely(data.read[Tel]) }

  // The parsed document at `file`, stat-checked on every call.
  private def document(file: Path on Linux): Optional[Tel] =
      safely(summon[FilesystemBackend on Linux].stat(file, true)).let: stat =>
        val key: Text = file.encode

        def reload(): Optional[Tel] =
          val parsed: Optional[Tel] = parse(file)
          cache(key) = Cached(stat.modified, stat.size, parsed)
          parsed

        cache.get(key) match
          case Some(cached) if cached.modified == stat.modified && cached.size == stat.size =>
            cached.config

          case _ =>
            reload()

  // The configuration governing `directory`: `empty` when there is no (readable) file. Relative
  // `classpath` entries are made absolute here — against the project root, three levels above the
  // file (`<root>/.pyrocosm/flame/config.tel`) — since the REPL server that `/classload`s them is a
  // background daemon with a working directory of its own.
  def config(directory: Text): Config =
    locate(directory).let { file => document(file).let((file, _)) }.lay(empty): (file, tel) =>
      val root: Text = file.parent.let(_.parent).let(_.parent).let(_.encode).or(directory)

      def atoms(keyword: Text): List[Text] =
        tel.fields(keyword).to[List].map(_.primaryAtom).filter(_ != t"")

      def absolute(entry: Text): Text =
        if entry.starts(t"/") then entry else t"$root/$entry"

      Config
       ( sets      = atoms(t"set"),
         languages = atoms(t"language"),
         classpath = atoms(t"classpath").map(absolute),
         port      = atoms(t"port").prim.let { (text: Text) => safely(text.as[Int]) },
         host      = atoms(t"host").prim,
         join      = atoms(t"join").prim,
         create    = atoms(t"create").prim,
         history   = atoms(t"history").prim.let { (text: Text) => safely(text.as[Int]) } )
