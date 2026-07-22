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

import java.io as ji
import java.lang as jl
import java.net as jn
import java.util.concurrent as juc

import scala.collection.concurrent.TrieMap
import scala.collection.mutable as scm

import soundness.*

import escapade.Faint
import escapade.Italic
import iridescence.WebColors
// `soundness.*` also re-exports an unrelated `Signal` (embarcadero's workload-grant signal), so the
// POSIX terminal `Signal` (whose `.Int` is SIGINT) is named explicitly to resolve the clash.
import profanity.Signal

import backstops.silentBackstop
import classloaders.threadContextClassloader
import executives.completions
import filesystemBackends.virtualMachine
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.disabled
import filesystemOptions.overwritePreexisting.disabled
import harlequin.Accent
import interfaces.paths.pathOnLinux
import internetAccess.online
import interpreters.posixInterpreter
import logging.silentLogging
import probates.cancelProbate
import socketBackends.virtualMachine
import supervisors.globalSupervisor
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading

val Serve = Subcommand("serve", "serve the Flame web front-end")
val Install = Subcommand("install", "install tab-completions into the shell")
val Listen = Subcommand("listen", "serve the terminal REPL over TCP, for remote clients")
val Port = Flag[Int]("port", false, List('p'), "a TCP port — the web front-end, or a remote REPL server")
val Host = Flag[Text]("host", false, List('H'), "connect to a flame REPL server running on this host")
val Session = Flag[Text]("session", false, List('s'), "join an existing REPL session by name")
val SetOpt = Flag[List[Text]]("set", true, Nil, "compiler settings to enable on startup (space-separated)")
val LanguageOpt = Flag[List[Text]]("language", true, Nil, "language features to enable on startup (space-separated)")
val Basic = Flag[Unit]("basic", false, Nil, "run a minimal, in-process, synchronous line-based REPL")

// Exoskeleton's derived `Int is Interpretable` decodes through a `Decodable[Int]` that captures its
// `Tactic[NumberError]`; under capture checking that impure instance cannot satisfy `Flag.apply`'s
// pure `Interpretable` requirement. This local, pure interpreter parses the argument with `safely`,
// yielding `Unset` for an absent or non-numeric value — so a `Port().or(default)` read falls back
// exactly as the old `recover`/`protect` on `NumberError` did, without a tracked capability.
given (Int is Interpretable) = new Interpretable:
  type Self = Int
  def interpret(arguments: List[Argument]): Optional[Int] =
    arguments.prim.lay(Unset): value =>
      safely(value().as[Int])

// `execute` runs its block with an ambient `Invocation`/`DaemonService` whose derived capabilities
// (`Stdio`, `Console`, `Environment`) are tracked; flame's command bodies take them as pure `using`
// parameters. Seal all three once here — they outlive the command they drive — so every command body
// type-checks under capture checking without a per-site cast. The capabilities are threaded through
// `body`'s context-function type so they resolve to the sealed givens below (not the capturing ones
// at the call site). Replaces a bare `execute(body)` at each dispatch arm.
private inline def command
   (inline body: (erased Effectful) ?=> (Stdio, Console, Environment) ?=> Exit)(using Cli)
:   Execution =
  execute:
    given Stdio = caps.unsafe.unsafeAssumePure(summon[Invocation].stdio)
    given Console = caps.unsafe.unsafeAssumePure(summon[Console])
    given Environment = caps.unsafe.unsafeAssumePure(summon[Invocation].environment)
    body

// Reads a repeatable settings flag, offering the `/set`/`/language` feature names of `kind` as
// tab-completions for its value. The `Interpretable`/`Discoverable` givens are scoped to this read so
// the two flags' different suggestion lists don't clash.
private def settingValues(flag: Flag of List[Text], kind: Repl.Kind)(using Cli, Interpreter): List[Text] =
  given (List[Text]) is Interpretable = _.map(_())
  given (List[Text]) is Discoverable =
    _ => Repl.settings.filter(_.kind == kind).map { setting => Suggestion(setting.name) }
  flag().or(Nil)

// The `/set`/`/language` commands to run on startup, from `--set`/`--language` (compiler settings
// applied first, so `--set experimental` unlocks any experimental `--language` features).
private def initialSettings(using Cli, Interpreter): List[Text] =
  settingValues(SetOpt, Repl.Kind.Set).map { name => t"/set $name" }
  ::: settingValues(LanguageOpt, Repl.Kind.Language).map { name => t"/language $name" }

// The default TCP port for `flame listen` (a remote-reachable REPL server) and for `flame --host`
// when no `--port` is given. Arbitrary, but stable so the two ends agree without configuration.
val defaultPort: Int = 4319

// The client's command dispatch. The `@main` entry point and burdock's `externalize` wrapper live
// alone in the `launcher` module (`src/launcher/flame_launcher.scala`), which depends on this module
// (and `core`/`web`) as PUBLISHED Maven artifacts — so `externalize` records their Central jar hashes
// and the repackager turns them into on-demand `Burdock-Require` downloads instead of inlining them.
def runClient(): Unit =
  cli:
    arguments match
      // `flame serve [--port N | -p N]` — the web front-end (default port 8080). `Port()` registers
      // the flag (so it is offered in tab-completion) and reads its value; the pure `Int`
      // interpreter above yields `Unset` for an absent or non-numeric value, so `.or(8080)` falls back.
      case Serve() :: _ =>
        command(httpServe(Port().or(8080)))

      // `flame install` — install this command's tab-completions into the user's shell.
      case Install() :: _ =>
        command(installCompletions())

      // `flame listen [--port N | -p N]` — a REPL server bound to a TCP port (not a per-process UNIX
      // socket), so a `flame --host` client on another machine can reach its sessions. Defaults to
      // `defaultPort`; a non-numeric `--port` also falls back to it.
      case Listen() :: _ =>
        command(serve(Port().or(defaultPort)))

      // Internal: the per-process UNIX-socket REPL server that `connectSocket`/`launchServer`
      // spawns in the background (see `launchServer`). Not a user-facing command — `serve` now
      // serves the web front-end — so it uses a distinct argument the launcher passes itself.
      case Argument("serve-socket") :: Nil =>
        command(serveSocket())

      // `flame` — the terminal REPL (connects to, or starts, a background socket server). Bare `flame`
      // starts a new session.
      case Nil =>
        command(connectSocket(Session(), Nil))

      // `flame -<flag>…` — the terminal REPL with options: `-s NAME` joins a session, `--host HOST`
      // (with optional `--port`) connects to a remote server (`flame listen`), and `--set`/`--language`
      // enable settings on startup. This case fires only when the first token is a FLAG (`head` begins
      // with `-`), so it reads the flags there rather than in the catch-all — a flag read in a
      // completion-visible catch-all would suppress the subcommand suggestions for the bare first word,
      // whereas here the subcommands are already ruled out (the word starts with `-`), and their
      // completions (`--set`/`--language` values etc.) are exactly what should be offered.
      case Argument(head) :: _ if head.starts(t"-") =>
        val settings: List[Text] = initialSettings

        // `flame --basic` — a minimal, self-contained, in-process synchronous REPL (no socket
        // server, no TUI). Checked before `--host`, so it never falls through to a remote/socket
        // connection; it still honours `--set`/`--language` startup settings.
        if Basic().present then command(basicRepl(settings))
        else Host() match
          case host: Text =>
            command(connectRemote(host, Port().or(defaultPort), Session(), settings))

          case _ =>
            command(connectSocket(Session(), settings))

      case _ =>
        command(Exit.Fail(1))

// Runs a REPL server on the given TCP port and blocks until interrupted.
private def serve(portNumber: Int)(using Stdio, Monitor, Probate, System): Exit =
  given Scalac[3.8, Universe.Classfile] = Scalac(Nil)
  given Classloader = serverClassloader

  safely(urticose.Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
    recover:
      case BindError(_) => Out.println(t"flame: port $portNumber is unavailable"); Exit.Fail(5)
      case error: Error => Out.println(t"flame: ${error.message}"); Exit.Fail(6)

    . protect:
        val sessions = Sessions()
        val service  = sessions.serve(port)
        Out.println(t"flame: serving a REPL on port $portNumber (Ctrl+C or /quit to stop)")
        sessions.awaitQuit()
        service.stop()
        Exit.Ok

// `flame --basic`: a minimal, self-contained, synchronous, line-based REPL — no socket server, no
// line editor, no completion, no async. It reads one line at a time from the (daemon-forwarded)
// stdin, accumulating lines while the buffer is still a syntactically-incomplete prefix (so a
// multi-line definition can be entered — the `| ` continuation prompt), then evaluates each complete
// submission in-process with `Repl.react` (which is inherently synchronous) and prints the same
// plain-text block the terminal front-end shows below a line (`replyText`). The engine renders in
// `Rendering.Inspect` (teletype/text) mode by default. `--set`/`--language` startup settings are
// applied first; `/quit` (or Ctrl+D / EOF) ends the session. `async` has no effect here (every
// submission runs synchronously), so `/set async` is intercepted with a short notice.
private def basicRepl(settings: List[Text])(using Stdio, Monitor, Probate, System, DaemonService[?]): Exit =
  given Scalac[3.8, Universe.Classfile] = Scalac(Nil)
  given Classloader = serverClassloader

  val repl:   Repl[3.8]         = Repl.make[3.8](Repl.Prelude.empty)
  val reader: ji.BufferedReader = ji.BufferedReader(ji.InputStreamReader(summon[Stdio].in, "UTF-8"))
  var id:     Int              = 0
  var buffer: Text             = t""

  // `Out.print` writes through a non-auto-flushing `PrintStream`, so flush after a prompt to be sure
  // it reaches the terminal before `readLine` blocks.
  def prompt(text: Text): Unit =
    Out.print(text)
    summon[Stdio].out.flush()

  def submit(code: Text): Unit =
    id += 1
    Out.print(replyText(repl.react(id, code)))

  // Ask the Ethereal launcher for a COOKED (canonical) terminal for the whole session, so the
  // terminal driver itself provides character echo and line editing (Backspace, kill-line, …) — the
  // launcher otherwise raw-modes a terminal stdin to forward keypresses to an interactive TUI, which
  // left `--basic` with no echo (Soundness #1648/#1651). A no-op for a pipe, and harmless against a
  // launcher too old to offer the control channel (it just stays in raw mode as before).
  summon[DaemonService[?]].cooked:
    // Apply the `--set`/`--language` startup settings, so each confirmation prints before the first prompt.
    settings.each(submit)
    Out.println(Repl.messages.session(t"basic"))

    var running: Boolean = true

    while running do
      prompt(if buffer == t"" then t"> " else t"| ")

      Optional(reader.readLine()) match
        case Unset =>
          // EOF (Ctrl+D or a closed pipe): end the session on a fresh line.
          Out.println()
          running = false

        case line: String =>
          val text: Text = line.tt

          if buffer == t"" && text.trim == t"/quit" then running = false
          else if buffer == t"" && text.trim == t"/set async" then
            Out.println(t"flame: async mode is not available in --basic")
          else
            val code: Text = if buffer == t"" then text else t"$buffer\n$text"

            // A `/`-command (at the start of a submission) is a REPL command, not Scala, so it always
            // submits — never run it through the Scala-incompleteness check (which would treat e.g.
            // `/classpath` as an incomplete prefix). Otherwise keep accumulating while the code is an
            // incomplete prefix; a complete line runs, and a malformed one also submits (surfacing its
            // compiler error), matching the terminal front-end's Enter semantics.
            val command: Boolean = buffer == t"" && text.starts(t"/")

            if !command && Repl.incomplete(code) then buffer = code else
              buffer = t""
              submit(code)

  Exit.Ok

// Launches the web-based front-end (`flame.web`) on the given TCP port, blocking until
// interrupted. It reuses `serverClassloader` — the same compile-classpath loader the
// socket server uses — so the embedded engine finds the scala library under the
// Burdock/Ethereal launcher, exactly as the terminal server does.
private def httpServe(portNumber: Int)
   (using Stdio, Monitor, Probate, System, Cli, Console, Environment)
:   Exit =

  given Classloader = serverClassloader

  val quit: Promise[Unit] = Promise()

  // A real SIGINT (e.g. `kill -INT <pid>`) fulfils `quit` and stops the server cleanly.
  trap:
    case Signal.Int => quit.offer(()) yet SignalResponse.Accept

  // Printed from the client (not the daemon) so it reaches the user's terminal; the URL is
  // on its own line so terminals render it as a clean, clickable link.
  Out.println(t"flame: serving the web REPL (press Ctrl+C to stop):")
  Out.println(t"  http://localhost:$portNumber/")

  // A TYPED Ctrl+C never reaches the `trap`: the Ethereal launcher reads the terminal in raw mode
  // and forwards the keystroke as a byte (never a SIGINT), and there is no editor to consume it. So
  // read the terminal ourselves — in the background, while the server blocks on `quit` — and fulfil
  // `quit` on Ctrl+C (or Ctrl+D). This is why the server never responded to Ctrl+C before.
  async:
    safely:
      interactive: terminal ?=>
        given Stdio = terminal.stdio
        val events = terminal.eventIterator()
        var reading = true

        while reading && events.hasNext do events.next() match
          case Keypress.Ctrl('C' | 'D') => quit.offer(()); reading = false
          case _                        => ()

  serveHttp(portNumber, quit)
  Exit.Ok

// Installs this command's shell tab-completions (`flame install`) — the zsh/bash/fish completion
// script that calls `flame '{completions}' …`, driven by the subcommand/flag tree the CLI registers
// (`Serve`/`Install`/`WebPort`). Exoskeleton's `Completions.ensure` does the write (the same call the
// built-in `{admin} install` uses); it needs an `Entrypoint`, which the ambient Ethereal
// `DaemonService` supplies (it extends `Entrypoint`). `force` installs even when `flame` is not yet
// on the `PATH`, so a freshly-built binary can set completions up before it is installed as a command.
private def installCompletions()(using stdio: Stdio, service: DaemonService[?])(using erased Effectful)
:   Exit =

  import errorDiagnostics.stackTracesDiagnostics
  import workingDirectories.javaWorkingDirectory

  // The `DaemonService` (which extends `Entrypoint`) carries the daemon's tracked capabilities; seal
  // it to the pure `Entrypoint` the completions installer wants — it outlives this one call.
  given Entrypoint = caps.unsafe.unsafeAssumePure(service)

  recover:
    case error: InstallError =>
      Out.println(t"flame: could not install tab-completions")
      Exit.Fail(8)

  . protect:
      Completions.ensure(force = true).each(Out.println(_))
      Exit.Ok

// The directory holding per-process REPL sockets, and this process's socket file.
// UNIX domain sockets are a Unix-only feature, so the directory follows
// `$XDG_RUNTIME_DIR` (then `$TMPDIR`, then `/tmp`) directly, as plain `Text`.
private def envText(name: String): Optional[Text] = Optional(jl.System.getenv(name)).let(_.nn.tt)

private def socketDirectory: Text =
  t"${envText("XDG_RUNTIME_DIR").or(envText("TMPDIR")).or(t"/tmp")}/flame"

private def socketFile: Text = t"$socketDirectory/${ProcessHandle.current.nn.pid}.sock"

// Runs a REPL server on a per-process UNIX domain socket (used when no port is
// given) and blocks until quit, unlinking the socket file on the way out.
private def serveSocket()(using Stdio, Monitor, Probate, System): Exit =
  given Scalac[3.8, Universe.Classfile] = Scalac(Nil)
  given Classloader = serverClassloader

  val socketPath: Text = socketFile

  // Delete the socket on any exit (a clean stop, a crash, or a kill signal), so it
  // never lingers as a stale file a later client has to clean up.
  val removeSocket: Runnable = () =>
    recover:
      case _: PathError | _: IoError => ()
    . protect:
      socketPath.as[Path on Linux].wipe()
      ()

  jl.Runtime.getRuntime.nn.addShutdownHook(jl.Thread(removeSocket))

  try
    // Prepare the socket directory and clear any stale socket, ignoring filesystem errors (a failure
    // here just surfaces as `serve` failing below). `create[Directory]` is not idempotent (it fails
    // if the directory exists), unlike `Files.createDirectories`, so the directory (which persists
    // across processes) is created only when absent.
    recover:
      case _: PathError | _: IoError => ()
    . protect:
      val directory = socketDirectory.as[Path on Linux]
      if !directory.exists() then directory.create[Directory]()
      socketPath.as[Path on Linux].wipe()

    val sessions = Sessions()
    val service  = sessions.serve(socketPath)
    Out.println(t"flame: serving a REPL on $socketPath (Ctrl+C or /quit to stop)")
    sessions.awaitQuit()
    service.stop()

    recover:
      case _: PathError | _: IoError => ()
    . protect:
      socketPath.as[Path on Linux].wipe()

    Exit.Ok
  catch case error: Throwable =>
    Out.println(t"flame: could not serve on $socketPath: ${error.toString.tt}")
    Exit.Fail(6)

// Builds the classloader the REPL compiles against inside the Ethereal daemon.
//
// Under the Burdock launcher the daemon runs with a bootstrap `URLClassLoader`
// (the thread-context loader) whose entries are this executable plus the
// externalized dependency jars — the scala library, the compiler, the soundness
// modules — fetched into `~/.cache/burdock`. dotc needs all of them on its compile
// classpath. The fetched jars already carry `.jar` paths it can read, but the
// executable entry is a shebang-prefixed file with no `.jar` suffix that dotc
// refuses to read (a `ZipFile` still reads the appended archive past the shebang),
// so we symlink it to a `.jar` path and substitute the symlink for it.
//
// We hand the REPL a *parent-first* `URLClassLoader` over [symlink ++ fetched jars]
// with the bootstrap loader as parent: the jars land on dotc's compile classpath,
// while parent-first delegation keeps runtime classes (notably `ReplBridge`, whose
// session registry must be shared) identical to the daemon's. Outside the daemon
// (e.g. `ethereal.script` unset, or a non-`URLClassLoader` launcher) we fall back
// to the thread-context loader.
private def serverClassloader(using System): Classloader =
  // `recover`/`protect` handles the filesystem errors the path operations RAISE; the outer `try`
  // catches anything the classloader machinery THROWS (a missing `ethereal.script`, a non-URL
  // launcher). Either way we fall back to the thread-context loader.
  try
    recover:
      case _: PathError | _: IoError => threadContextClassloader
    . protect:
      val executable: Text = unsafely(System.properties.ethereal.script[Text]())
      val tmpDir: Path on Linux = temporaryDirectory/Uuid()
      tmpDir.create[Directory]()
      val link: Path on Linux = unsafely(tmpDir/t"flame.jar")
      executable.as[Path on Linux].symlinkTo(link)
      // `.javaFile`/`.javaPath` are re-exported into `soundness` from both galilei-core and
      // galilei-jvm (an orphan-given clash), so the extension search is ambiguous; build the
      // `java.io.File` from the path's encoded text instead.
      val url: jn.URL = ji.File(link.encode.s).toURI.nn.toURL.nn

      val executablePath: Text = ji.File(executable.s).getCanonicalPath.nn.tt
      val bootstrapUrls: List[jn.URL] = threadContextClassloader.java match
        case loader: jn.URLClassLoader =>
          loader.getURLs.nn.iterator.to(List).map(_.nn).filter: entry =>
            safely(ji.File(entry.toURI).getCanonicalPath.nn.tt) != executablePath
        case _ =>
          Nil

      new Classloader(jn.URLClassLoader((url :: bootstrapUrls).toArray, threadContextClassloader.java))
  catch case _: Throwable => threadContextClassloader

private def invalidPort(portNumber: Int)(using Stdio): Exit =
  Out.println(t"flame: $portNumber is not a valid TCP port")
  Exit.Fail(2)

private def missingHost(using Stdio): Exit =
  Out.println(t"flame: --host needs the hostname of a flame server to connect to")
  Exit.Fail(2)

private def invalidHost(host: Text)(using Stdio): Exit =
  Out.println(t"flame: '$host' is not a valid hostname")
  Exit.Fail(2)

private def unreachableRemote(host: Text, portNumber: Int)(using Stdio): Exit =
  Out.println(t"flame: could not connect to $host:$portNumber")
  Exit.Fail(3)

// Runs `body` over a fresh TCP connection to `endpoint` — always closing it afterwards —
// or returns `Unset` if the connection is refused. `Connectable.tcpEndpoint` is honestly tracked
// (`^{online, caps.any}`), and its `caps.any` cannot flow into the fresh capture variable the
// `duplex` loan introduces. The loan confines the connection to `body`, so a pure connectable is
// sound here: seal it for this call. (The domain-socket connectable carries no `caps.any`, so
// `connectDomain` needs no such seal.)
private def connect[result](endpoint: Endpoint[TcpPort])(body: Duplex => result): Optional[result] =
  given (Endpoint[TcpPort] is Connectable) = caps.unsafe.unsafeAssumePure(Connectable.tcpEndpoint)
  try endpoint.duplex(body) catch case _: ji.IOException => Unset

// As `connect`, but over a UNIX domain socket.
private def connectDomain[result](socket: DomainSocket)(body: Duplex => result): Optional[result] =
  try socket.duplex(body) catch case _: ji.IOException => Unset

private def unreachableSocket(path: Text)(using Stdio): Exit =
  Out.println(t"flame: could not connect to $path")
  Exit.Fail(3)

private def failedToLaunch(using Stdio): Exit =
  Out.println(t"flame: could not start a REPL server")
  Exit.Fail(3)

// Starts a REPL server in the background — a detached `flame serve-socket` process on its
// own per-process domain socket — waits for it to bind, and connects to it. Because
// the server is a separate process it outlives this client, so the same session can
// be reconnected to later by running `flame` again. Returns the live connection,
// or `Unset` if no server became reachable in time.
private def launchServer[result]()(using Stdio, System)(body: Duplex => result): Optional[result] =
  safely(System.properties.ethereal.script[Text]()).lay(Unset): executable =>
    val before: List[Text] = socketPaths(socketDirectory)

    val builder = jl.ProcessBuilder(executable.s, "serve-socket")
    builder.redirectOutput(jl.ProcessBuilder.Redirect.DISCARD)
    builder.redirectError(jl.ProcessBuilder.Redirect.DISCARD)
    builder.redirectInput(ji.File("/dev/null"))
    safely(builder.start())

    var result: Optional[result] = Unset
    var waited: Int              = 0

    // Poll for a new, connectable socket (the socket file appears once it binds), then
    // run `body` over the first one that connects.
    while result.absent && waited < 10000 do
      jl.Thread.sleep(100)
      waited += 100

      socketPaths(socketDirectory).each: candidate =>
        if result.absent && !before.contains(candidate) then
          result = connectDomain(DomainSocket(candidate))(body)

    result

// TEMPORARY keyboard diagnostic. Instead of the editor, print each terminal event
// as a Profanity `Keypress` (or other `TerminalEvent`), so we can see exactly how
// keys — including Shift+Enter — are decoded. With `kitty = true` the kitty
// keyboard protocol is enabled first. Ctrl+C or Ctrl+D stops it.
private def keyTest(kitty: Boolean)(using Stdio, Monitor, Probate, Console, Environment): Exit =
  recover:
    case TerminalError() =>
      Out.println(t"flame: the terminal could not be initialised")
      Exit.Fail(4)

  . protect:
      interactive: terminal ?=>
        given Stdio = terminal.stdio
        if kitty then Out.print(t"\e[>1u")
        Out.print(t"flame: press keys to see how they decode; Ctrl+C or Ctrl+D to stop\r\n")
        val events = terminal.eventIterator()
        var running = true

        try
          while running && events.hasNext do
            val event = events.next()

            val rendered = event match
              case keypress: Keypress => keypress.show
              case other              => other.toString.tt

            Out.print(t"$rendered\r\n")

            event match
              case Keypress.Ctrl('C' | 'D') => running = false
              case _                        => ()
        finally
          if kitty then Out.print(t"\e[<u")

        Exit.Ok

// The full paths of the `.sock` files in the socket directory (an absent directory has no children,
// so this is empty on the first run; an undecodable directory path yields nothing).
private def socketPaths(directory: Text): List[Text] =
  recover:
    case _: PathError => Nil
  . protect:
    directory.as[Path on Linux].children.map(_.encode).filter(_.ends(t".sock")).to(List)

// Connects to a per-process UNIX domain socket. With no server running, launches one
// in the background and attaches to it (so `flame` alone is a self-contained REPL,
// reconnectable later); with exactly one, connects to it; with several, lists them.
private def connectSocket(join: Optional[Text], initial: List[Text])
    (using Stdio, Monitor, Probate, Console, Environment, System)
:   Exit =
  // Probe every socket file: a connectable one is live; one that refuses (a crashed or
  // killed server that never cleaned up) is a stale leftover, so delete it. Then attach
  // to the lone live server, list several, or — when none survive — start a fresh one.
  val live: scm.ArrayBuffer[Text] = scm.ArrayBuffer()

  socketPaths(socketDirectory).each: path =>
    if connectDomain(DomainSocket(path)) { _ => () }.absent
    then
      recover:
        case _: PathError | _: IoError => ()
      . protect:
        path.as[Path on Linux].wipe()
    else live += path

  live.to(List) match
    case Nil =>
      Out.println(t"flame: starting a REPL server…")
      launchServer()(converse(join, initial)(_)).or(failedToLaunch)

    case path :: Nil =>
      connectDomain(DomainSocket(path))(converse(join, initial)(_)).or(unreachableSocket(path))

    case paths =>
      Out.println(t"flame: several REPL servers are running:")

      paths.each: path =>
        Out.println(t"  $path")

      Out.println(t"flame: stop all but one, or connect to a specific one with 'flame --host localhost'")
      Exit.Fail(7)

// Connects the terminal REPL to a flame server on another host over TCP (`flame --host HOST
// [--port N]`, reaching a server started with `flame listen`), optionally joining a named session
// with `-s`/`--session`. The transport differs from `connectSocket` (a TCP endpoint rather than a
// local UNIX domain socket), but the conversation — the `converse`/`runRepl` loop — is identical, so
// a remote session behaves exactly like a local one.
private def connectRemote(host: Optional[Text], portNumber: Int, join: Optional[Text], initial: List[Text])
    (using Stdio, Monitor, Probate, Console, Environment, System)
:   Exit =
  host.lay(missingHost): hostText =>
    safely(urticose.Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
      recover:
        case HostnameError(_, _) => invalidHost(hostText)
      . protect:
          val endpoint: Endpoint[TcpPort] = hostText.as[Hostname] via port
          connect(endpoint)(converse(join, initial)(_)).or(unreachableRemote(hostText, portNumber))

// The read/edit/print loop. The server's reply is printed verbatim. Ctrl+C/Ctrl+D
// dismiss the line editor (`DismissError`) and end the session.
private def converse(join: Optional[Text], initial: List[Text])(duplex: Duplex)
    (using Stdio, Monitor, Probate, Console, Environment)
:   Exit =
  // The kitty keyboard protocol (applied by `interactive`) makes the terminal report
  // Shift+Enter distinctly, so the editor can submit on it.
  import terminalFeatures.kittyKeyboardFeature

  val state       = LiveState()
  val pending     = TrieMap[Int, Int]()                  // tokenize id → version
  val submits     = juc.LinkedBlockingQueue[Repl.Reply]()
  val completions = juc.LinkedBlockingQueue[List[Repl.CompletionItem]]()
  val nextId      = juc.atomic.AtomicInteger(1)          // 0 is reserved for submit

  recover:
    case TerminalError() =>
      Out.println(t"flame: the terminal could not be initialised")
      Exit.Fail(4)

  . protect:
      interactive: terminal ?=>
        runRepl(duplex, state, pending, nextId, submits, completions, join, initial)
        Exit.Ok

// One REPL input line is an inline block at the bottom of the console — a bordered,
// live-highlighted editor with, when completions are active, a pane below it listing
// them. On submit the block is frozen in place (`finish`), the result is printed below
// it, and the next line opens a fresh block beneath, so output scrolls up into the
// console's own scrollback above the editor. Rendering goes through ultimatum's
// `paint`/`InlineRoot`, so the editor's syntax colours and wide-character widths are
// exactly those of the cell grid.
private def runRepl
  ( duplex:      Duplex,
    state:       LiveState,
    pending:     TrieMap[Int, Int],
    nextId:      juc.atomic.AtomicInteger,
    submits:     juc.LinkedBlockingQueue[Repl.Reply],
    completions: juc.LinkedBlockingQueue[List[Repl.CompletionItem]],
    join:        Optional[Text],
    initial:     List[Text] )
  ( using terminal: Terminal, monitor: Monitor )
:   Unit =

  given Stdio = terminal.stdio

  // `duplex.source` blocks on its first socket read, so force the iterator lazily —
  // only once a request has been sent — otherwise it would deadlock before the editor
  // even starts (the server sends nothing until it receives a message). `toLazyList` is
  // zephyrine's legacy view of the kernel read stream: one materialized chunk per refill.
  lazy val chunks: Iterator[Data] = duplex.source.toLazyList.iterator
  @volatile var live = true

  // Replies to session queries/switches (the server only sends a `Session` reply in answer to a
  // `Session` request, so this queue is drained 1:1 by whoever sent the request).
  val sessionReplies: juc.LinkedBlockingQueue[Repl.Reply.Session] = juc.LinkedBlockingQueue()

  // Inline autosuggestion ("ghost text"): on each edit we ask the server (with a non-zero
  // id, to distinguish from a Tab completion's id 0) for completions at the cursor; the
  // reply, paired with the value it was for, is stashed here and a redraw posted. The
  // editor renders the unique completion's remainder (or the common stem + "...") faint.
  @volatile var ghostReply: Optional[(Text, List[Repl.CompletionItem])] = Unset
  val ghostPending: TrieMap[Int, Text] = TrieMap()

  // The server's latest verdict on whether the current line is an incomplete prefix (so
  // Enter continues onto a new line) or ready to submit, paired with the value it was
  // computed for. Read synchronously by the Multiline submit predicate; until a verdict for
  // the current value arrives, the predicate falls back to a cheap bracket check.
  @volatile var incompleteFor: Optional[(Text, Boolean)] = Unset
  val incompletePending: TrieMap[Int, Text] = TrieMap()

  // Async mode (`/set async`): a submission whose result is not ready within the server's grace window
  // is answered with a `Pending` placeholder; the real reply arrives out-of-band later, carrying the
  // submission's id. `asyncPending` holds ids awaiting a fill; `asyncEntries` maps an id to its
  // placeholder transcript entry so the fill updates it in place; `asyncResults` stashes a fill that
  // races ahead of the entry being recorded (essentially impossible — a fill is at least a grace window
  // away — but cheap to handle). A fill mutates the entry's `result`, sets `asyncReplay`, and posts a
  // `Redraw`, which replays the transcript with the filled result in the placeholder's position.
  val asyncPending: TrieMap[Int, Unit]            = TrieMap()
  val asyncEntries: TrieMap[Int, TranscriptEntry] = TrieMap()
  val asyncResults: TrieMap[Int, Text]            = TrieMap()
  // Stdout streamed (async mode) for an in-flight submission, accumulated as `Output` chunks arrive so
  // it shows live under the "evaluating" panel; only the (single) reader appends, so `Text` is safe.
  val asyncOutput:  TrieMap[Int, Text]            = TrieMap()
  @volatile var asyncReplay: Boolean              = false

  // The live display for a still-running async submission: its stdout streamed so far, then a faint
  // "evaluating" line. Replaced by the full result once the final reply arrives.
  def evaluating(id: Int): Text =
    t"${asyncOutput.getOrElse(id, t"")}\e[2m⋯ evaluating…\e[22m\n"

  // What Enter does depends on how much has been typed:
  //  - a SINGLE line submits as soon as it is a COMPLETE (not incomplete-prefix) statement or
  //    expression; while it is still an incomplete prefix (`def f =`, `{`, `1 +`, …), Enter instead
  //    opens a new line to continue it.
  //  - a MULTI-LINE entry submits only once a BLANK line is left at the end (i.e. press Enter twice),
  //    so a deliberately multi-line block is never submitted before the user is done. The trailing
  //    blank line is stripped on submit (see the event loop), so the frozen box and the transcript
  //    render tight.
  // Shift+Enter always submits (profanity's `Multiline`). A line beginning `/` is a REPL command,
  // not Scala (the engine would parse `/clear`, `/quit`, … as incomplete Scala), so it always submits.
  def readyToSubmit(text: Text): Boolean =
    if text.starts(t"/") then true
    else if text.contains(t"\n") then blankLastLine(text) && text.trim != t""
    else notIncomplete(text)

  // The single-line completeness verdict: the server parser's authoritative result for this exact
  // text, or — until it arrives (and for the empty line, never tokenized) — the local heuristic, so a
  // clearly-incomplete first line (`def f =`) still continues onto a new line on a cold server.
  def notIncomplete(text: Text): Boolean =
    incompleteFor.lay(singleLineComplete(text)): verdict =>
      val (value, incomplete) = verdict
      if value == text then !incomplete else singleLineComplete(text)

  // Background reader: route replies by kind. A `tokenize` reply refines the live
  // highlight and posts a `Redraw`, so the refined colour appears as soon as the server
  // answers — not one keypress later. Other replies are a `submit` result the loop
  // awaits, or the `completions` the Tab handler blocks on.
  async:
    // Construct the reader here, not on the main thread: forcing `chunks`
    // (`duplex.source.toLazyList.iterator`) blocks until the first byte arrives, and the main
    // thread must stay free to render the editor and drive the event loop.
    val frames: FrameReader = new FrameReader(chunks)

    while live do
      val data: Optional[Data] = frames.next()

      if data.absent then live = false
      else safely(Bintel.read[Repl.Reply](data.vouch)).let:
        case Repl.Reply.Tokenized(id, highlight, incomplete) =>
          pending.remove(id).foreach(state.reconcile(_, highlight))
          incompletePending.remove(id).foreach { value => incompleteFor = (value, incomplete) }
          terminal.events.put(TerminalInfo.Redraw)

        case Repl.Reply.Completed(id, items) =>
          if id == 0 then completions.put(items)
          else ghostPending.remove(id).foreach: forValue =>
            ghostReply = (forValue, items)
            terminal.events.put(TerminalInfo.Redraw)

        case reply: Repl.Reply.Session =>
          sessionReplies.put(reply)

        // The placeholder ack for an async submission: note its id, then unblock the waiting submit
        // (which shows the "evaluating" panel). The real reply follows later with the same id.
        case reply @ Repl.Reply.Pending(id) =>
          asyncPending(id) = ()
          submits.put(reply)

        // A streamed chunk of an async run's stdout: accumulate it and, if the transcript entry is
        // recorded, refresh its live display and replay so the output appears as it is produced. (If
        // the entry isn't recorded yet, the submit loop applies the accumulated output when it files it.)
        case Repl.Reply.Output(id, chunk) =>
          asyncOutput(id) = asyncOutput.getOrElse(id, t"") + chunk

          asyncEntries.get(id).foreach: entry =>
            entry.result = evaluating(id)
            asyncReplay = true
            terminal.events.put(TerminalInfo.Redraw)

        // An out-of-band fill for an async submission (id previously seen via `Pending`): update the
        // matching transcript entry — or stash the text if the entry isn't recorded yet — and replay.
        case reply if asyncPending.contains(replyId(reply)) =>
          val id = replyId(reply)
          asyncPending.remove(id)
          asyncOutput.remove(id)
          val text = replyText(reply)

          asyncEntries.remove(id) match
            case Some(entry) => entry.result = text
            case None        => asyncResults(id) = text

          asyncReplay = true
          terminal.events.put(TerminalInfo.Redraw)

        case reply =>
          submits.put(reply)

  // Start this connection's session: join the one named by `-s`/`--session` if it exists, otherwise
  // let the server assign a fresh animal name. Show the resulting session, noting when a requested
  // name was not found (the server then started a new one instead).
  val joinName: Text = join.or(t"")
  duplex.send(zephyrine.Stream(framed(encode(Repl.Request.Session(nextId.getAndIncrement, joinName)))))

  safely(sessionReplies.take().nn).let: reply =>
    if joinName != t"" && reply.name != joinName
    then Out.println(Repl.messages.joinFailed(joinName, reply.name))
    else Out.println(Repl.messages.session(reply.name))

  // Apply the startup settings from `--set`/`--language` by submitting each as a command to the
  // session, printing its confirmation (`flame: X enabled`) below the banner. Synchronous — async mode
  // is off at startup — so each reply lands on `submits` in order.
  initial.each: command =>
    duplex.send(zephyrine.Stream(framed(encode(Repl.Request.Submit(nextId.getAndIncrement, command)))))
    safely(submits.take().nn).let { reply => Out.print(replyText(reply)) }

  val events = terminal.eventIterator()
  var running = true

  // The inline block's rendering policy (ultimatum). `Inline` renders the editor RELATIVE to the
  // cursor, so each line's block appears exactly where output has flowed to — right after the
  // previous result — and never docks to a fixed screen edge. This makes the REPL flow like an
  // ordinary terminal: submit → result → next prompt, always tight, with unused space simply being
  // blank screen below. It also gives the desired edit-time behaviour for free: when the editor (or
  // its completion pane) shrinks, the inline shrink holds the block's top and clears the rows below,
  // so the box stays put rather than jumping — no reserved-height workaround needed. Early in a
  // session the block sits mid-screen (wherever the cursor is); once output fills the screen it
  // naturally rides the bottom, scrolling like any REPL. It also governs the static replay boxes.
  given InlineAnchoring = InlineAnchoring.Inline

  // Resize hysteresis: each `WindowSize` bumps this generation and schedules a settle check;
  // only the latest generation, once no newer resize has arrived, redraws — so a drag that
  // fires many events collapses to a single redraw once the terminal size stops changing.
  // `resizeReplay` tells the `Redraw` handler that this redraw is the settle: replay the
  // transcript from the top before drawing the editor, rather than just repainting in place.
  @volatile var resizeGen: Int = 0
  @volatile var resizeReplay: Boolean = false

  // The session transcript: one entry per submitted line, holding what is needed to re-render
  // it from scratch — the source text (to compute the box's wrapped height at any width), the
  // highlight tokens (to redraw the box), and the result text printed below it. Kept so a resize
  // can wipe the reflowed screen and lay the whole session out afresh at the new width.
  val transcript: scm.ArrayBuffer[TranscriptEntry] = scm.ArrayBuffer()

  // Re-render the whole session from the top at the current width. A resize reflows all prior
  // on-screen content unpredictably, so instead of reconciling it we clear the screen AND its
  // scrollback (`\e[3J`), then replay each transcript entry — the box reflows to the new width,
  // the result is reprinted verbatim — leaving the cursor right after the last result, where the
  // caller then draws the live editor. Each box is a throwaway static `InlineRoot` (`Inline`
  // anchoring, so it flows at the cursor and drops the cursor below itself, exactly like a live
  // submission did). Overflow scrolls into the (freshly cleared) scrollback, one clean copy.
  def replay(): Unit =
    Out.print(t"\e[2J\e[3J\e[H")
    val innerWidth = (terminal.knownColumns - 4).max(1)

    transcript.each: entry =>
      val rows = LineEditor.cursorPosition(entry.text, entry.text.length, innerWidth)._1 + 1
      val staticRoot = InlineRoot(terminal)
      paint(staticRoot, replayBox(entry.tokens, rows))
      staticRoot.finish()
      if entry.result != t"" then Out.print(entry.result)

  // Shell-style command history, like the web front-end: Up/Down cycle through submitted
  // lines. `histIdx == history.length` is the current draft; the draft is saved on the first
  // Up and restored when Down returns past the newest entry. Consecutive duplicates are
  // skipped. Recall happens only from the first/last line, so Up/Down still move the cursor
  // between the rows of a multi-line entry.
  val history: scm.ArrayBuffer[Text] = scm.ArrayBuffer()
  var histIdx: Int = 0
  var draft: Text = t""

  while running do
    val root = InlineRoot(terminal)
    var editor: LineEditor = LineEditor(mode = LineEditor.Mode.Multiline(readyToSubmit))
    var candidates: List[Repl.CompletionItem] = Nil
    var tokens: List[Repl.Token] = Nil
    var lastValue: Text = t""
    var lastVersion: Int = -1

    // Record the live-highlight edit and fire an async server `tokenize` to refine it.
    def refresh(): Unit =
      val (version, refreshed) = state.record(lastValue, editor.value)
      tokens = refreshed
      lastValue = editor.value

      if editor.value.length > 0 && version != lastVersion then
        lastVersion = version
        val id = nextId.getAndIncrement
        pending(id) = version
        incompletePending(id) = editor.value
        duplex.send(zephyrine.Stream(framed(encode(Repl.Request.Tokenize(id, editor.value)))))

        // Ask for the inline suggestion too, but only with the cursor at the end of the
        // line (where the ghost is shown).
        if editor.position == editor.value.length then
          val gid = nextId.getAndIncrement
          ghostPending(gid) = editor.value
          duplex.send(zephyrine.Stream(framed(encode(Repl.Request.Complete(gid, editor.value, editor.position)))))

    // The inline suggestion for the current line: the remainder of a unique completion, or
    // the common stem of several (then "..."), shown only with the cursor at the line's end
    // and only while the latest reply still matches the value on screen.
    def ghost: Text =
      if editor.value.length == 0 || editor.position != editor.value.length then t"" else
        ghostReply.lay(t""): reply =>
          val (forValue, items) = reply
          if forValue != editor.value then t"" else
            val stem: Text =
              if editor.value.starts(t"/") && !completesAsScala(editor.value) then editor.value
              else editor.value.skip(identifierStart(editor))

            items match
              case Nil => t""
              case single :: Nil =>
                if single.name.length > stem.length && single.name.starts(stem)
                then single.name.skip(stem.length) else t""

              case many =>
                val common = longestCommonPrefix(many.map(_.name))
                if common.length > stem.length && common.starts(stem)
                then t"${common.skip(stem.length)}..." else t""

    def frame(): Unit =
      // The editor's text width: the terminal less the border (2) and one column of padding
      // inside each border edge (2) — matching `replPane`, so wrapping and height agree.
      val innerWidth = (terminal.knownColumns - 4).max(1)
      val rows = LineEditor.cursorPosition(editor.value, editor.value.length, innerWidth)._1 + 1

      val compLines: List[Teletype] =
        if candidates.isEmpty then Nil
        else completionTable(candidates.take(candidates.length.min(10)), terminal.knownColumns.max(1))

      // `paint` already flushes the block to the terminal (via `InlineRoot.flush`), so no explicit
      // `root.flush()` is needed here — and a second flush would be actively wrong, re-running the
      // inline shrink handling against an already-settled block.
      paint(root, replPane(editor, tokens, ghost, rows, compLines))

    refresh()
    frame()
    var editing = true
    var submitted: Optional[Text] = Unset

    // The block is already drawn; now wait for events. `hasNext` blocks until a
    // keypress (or returns false when the input stream closes, ending the session).
    while editing do
      if !events.hasNext then
        editing = false
        running = false
      else
        events.next() match
          case Keypress.Ctrl('C' | 'D') => running = false; editing = false
          case Keypress.Escape          => running = false; editing = false

          case _: TerminalInfo.WindowSize =>
            // A resize reflows all prior on-screen content unpredictably, so there is no sound
            // way to reconcile the existing inline layout. Instead WIPE the screen and reset the
            // block, then — once the resize settles (hysteresis) — redraw the editor fresh from the
            // top. Clearing on every event keeps the screen blank during a drag, not garbled; the
            // reset means any frame in the meantime (a keystroke) also draws cleanly from the top.
            Out.print(t"\e[2J\e[H")
            root.reset()
            resizeGen += 1
            val gen = resizeGen

            async:
              jl.Thread.sleep(200)
              if resizeGen == gen then
                resizeReplay = true
                terminal.events.put(TerminalInfo.Redraw)

          // A tokenize reply arrived (posted by the reader): re-derive the highlight
          // from the reconciled checkpoint and repaint, with no edit and no new request.
          case TerminalInfo.Redraw =>
            // The settle after a resize: lay the whole session out afresh from the top, then draw
            // the (reset) editor immediately below it. Otherwise it is a tokenize repaint.
            if resizeReplay || asyncReplay then
              resizeReplay = false
              asyncReplay = false
              replay()
              root.reset()
              frame()
            else
              tokens = state.record(editor.value, editor.value)._2
              frame()

          // `/session <name>` completes against the server's live session list (queried fresh here).
          case Keypress.Tab if editor.value.starts(t"/session ") =>
            duplex.send(zephyrine.Stream(framed(encode(Repl.Request.Session(nextId.getAndIncrement, t"")))))
            val names:   List[Text] = safely(sessionReplies.take().nn).lay(Nil)(_.names)
            val partial: Text       = editor.value.skip(t"/session ".length)
            val matches: List[Text] = names.filter(_.starts(partial)).map { name => t"/session $name" }

            matches match
              case Nil => ()

              case one :: Nil =>
                editor = LineEditor(one, one.length, editor.mode)
                candidates = Nil

              case several =>
                val prefix = longestCommonPrefix(several)
                if prefix.length > editor.value.length
                then editor = LineEditor(prefix, prefix.length, editor.mode)
                candidates = several.map { name => Repl.CompletionItem(name, t"command", t"") }

            refresh()
            frame()

          // `/classload <path>` completes its argument against the server's filesystem (the engine lists
          // the working directory). The candidates are whole `/classload <path>` lines — a path is not
          // an identifier — so a pick replaces the whole line; a directory candidate ends in `/`, so a
          // further Tab drills into it. Mirrors the `/session` handler, but the list comes from the
          // server via an ordinary `Complete` request (id 0, drained on `completions`).
          case Keypress.Tab if editor.value.starts(t"/classload ") =>
            duplex.send(zephyrine.Stream(framed(encode(Repl.Request.Complete(0, editor.value, editor.position)))))
            val cands: List[Repl.CompletionItem] = completions.take().nn

            cands match
              case Nil => ()

              case one :: Nil =>
                editor = LineEditor(one.name, one.name.length, editor.mode)
                candidates = Nil

              case several =>
                val prefix = longestCommonPrefix(several.map(_.name))
                if prefix.length > editor.value.length
                then editor = LineEditor(prefix, prefix.length, editor.mode)
                candidates = several

            refresh()
            frame()

          case Keypress.Tab =>
            val (advanced, shown) = completeAt(editor, duplex, completions)
            editor = advanced
            candidates = shown
            refresh()
            frame()

          // Right at the end of the line accepts the ghost suggestion (its stem, dropping a
          // trailing "..."), fish-style; elsewhere it falls through to move the cursor.
          case Keypress.Right if editor.position == editor.value.length && ghost != t"" =>
            val suggestion: Text = ghost
            val accepted: Text = if suggestion.ends(t"...") then suggestion.keep(suggestion.length - 3) else suggestion
            val value: Text = t"${editor.value}$accepted"
            editor = LineEditor(value, value.length, editor.mode)
            candidates = Nil
            refresh()
            frame()

          // Up from the first line recalls the previous submission (saving the draft first);
          // from a lower line it falls through to move the cursor up within a multi-line entry.
          case Keypress.Up if !editor.value.keep(editor.position).contains(t"\n") && history.nonEmpty =>
            if histIdx == history.length then draft = editor.value
            if histIdx > 0 then
              histIdx -= 1
              val entry = history(histIdx)
              editor = LineEditor(entry, entry.length, editor.mode)
              candidates = Nil
              refresh()
              frame()

          // Down from the last line recalls the next submission (or the saved draft once past
          // the newest); from a higher line it falls through to move the cursor down.
          case Keypress.Down if !editor.value.skip(editor.position).contains(t"\n") && histIdx < history.length =>
            histIdx += 1
            val entry = if histIdx == history.length then draft else history(histIdx)
            editor = LineEditor(entry, entry.length, editor.mode)
            candidates = Nil
            refresh()
            frame()

          case event if editor.submitsOn(event) =>
            // A multi-line entry submitted by leaving a blank line at the end drops that blank line,
            // so the frozen box, the transcript and what is sent to the server all render tight. TRIM
            // the existing (correctly highlighted) `tokens` to the stripped length rather than
            // re-highlighting via `refresh()` — stripping a whole line is not a single-char edit, so the
            // live-highlight heuristic would fall back to one un-coloured token and the frozen box would
            // lose its syntax colours. Then repaint so `finish` freezes the tight, still-coloured box.
            val tight: Text = stripTrailingBlank(editor.value)
            if tight != editor.value then
              editor = LineEditor(tight, tight.length, editor.mode)
              candidates = Nil
              tokens = trimTokens(tokens, tight.length)
              lastValue = tight
              frame()

            submitted = tight
            editing = false

          // Auto-indent a CONTINUED line (a submitting Enter is caught above): when the cursor is at
          // the end of the current line — nothing to its right, so the newline isn't splitting text —
          // start the new line with the same leading whitespace as the line being left. An Enter in the
          // middle of text falls through to the plain split below.
          case Keypress.Enter
              if { val rest = editor.value.skip(editor.position); rest == t"" || rest.starts(t"\n") } =>
            val before: Text = editor.value.keep(editor.position)
            val indent: Text = leadingWhitespace(before.cut(t"\n").last)
            val value:  Text = t"$before\n$indent${editor.value.skip(editor.position)}"
            editor = LineEditor(value, editor.position + 1 + indent.length, editor.mode)
            candidates = Nil
            refresh()
            frame()

          case keypress: Keypress =>
            editor = editor(keypress)
            candidates = Nil
            refresh()
            frame()

          case _ =>
            ()

    // Drop any open completion pane before finishing, so the frozen block is just the editor box
    // (the inline shrink holds the box's top and clears the pane below it); otherwise `finish`
    // would leave the listing frozen in the scrollback between the line and its result.
    if candidates.nonEmpty then
      candidates = Nil
      frame()

    // Freeze the block onto the screen (the cursor drops onto the row right after it) and print
    // the result there; the next line's inline block then flows immediately below.
    root.finish()

    submitted.let: line =>
      // Record the submission for Up/Down recall (skip a consecutive duplicate), then reset
      // the cursor back to the newest position, exactly as the web front-end does on submit.
      if line.trim != t"" && history.lastOption != Some(line) then history += line
      histIdx = history.length
      draft = t""

      // Server-side commands go through `Submit` like ordinary input; the engine recognises them
      // (`Repl.isCommand`) and replies with a confirmation message. `/disconnect`, `/quit`, `/clear`
      // and `/session` are the client-/connection-only commands, handled below before that check.

      // Set to the submission's id when the server answered with an async `Pending` placeholder, so the
      // transcript entry below is filed for the reader to fill when the real reply arrives.
      var asyncId: Optional[Int] = Unset

      // The text shown below the box — the result, error, or command message. Captured (rather
      // than printed straight to `Out`) so the identical rendering is reused when the transcript
      // is replayed after a resize. It ends with a newline when non-empty, so the next inline
      // block starts on a fresh line; empty means the line produced nothing (a definition/Unit).
      val result: Text =
        if line == t"/disconnect" then
          running = false
          t""
        else if line == t"/quit" then
          duplex.send(zephyrine.Stream(framed(encode(Repl.Request.Quit(0)))))
          running = false
          t""
        else if line == t"/clear" then
          // Drop the whole session and wipe the screen AND scrollback (`\e[3J`), leaving a fresh
          // editor at the top. The just-submitted `/clear` box (already frozen by `finish`) is
          // cleared along with it, and the emptied transcript means a later resize starts clean.
          transcript.clear()
          Out.print(t"\e[2J\e[3J\e[H")
          t""
        else if line == t"/session" || line.starts(t"/session ") then
          // Switch this connection to another session (server-side, per connection) — or, with no
          // argument, report the current session and the ones available.
          val name = line.skip(t"/session".length).trim
          duplex.send(zephyrine.Stream(framed(encode(Repl.Request.Session(nextId.getAndIncrement, name)))))
          safely(sessionReplies.take().nn).lay(t"flame: no response from the server\n"): reply =>
            if name == t"" then t"${Repl.messages.sessionList(reply.name, reply.names)}\n"
            else if reply.name == name then t"${Repl.messages.switched(reply.name)}\n"
            else t"${Repl.messages.noSession(name)}\n"
        else if line.starts(t"/") && !Repl.isCommand(line) then
          t"${Repl.messages.unknownCommand(line)}\n"
        else
          // A fresh id per submission so an async placeholder and its later out-of-band fill correlate
          // (submits are serialized — we block on the first reply before the next line — so first-reply
          // FIFO holds; only the async fills, routed by id, are concurrent).
          val sid = nextId.getAndIncrement
          duplex.send(zephyrine.Stream(framed(encode(Repl.Request.Submit(sid, line)))))
          submits.take().nn match
            case Repl.Reply.Pending(_) =>
              // Async mode: the result isn't ready. Record the id so the transcript entry is filed for
              // the reader to stream stdout into and finally fill; show its live panel (any output
              // already streamed, then a faint "evaluating" line) meanwhile.
              asyncId = sid
              evaluating(sid)

            case reply =>
              replyText(reply)

      if result != t"" then Out.print(result)

      // Keep the line so a resize can re-render the whole session from the top: the box redraws
      // from `tokens` (reflowing to the new width) and the result is reprinted verbatim. `/clear`
      // is the exception — it just wiped the session, so it must not re-seed the transcript. An async
      // submission files its entry under `asyncId` so the out-of-band fill can update it in place (and
      // applies a fill that raced ahead of this point).
      if line != t"/clear" then
        val entry = TranscriptEntry(line, tokens, result, asyncId.or(0))
        transcript += entry

        asyncId.let: sid =>
          asyncResults.remove(sid) match
            case Some(text) =>
              // The final reply already arrived — apply it; no need to register for streaming.
              entry.result = text
              asyncReplay = true
              terminal.events.put(TerminalInfo.Redraw)

            case None =>
              // Register so the reader can stream stdout into this entry and finally fill it; apply any
              // output that streamed in before this point.
              asyncEntries(sid) = entry
              if asyncOutput.contains(sid) then
                entry.result = evaluating(sid)
                asyncReplay = true
                terminal.events.put(TerminalInfo.Redraw)

  // The loop has exited (Ctrl+D/C, Escape, /quit, or a closed stream): stop the reader.
  live = false

// The inline block: a bordered editor box showing the live-highlighted line with its
// caret, and — when completions are active — a pane below it listing them, which
// auto-sizes to the candidate count (capped) and vanishes when there are none.
private def replPane
  ( editor:    LineEditor,
    tokens:    List[Repl.Token],
    ghost:     Text,
    rows:      Int,
    compLines: List[Teletype] )
:   Pane =

  // A one-column empty spacer, so the editor sits one character in from each border edge.
  def edge: Pane = panel(minWidth = 1, maxWidth = 1)(())

  val box = cadetBorder:
    file
     ( edge,
       panel(minHeight = rows, maxHeight = rows):
         val extent = summon[Extent]
         val cols = extent.width.max(1)
         // The ghost is shown faint after the line; it is not part of `editor.value`, so the
         // caret still sits at the cursor, with the suggestion trailing to its right.
         extent.put(if ghost == t"" then colourful(tokens) else colourful(tokens)+e"$Faint($ghost)")
         val (curRow, curCol) = LineEditor.cursorPosition(editor.value, editor.position, cols)
         extent.showCaret(curCol.z, curRow.z),
       edge )

  // The completion pane below the box, sized exactly to the candidate lines (joined without a
  // trailing newline, which would scroll the panel and clip the first row). When the pane
  // shrinks or vanishes, the inline anchoring holds the box's top and clears the rows below it,
  // so the editor never moves; when there are no candidates the block is just the box.
  if compLines.isEmpty then box
  else
    val list = panel(minHeight = compLines.length, maxHeight = compLines.length):
      summon[Extent].put(compLines.join(e"\n"))

    rank(box, list)

// One replayable line of the session: the source `text` (to compute the box's wrapped height at any
// terminal width), the highlight `tokens` (to redraw the box), and the `result` text printed below it
// (see `runRepl`'s `transcript`/`replay`). `result` is a `var` and `id` identifies the submission so an
// async fill (arriving out-of-band after a `Pending` placeholder) can update the result in place; `id`
// is 0 for synchronous entries, which never need locating.
private case class TranscriptEntry
   (text: Text, tokens: List[Repl.Token], var result: Text, id: Int = 0)

// A static (non-interactive) editor box for transcript replay: the same rounded border and
// 1-column inner padding as `replPane`'s box, showing the highlighted line, but with no caret,
// ghost, or completion pane. `rows` is the line's wrapped height at the current width.
private def replayBox(tokens: List[Repl.Token], rows: Int): Pane =
  def edge: Pane = panel(minWidth = 1, maxWidth = 1)(())

  cadetBorder:
    file
     ( edge,
       panel(minHeight = rows, maxHeight = rows)(summon[Extent].put(colourful(tokens))),
       edge )

// The editor box's border, drawn in CadetBlue. Mirrors ultimatum's `border` (rounded style),
// but colours each glyph — the library's `border` renders its rules and corners uncoloured.
private def cadetBorder(child: Pane): Pane =
  val style  = BorderStyle.rounded
  val colour = WebColors.CadetBlue

  def horizontalRule: Pane = panel(minHeight = 1, maxHeight = 1):
    val extent = summon[Extent]
    extent.move(Prim, Prim)
    extent.put(e"$colour(${style.horizontal*extent.width})")

  def verticalRule: Pane = panel(minWidth = 1, maxWidth = 1):
    val extent = summon[Extent]
    var row = 0
    while row < extent.height do
      extent.move(Prim, row.z)
      extent.put(e"$colour(${style.vertical})")
      row += 1

  def corner(glyph: Text): Pane =
    panel(minWidth = 1, maxWidth = 1, minHeight = 1, maxHeight = 1):
      summon[Extent].put(e"$colour($glyph)")

  def band(left: Text, right: Text): Pane = file(corner(left), horizontalRule, corner(right))

  rank
   ( band(style.topLeft, style.topRight),
     file(verticalRule, child, verticalRule),
     band(style.bottomLeft, style.bottomRight) )

// Renders a server reply to the text shown below the submitted line's box — the SAME text
// whether printed live or reprinted when the transcript is replayed after a resize. Each
// non-empty part ends with a newline (as the live output did), so lines flow correctly; an
// empty result means the line produced no visible value (a definition, import, or Unit).
private def replyText(reply: Repl.Reply): Text = reply match
  case Repl.Reply.Ran(_, value, output, tpe, name, diagnostics, _) =>
    // The value is shown only when there is one, as `name = value : type`, each part omitted
    // when absent — so no bare `=`/`:` appears when there is no result.
    val valueLine: Text =
      val rendered: Text = value.or(t"")
      if rendered == t"" then t"" else
        val binding: Text = name.let { each => t"$each = " }.or(t"")
        val typed:   Text = tpe.let { each => t" : $each" }.or(t"")
        t"$binding$rendered$typed\n"

    val diag: Text = if diagnostics != t"" then t"$diagnostics\n" else t""
    t"$output$valueLine$diag"

  case Repl.Reply.Threw(_, output, diagnostics, _) => t"$output$diagnostics\n"
  case Repl.Reply.Rejected(_, diagnostics, _)      => t"$diagnostics\n"
  case Repl.Reply.Crashed(_, diagnostics, _)       => t"$diagnostics\n"
  case Repl.Reply.Failed(_, message)               => t"$message\n"
  case Repl.Reply.Tokenized(_, _, _)               => t""
  case Repl.Reply.Completed(_, _)                  => t""
  case Repl.Reply.Session(_, _, _)                 => t""
  case Repl.Reply.Pending(_)                       => t""
  case Repl.Reply.Output(_, _)                     => t""

// The request id every reply echoes — used to correlate an out-of-band async fill with the submission
// (its `Pending` placeholder) it completes. (The enum's cases share the field but not an accessor.)
private def replyId(reply: Repl.Reply): Int = reply match
  case Repl.Reply.Tokenized(id, _, _)         => id
  case Repl.Reply.Completed(id, _)            => id
  case Repl.Reply.Ran(id, _, _, _, _, _, _)   => id
  case Repl.Reply.Rejected(id, _, _)          => id
  case Repl.Reply.Threw(id, _, _, _)          => id
  case Repl.Reply.Crashed(id, _, _)           => id
  case Repl.Reply.Failed(id, _)               => id
  case Repl.Reply.Session(id, _, _)           => id
  case Repl.Reply.Pending(id)                 => id
  case Repl.Reply.Output(id, _)               => id

// The completion candidates as a compact, HEADER-LESS list — one Teletype line each: a kind glyph,
// the name, then the type signature (subdued, truncated to fit). The kind (replacing the old `kind`
// column) is a single Unicode glyph, and the glyph + name are coloured by the kind's broad category
// using the syntax palette — a term/callable peach, a type/namespace teal, a keyword orange — so a
// method reads like a term and a type like a type, matching how each would look in code.
private def completionTable(items: List[Repl.CompletionItem], width: Int): List[Teletype] =
  def style(kind: Text): (Text, Color in Srgb) = kind.lower match
    case t"method"    => (t"ƒ", palette.scalaTerm)
    case t"term"      => (t"•", palette.scalaTerm)
    case t"given"     => (t"⊢", palette.scalaTerm)
    case t"extension" => (t"⊕", palette.scalaTerm)
    case t"type"      => (t"◇", palette.scalaType)
    case t"module"    => (t"◆", palette.scalaType)
    case t"package"   => (t"▦", palette.scalaType)
    case t"keyword"   => (t"▸", palette.scalaKeyword)
    case t"command"   => (t"⌘", palette.scalaKeyword)
    case _            => (t"·", palette.foreground)

  // The name column is as wide as the widest name (capped so the signature keeps room); the glyph
  // takes 1 column + 1 space, and the signature the remainder — kept 1 column short of the terminal
  // so an ambiguous-width glyph can't push the line into a wrap.
  val nameCol: Int = items.map(_.name.length).maxOption.getOrElse(0).min((width - 12).max(1))
  val sigCol:  Int = (width - nameCol - 4).max(1)

  items.map: item =>
    val (glyph, colour) = style(item.kind)
    val name:   Text = item.name
    val padded: Text = if name.length >= nameCol then name.keep(nameCol) else name+t" "*(nameCol - name.length)
    val sig0:   Text = item.signature
    val sig:    Text = if sig0.length > sigCol then t"${sig0.keep(sigCol - 1)}…" else sig0

    e"${colour}($glyph) ${colour}($padded) ${palette.scalaComment}($sig)"

// A `/`-command line whose ARGUMENT the SERVER completes (as opposed to the client completing the
// command name locally against `slashCommands`): `/tasty`/`/bytecode` complete their argument as
// ordinary Scala; `/language` completes its argument against the session's available features (which
// depend on whether `experimental` is on, so only the server knows them). All three insert at the
// identifier boundary, keeping the command prefix — hence the shared treatment here and in the ghost.
private def completesAsScala(value: Text): Boolean =
  value.starts(t"/tasty ") || value.starts(t"/bytecode ") || value.starts(t"/language ")

// On Tab, complete: a `/`-command line completes locally against `slashCommands`;
// otherwise the server is asked and we block for the reply. A unique candidate is
// inserted; with several, the longest common prefix is inserted and the candidates are
// returned for display.
private def completeAt
  ( editor:      LineEditor,
    duplex:      Duplex,
    completions: juc.LinkedBlockingQueue[List[Repl.CompletionItem]] )
:   (LineEditor, List[Repl.CompletionItem]) =

  // A `/`-command line completes locally against `slashCommands` — EXCEPT the argument-completing
  // commands (`/tasty`, `/bytecode`), whose argument the server completes as ordinary Scala, so they
  // fall through to the server branch below.
  if editor.value.starts(t"/") && !completesAsScala(editor.value) then
    slashCommands.filter { (name, _) => name.starts(editor.value) } match
      case (name, _) :: Nil =>
        (LineEditor(name, name.length, editor.mode), Nil)

      case matches =>
        val items =
          matches.map: (name, help) =>
            Repl.CompletionItem(name, t"command", help)

        val prefix = longestCommonPrefix(matches.map(_._1))

        val advanced =
          if prefix.length > editor.value.length then LineEditor(prefix, prefix.length, editor.mode)
          else editor

        (advanced, items)
  else
    val request = encode(Repl.Request.Complete(0, editor.value, editor.position))
    duplex.send(zephyrine.Stream(framed(request)))

    val candidates: List[Repl.CompletionItem] = completions.take().nn

    candidates match
      case single :: Nil =>
        (insertCompletion(editor, single.name), Nil)

      case _ =>
        val prefix = longestCommonPrefix(candidates.map(_.name))

        val advanced =
          if prefix.length > partialLength(editor) then insertCompletion(editor, prefix) else editor

        (advanced, candidates)

// An `Srgb` colour from a 24-bit `0xRRGGBB` hex literal (the form Zed themes use), so the palette
// below can be transcribed straight from `~/.config/zed/themes/propensive.json`.
private def hex(rgb: Int): Color in Srgb =
  Srgb(((rgb >> 16) & 0xff)/255.0, ((rgb >> 8) & 0xff)/255.0, (rgb & 0xff)/255.0)

// The editor's syntax-highlighting colours, *specified* here as an iridescence
// `Palette` rather than hardcoded per accent. Harlequin's `syntaxHighlighting`
// renderer turns tokens into a `Teletype` using these colours, and the terminal's
// colour depth is applied when the `Teletype` is rendered (see `render`). Swap this
// given — or `import` a different `ScalaSyntaxPalette` — to retheme the REPL.
//
// Matched to the user's Zed theme "Propensive" (its `style.syntax` scopes): scalaKeyword→keyword,
// scalaType→type, scalaString→string, scalaNumber→number, scalaTerm→variable, scalaComment→comment;
// the two symbol accents map to Zed's punctuation.bracket and operator (see the note at those
// methods). A Harlequin accent is now a COLOUR CATEGORY only: every term (a `val`/`def` name AND a
// reference) shares `scalaTerm`, and the binding-vs-usage distinction is a `Role` styled separately
// (italic — see `colourful`). Zed has no distinct "modifier" scope (modifiers share the keyword
// colour) and no error/margin syntax scopes, so those reuse the nearest editor/UI colours; and
// Harlequin has no boolean accent, so `true`/`false` render as keywords, not Zed's pink.
private given palette: ScalaSyntaxPalette = new Palette:
  type Form = Srgb
  def background:       Color in Srgb = hex(0x000000)  // editor.background
  def foreground:       Color in Srgb = hex(0xd4be98)  // editor.foreground
  def scalaError:       Color in Srgb = hex(0xea6962)  // deleted (a clear red)
  def scalaNumber:      Color in Srgb = hex(0xcc3366)  // number
  def scalaString:      Color in Srgb = hex(0x99ffff)  // string
  def scalaTerm:        Color in Srgb = hex(0xffcc99)  // variable — every term (binding or usage)
  def scalaType:        Color in Srgb = hex(0x00cc99)  // type
  def scalaKeyword:     Color in Srgb = hex(0xff6633)  // keyword
  // Harlequin's `Symbol` accent covers brackets and `:` (Zed punctuation.bracket); its `Parens`
  // accent covers `=`/`.`-style operators (Zed operator) — the names read backwards, so the colours
  // are assigned by which tokens actually carry each accent, not by the method name.
  def scalaSymbol:      Color in Srgb = hex(0xcc6699)  // punctuation.bracket — `(` `)` `[` `]` `:`
  def scalaParenthesis: Color in Srgb = hex(0xf28534)  // operator — `=` `.`
  def scalaModifier:    Color in Srgb = hex(0xff6633)  // keyword (no distinct modifier scope)
  def scalaComment:     Color in Srgb = hex(0x928374)  // comment
  def subdued:          Color in Srgb = hex(0x5a524c)  // editor.line_number
  def accented:         Color in Srgb = hex(0xd4be98)  // editor.active_line_number
  def margin:           Color in Srgb = hex(0x111111)  // editor.gutter.background

// Maps a Harlequin accent name (as carried on the wire) back to its `Accent`.
private def accentOf(accent: Text): Accent = accent match
  case t"keyword"  => Accent.Keyword
  case t"modifier" => Accent.Modifier
  case t"typal"    => Accent.Typal
  case t"string"   => Accent.String
  case t"number"   => Accent.Number
  case t"symbol"   => Accent.Symbol
  case t"parens"   => Accent.Parens
  case t"error"    => Accent.Error
  case t"unparsed" => Accent.Unparsed
  case _           => Accent.Term

// Reconstructs the source line from the highlight tokens, colouring each through the palette via
// Harlequin's syntax-highlighting renderer — and italicising a NEWLY-INTRODUCED identifier. The
// accent gives only the colour (a term is `term`, a type is `typal`, whether bound or used); the
// token's `role` says whether it is a `binding` (a `val`/`def`/parameter/pattern name, a class/type
// definition, or a type parameter) or a `usage`. So italicising the `binding` role distinguishes
// `foo`/`T` where introduced from where applied — as the styling policy Harlequin#1439 leaves to the
// front-end. (A live-heuristic token has no role yet, so it is refined to italic by the server.)
private def colourful(tokens: List[Repl.Token]): Teletype =
  import harlequin.syntaxHighlighting.tokenTeletypeable

  val text: Text = tokens.map(_.text).join

  // A `/`-command line is a REPL command, not Scala, so it is highlighted specially rather than
  // through the Harlequin accents: the command word itself keeps the keyword colour, and its
  // parameters (everything after the first space) are shown in a fainter, more-yellow colour, to set
  // them apart from the command.
  if text.starts(t"/") then
    val split: Int = text.s.indexOf(' ') match
      case -1 => text.length
      case n  => n

    val command: Teletype = e"${palette.scalaKeyword}(${text.keep(split)})"
    val params:  Text     = text.skip(split)

    if params == t"" then command else e"$command$Faint(${commandParameter}($params))"
  else
    tokens.map: token =>
      val coloured: Teletype = harlequin.Token(token.text, accentOf(token.accent)).teletype
      if token.role.let(_ == t"binding").or(false) then e"$Italic($coloured)" else coloured

    . join

// The fainter, more-yellow colour used for the parameters of a `/`-command (see `colourful`),
// dimmed further by `Faint` so the command word reads as the emphasis and its arguments recede.
private val commandParameter: Color in Srgb = hex(0xd8a657)  // a muted yellow

// ── Live-highlight heuristic ────────────────────────────────────────────────
// A single-character edit and the "kind" of a character, for guessing accents
// before the server's tokenization arrives.
private enum Edit:
  case Insert(at: Int, char: Char)
  case Delete(at: Int)

private enum CharKind:
  case Word, Symbol, Space

private def charKind(c: Char): CharKind =
  if c.isLetterOrDigit || c == '_' then CharKind.Word
  else if c.isWhitespace then CharKind.Space
  else CharKind.Symbol

// The character kind a token's accent stands for, so an inserted character can be
// matched against the token it touches.
private def accentKind(accent: Text): CharKind =
  if accent == t"symbol" || accent == t"parens" then CharKind.Symbol
  else if accent == t"unparsed" then CharKind.Space
  else CharKind.Word

// The heuristic accent for a freshly-typed run before the server's tokenize refines it. A word is a
// `term` (all terms share one colour now). A heuristic token carries no `role`, so it is not italic;
// the server then annotates an actual definition with the `binding` role (→ italic). So a typed
// reference never flickers italic — only a confirmed binding gains it once the tokenize reply lands.
private def defaultAccent(kind: CharKind): Text = kind match
  case CharKind.Word   => t"term"
  case CharKind.Symbol => t"symbol"
  case CharKind.Space  => t"unparsed"

private def commonPrefix(a: Text, b: Text): Int =
  val n = a.length.min(b.length)
  var i = 0
  while i < n && a.s.charAt(i) == b.s.charAt(i) do i += 1
  i

// Classifies `oldBuf -> newBuf` as a single-character insert or delete, or `None`.
private def diff(oldBuf: Text, newBuf: Text): Option[Edit] =
  val p = commonPrefix(oldBuf, newBuf)

  if newBuf.length == oldBuf.length + 1 && newBuf.skip(p + 1) == oldBuf.skip(p)
  then Some(Edit.Insert(p, newBuf.s.charAt(p)))
  else if newBuf.length == oldBuf.length - 1 && oldBuf.skip(p + 1) == newBuf.skip(p)
  then Some(Edit.Delete(p))
  else None

// Inserts `c` at offset `p`, giving it an accent from the touching token(s): join
// the token it lands in/next to if their kinds match, otherwise split / start a
// new token (anything joins a `string`). Preserves the text, so widths are exact.
private def insertChar(tokens: List[Repl.Token], p: Int, c: Char): List[Repl.Token] =
  val kind     = charKind(c)
  val ch: Text = c.toString.tt
  def tok(text: Text, accent: Text): Repl.Token = Repl.Token(text, accent, Unset)
  val arr      = tokens.toVector
  val offsets  = arr.scanLeft(0)(_ + _.text.length)
  val total    = offsets(arr.length)

  if arr.isEmpty then List(tok(ch, defaultAccent(kind)))
  else arr.indices.find { i => p > offsets(i) && p < offsets(i + 1) } match
    case Some(i) =>
      val t  = arr(i)
      val at = p - offsets(i)

      val mid =
        if kind == accentKind(t.accent) || t.accent == t"string"
        then Series(tok(t.text.keep(at) + ch + t.text.skip(at), t.accent))
        else Series(tok(t.text.keep(at), t.accent), tok(ch, defaultAccent(kind)),
                    tok(t.text.skip(at), t.accent))

      (arr.take(i) ++ mid ++ arr.drop(i + 1)).to(List)

    case None =>
      if p <= 0 then
        val r = arr(0)

        if kind == accentKind(r.accent) then (tok(ch + r.text, r.accent) +: arr.drop(1)).to(List)
        else (tok(ch, defaultAccent(kind)) +: arr).to(List)
      else if p >= total then
        val l = arr(arr.length - 1)

        if kind == accentKind(l.accent)
        then (arr.dropRight(1) :+ tok(l.text + ch, l.accent)).to(List)
        else (arr :+ tok(ch, defaultAccent(kind))).to(List)
      else
        val i  = arr.indices.find { j => offsets(j) == p }.getOrElse(arr.length)
        val l  = arr(i - 1)
        val r  = arr(i)
        val lk = accentKind(l.accent)
        val rk = accentKind(r.accent)

        if kind == lk
        then (arr.take(i - 1) ++ Series(tok(l.text + ch, l.accent)) ++ arr.drop(i)).to(List)
        else if kind == rk
        then (arr.take(i) ++ Series(tok(ch + r.text, r.accent)) ++ arr.drop(i + 1)).to(List)
        else (arr.take(i) ++ Series(tok(ch, defaultAccent(kind))) ++ arr.drop(i)).to(List)

private def deleteChar(tokens: List[Repl.Token], p: Int): List[Repl.Token] =
  var offset = 0

  tokens.flatMap: t =>
    val s = offset
    offset += t.text.length

    if p >= s && p < offset then
      val text = t.text.keep(p - s) + t.text.skip(p - s + 1)
      if text.length == 0 then Nil else List(Repl.Token(text, t.accent, t.tpe))
    else
      List(t)

private def replay(base: List[Repl.Token], edits: List[Edit]): List[Repl.Token] =
  edits.foldLeft(base): (tokens, edit) =>
    edit match
      case Edit.Insert(at, c) => insertChar(tokens, at, c)
      case Edit.Delete(at)    => deleteChar(tokens, at)

// Tracks the live highlight: an authoritative server checkpoint at some buffer
// version, plus the log of edits made since, replayed through the heuristic so
// the buffer stays coloured without waiting for the server. Thread-safe — the
// editor records edits, the background reader adopts checkpoints.
private class LiveState:
  private var version: Int = 0
  private var checkpointVersion: Int = 0
  private var checkpointTokens: List[Repl.Token] = Nil
  private var log: List[(Int, Edit)] = Nil

  // Records the keystroke `oldBuf -> newBuf`, returning this buffer's version and
  // the heuristic tokens to draw now.
  def record(oldBuf: Text, newBuf: Text): (Int, List[Repl.Token]) = synchronized:
    if newBuf.length == 0 then
      checkpointTokens = Nil
      log = Nil
      checkpointVersion = version
      (version, Nil)
    else
      if newBuf != oldBuf then diff(oldBuf, newBuf) match
        case Some(edit) =>
          version += 1
          log = log :+ (version, edit)

        case None =>   // not a single-char edit (paste, kill-line): plain fallback
          checkpointTokens = List(Repl.Token(newBuf, t"term", Unset))
          checkpointVersion = version
          version += 1
          log = Nil

      (version, replay(checkpointTokens, log.map(_._2)))

  // A `tokenize` reply for buffer version `v` arrived: if newer than our
  // checkpoint, adopt it and drop the edits it already accounts for.
  def reconcile(v: Int, tokens: List[Repl.Token]): Unit = synchronized:
    if v > checkpointVersion then
      checkpointVersion = v
      checkpointTokens = tokens
      log = log.filter(_._1 > v)

// The `/`-commands offered as Tab completions when the line starts with `/`: the engine's
// own commands (shared with the web front-end via `Repl.slashCommands`) plus the client-only
// ones. Keep the client-only entries in step with the dispatch in `converse`.
private val slashCommands: List[(Text, Text)] =
  Repl.slashCommands ++ List
    ( t"/clear"      -> t"clear the screen and forget the session history",
      t"/session"    -> t"switch to another session (or list them)",
      t"/disconnect" -> t"leave the session, keeping the server running",
      t"/quit"       -> t"stop the server and quit" )

// Replaces the partial identifier ending at the cursor with the completion `name`,
// returning the editor with the cursor just after the inserted text.
private def insertCompletion(editor: LineEditor, name: Text): LineEditor =
  val start:  Int  = identifierStart(editor)
  val prefix: Text = editor.value.keep(start)
  val suffix: Text = editor.value.skip(editor.position)

  LineEditor(t"$prefix$name$suffix", start + name.length, editor.mode)

private def identifierStart(editor: LineEditor): Int =
  val before: String = editor.value.keep(editor.position).s
  var start:  Int    = before.length

  while start > 0 && isIdentifierChar(before.charAt(start - 1)) do start -= 1

  start

private def partialLength(editor: LineEditor): Int = editor.position - identifierStart(editor)

private def isIdentifierChar(char: Char): Boolean = char.isLetterOrDigit || char == '_'

private def longestCommonPrefix(names: List[Text]): Text = names match
  case Nil => t""

  case head :: tail =>
    tail.foldLeft(head): (prefix, name) =>
      prefix.keep(commonPrefix(prefix, name))

// The leading run of spaces/tabs of `line` — the indentation copied onto an auto-indented new line.
private def leadingWhitespace(line: Text): Text =
  line.s.takeWhile { char => char == ' ' || char == '\t' }.nn.tt

// Whether `text`'s last line — the text after its final newline — is blank. This is the "leave a
// blank line at the end" trigger that submits a multi-line entry.
private def blankLastLine(text: Text): Boolean =
  text.contains(t"\n") && text.cut(t"\n").last.trim == t""

// Drops a single trailing blank line (the final newline and any whitespace-only last line), so a
// multi-line entry submitted by leaving a blank line renders tight everywhere it is shown or stored.
// Text with no trailing blank line is returned unchanged.
private def stripTrailingBlank(text: Text): Text =
  if blankLastLine(text) then text.keep(text.s.lastIndexOf('\n')) else text

// Keeps the first `length` characters' worth of `tokens`, preserving each kept token's accent/role so
// the highlighting survives — used to drop the stripped trailing blank line from a multi-line
// submission's highlight without re-tokenizing (which would lose the colours). The token straddling the
// cut is truncated to its kept prefix; tokens past the cut are dropped.
private def trimTokens(tokens: List[Repl.Token], length: Int): List[Repl.Token] =
  var remaining = length
  tokens.flatMap: token =>
    if remaining <= 0 then Nil
    else if token.text.length <= remaining then
      remaining -= token.text.length
      List(token)
    else
      val kept = token.text.keep(remaining)
      remaining = 0
      List(token.copy(text = kept))

// Non-empty with every bracket closed; open brackets continue the input onto a new line.
private def balanced(text: Text): Boolean =
  val depth: Int = text.s.foldLeft(0):
    case (depth, '(' | '[' | '{') => depth + 1
    case (depth, ')' | ']' | '}') => depth - 1
    case (depth, _)               => depth

  text.s.length > 0 && depth <= 0

// A local, cheap heuristic for whether a SINGLE line reads as a complete statement/expression —
// used to decide Enter's behaviour before (or instead of) the server's authoritative parser verdict
// arrives (notably on a cold server, whose first verdict is slow). A line is treated as INCOMPLETE
// (so Enter continues onto a new line) when its brackets are unbalanced, it ends with an operator /
// assignment / selector / separator that needs a right-hand side (`def f =`, `x +`, `a.`, `xs,`), or
// it ends with a keyword that must be followed by more (`if … then`, `for`, `new`, `def`, …).
private val continuationKeywords: Set[Text] =
  Set(t"then", t"else", t"do", t"yield", t"match", t"catch", t"finally", t"extends", t"with",
      t"derives", t"if", t"while", t"for", t"new", t"def", t"val", t"var", t"lazy", t"given",
      t"import", t"export", t"return", t"throw", t"case", t"try")

private def endsWithContinuation(text: Text): Boolean =
  val trimmed: String = text.s.trim.nn
  if trimmed.isEmpty then true else
    val last:     Char    = trimmed.charAt(trimmed.length - 1)
    val operator: Boolean = "+-*/%<>=&|^.:,@".indexOf(last.toInt) >= 0
    val lastWord: Text    = trimmed.drop(trimmed.lastIndexWhere(_.isWhitespace) + 1).nn.tt
    operator || continuationKeywords.contains(lastWord)

private def singleLineComplete(text: Text): Boolean =
  balanced(text) && !endsWithContinuation(text)


// Serializes a `Request` to BinTEL body bytes, deriving the schema from the type
// (`value.bintel`). A valid request always type-assigns, so the encode is total.
private def encode(request: Repl.Request): Data = unsafely(request.bintel)

// Prefixes BinTEL body bytes with a 4-byte big-endian length, the on-wire frame the
// server reads with `DataInputStream.readInt` + `readFully`.
private def framed(data: Data): Data =
  val length: Int = data.length

  val header: Data =
    IArray[Byte]((length >>> 24).toByte, (length >>> 16).toByte, (length >>> 8).toByte, length.toByte)

  header ++ data

// Reassembles length-prefixed frames from the (chunk-at-a-time) socket stream, keeping
// a buffer across calls so a frame split over chunks — or several frames in one chunk —
// is handled. `next()` yields one frame's body, or `Unset` when the stream ends.
private class FrameReader(chunks: Iterator[Data]):
  private val buffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()

  private def fill(count: Int): Boolean =
    while buffer.length < count && chunks.hasNext do chunks.next().each(buffer += _)
    buffer.length >= count

  def next(): Optional[Data] =
    if !fill(4) then Unset else
      val length: Int =
        ((buffer(0) & 0xff) << 24) | ((buffer(1) & 0xff) << 16)
        | ((buffer(2) & 0xff) << 8) | (buffer(3) & 0xff)

      if !fill(4 + length) then Unset else
        val body: Data = IArray.from(buffer.slice(4, 4 + length))
        buffer.remove(0, 4 + length)
        body
