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
import java.lang as jl
import java.net as jn
import java.nio.channels as jnc
import java.util.concurrent as juc

import scala.caps
import scala.collection.immutable as sci

import scala.collection.concurrent.TrieMap
import scala.collection.mutable as scm

import soundness.*
import dysasymptotics.linearSize

// The stdlib-shaped operations on the prelude's opaque collections (Soundness #1693).

import escapade.Faint
import escapade.Italic
import escapade.Underline
import termcapDefinitions.xtermTrueColorTermcap
import textMetrics.wideCharacterWidthMetric
import iridescence.WebColors
// `soundness.*` also re-exports an unrelated `Signal` (embarcadero's workload-grant signal), so the
// POSIX terminal signal type (whose `.Int` is SIGINT) is named explicitly to resolve the clash. It
// is `profanity.Interrupt` as of Soundness 0.64 (`Signal` was its former name).
import profanity.Interrupt

import backstops.silentBackstop
import classloaders.threadContextClassloader
import executives.completionsExecutive
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteOnlyEmpty
import filesystemOptions.failOnPreexisting
import harlequin.Accent
// `border` is the one ultimatum layout combinator the `soundness` umbrella does not re-export —
// `panel`, `stack`, `strip`, `layout` and `paint` all are — so it is named directly from its own
// package rather than reached through the umbrella.
import pyrocosm.TerminalFrontend
import pathInterfaces.pathOnLinux
import internetAccess.online
import interpreters.posixInterpreter
import logging.silentLogging
import probates.cancelProbate
import socketBackends.javaBaseSockets
import supervisors.globalSupervisor
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading

val Serve = Subcommand("serve", "serve the Flame web front-end")
val Install = Subcommand("install", "install tab-completions into the shell")
val Listen = Subcommand("listen", "serve the terminal REPL over TCP, for remote clients")
val Port = Flag[Int]("port", false, List('p'), "a TCP port — the web front-end, or a remote REPL server")
val Host = Flag[Text]("host", false, List('H'), "connect to a flame REPL server running on this host")
val Join = Flag[Text]("join", false, List('j'), "join an existing REPL session by name")
val Create = Flag[Text]("create", false, List('c'), "create a new REPL session with the given name")
val Basic = Flag[Unit]("basic", false, Nil, "run a minimal, in-process, synchronous line-based REPL")

// One flag per `/set`/`/language` setting, so every setting is enabled by name —
// `flame --experimental --captureChecking`. Derived from the single list in `Repl.settings`, so the
// flags, the REPL's own commands and their completions cannot drift apart: a setting added there is
// a `--flag` here for free, and this is the ONLY way to set one from the command line.
val settingFlags: List[(Repl.Setting, Flag of Unit)] =
  Repl.settings.map: setting =>
    (setting, Flag[Unit](setting.name, false, Nil, setting.description))

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

// The settings named by their own flags (`--experimental`, `--feature`, …).
//
// EVERY flag is read, not just until one matches: reading a flag is what registers it with
// Exoskeleton (`Flag#apply` calls `cli.register`), and registration is the only way it reaches the
// completion output. A read placed behind a condition — or inside `execute`, whose block does not
// run at all in completion mode — silently costs that flag its tab-completion.
private def flaggedSettings(using Cli, Interpreter): List[Repl.Setting] =
  settingFlags.filter { (_, flag) => flag().present }.map { (setting, _) => setting }

// The commands to run on startup: the `/set`/`/language` settings named by flags and by the
// workspace's `config.tel` (see `Workspace`), then a `/classload` per configured classpath entry.
// Compiler settings are applied FIRST, so `--experimental` unlocks the experimental `--language`
// features named alongside it — `/language captureChecking` is rejected until `/set experimental`
// has run — and the classpath last, once the compiler is configured. A setting named by both a flag
// and the file is applied once.
private def startupCommands(flagged: List[Repl.Setting], workspace: Workspace.Config): List[Text] =
  def named(kind: Repl.Kind): List[Text] = flagged.filter(_.kind == kind).map(_.name)
  def distinct(names: List[Text]): List[Text] = List.from(names.stdlib.distinct)

  distinct(named(Repl.Kind.Set) + workspace.sets).map { (name: Text) => t"/set $name" }
  + distinct(named(Repl.Kind.Language) + workspace.languages).map { (name: Text) => t"/language $name" }
  + workspace.classpath.map { (entry: Text) => t"/classload $entry" }

// How a launch chooses its session: create a fresh one under a made-up name (the default, when
// neither flag is given), JOIN a named existing one (`--join`), or CREATE a named new one
// (`--create`). `Invalid` carries the message for a misuse — both flags at once, or a flag with no
// name — caught before any connection is attempted.
enum SessionIntent:
  case Default
  case Join(name: Text)
  case Create(name: Text)
  case Invalid(message: Text)

  // The requested name, for a failure message; empty for `Default`/`Invalid`.
  def requestedName: Text = this match
    case SessionIntent.Join(name)   => name
    case SessionIntent.Create(name) => name
    case _                          => t""

// Reads `--join`, its operand completing to the names of the sessions on the live local servers —
// so `flame --join <TAB>` (or `--join=<TAB>`) offers the sessions that can be joined. The
// suggestions are gathered LAZILY, only when a completion lands on the operand; the `Discoverable`
// closes over nothing tracked and probes the sockets itself. Returns the flag handle so the caller
// can see both whether the flag appeared and its operand.
private def joinFlag()(using Cli, Interpreter): Prospective[Text] =
  given discoverable: (Text is Discoverable) = (_, _) => liveSessionNames().map(Suggestion(_))
  Join()

// Whether a flag TOKEN appears in the raw arguments, in any spelling — `--long`, `--long=value`,
// `-s`, or a short flag with an attached value (`-sVALUE`). Used to detect a flag given WITHOUT a
// value, which the parsed operand alone cannot distinguish from an absent flag.
private def flagGiven(arguments: List[Argument], long: Text, short: Char): Boolean =
  arguments.exists: (argument: Argument) =>
    val text: Text = argument()
    text == long || text.starts(t"$long=") || text == t"-$short"
    || (text.starts(t"-$short") && !text.starts(t"--"))

// Resolves the session intent from the `--join`/`--create` flags and the workspace config. The
// flags win; only when NEITHER flag appears is the config consulted (its own `join`/`create`
// keys). A flag present without a name, or both a join and a create at once, is `Invalid`.
private def sessionIntent
   ( arguments: List[Argument],
     join:      Prospective[Text],
     create:    Prospective[Text],
     workspace: Workspace.Config )
:   SessionIntent =
  // A value flag reports `present` only once its operand PARSES (`Prospective.present` is
  // `value.present`), so a bare `--join` with no name would look absent and silently fall through
  // to the default. Presence is therefore read from the raw arguments — the flag TOKEN in any of
  // its forms — and the name from the parsed operand, so a flag with no name is a caught error.
  val joinGiven:   Boolean = flagGiven(arguments, t"--join", 'j')
  val createGiven: Boolean = flagGiven(arguments, t"--create", 'c')

  if joinGiven && createGiven then
    SessionIntent.Invalid(t"Specify only one of --join and --create")
  else if joinGiven then
    join.value.lay(SessionIntent.Invalid(t"--join requires a session name"))(SessionIntent.Join(_))
  else if createGiven then
    create.value.lay(SessionIntent.Invalid(t"--create requires a session name"))(SessionIntent.Create(_))
  else (workspace.join, workspace.create) match
    case (join: Text, create: Text) => SessionIntent.Invalid(t"config.tel names both a join and a create session")
    case (join: Text, _)            => SessionIntent.Join(join)
    case (_, create: Text)          => SessionIntent.Create(create)
    case _                          => SessionIntent.Default

// The names of every session on every live local REPL server: each per-process socket in the
// socket directory is asked (with an empty `Session` request, which reports without switching —
// and, since a connection's own session is created lazily, leaves no throwaway session behind) and
// a socket that refuses is skipped. A short timeout keeps a wedged server from stalling the shell.
private def liveSessionNames(): List[Text] =
  def ask(path: Text): List[Text] =
    var channel: jnc.SocketChannel | Null = null

    try
      channel = jnc.SocketChannel.open(jn.UnixDomainSocketAddress.of(path.s).nn).nn

      val out = ji.DataOutputStream(jnc.Channels.newOutputStream(channel).nn)
      val body = SocketEngine.encode(Repl.Request.SessionList(0))
      out.writeInt(body.length)
      out.write(body.mutable(using Unsafe))
      out.flush()

      val in     = ji.DataInputStream(jnc.Channels.newInputStream(channel).nn)
      val length = in.readInt()
      val bytes  = new scala.Array[Byte](length)
      in.readFully(bytes)

      safely(Bintel.read[Repl.Reply](bytes.immutable(using Unsafe))).lay(Nil):
        case Repl.Reply.SessionList(_, names) => names
        case _                                => Nil

    catch case _: Exception => Nil
    finally if channel != null then channel.close()

  val names: List[Text] = socketPaths(socketDirectory).bind[List[Text], Text, List[Text]](ask)
  List.from(names.stdlib.distinct)

// The default TCP port for `flame listen` (a remote-reachable REPL server) and for `flame --host`
// when no `--port` is given. Arbitrary, but stable so the two ends agree without configuration.
val defaultPort: Int = 4319

// The client's command dispatch. The `@main` entry point and burdock's `externalize` wrapper live
// alone in the `launcher` module (`src/launcher/flame_launcher.scala`), which depends on this module
// (and `core`/`web`) as PUBLISHED Maven artifacts — so `externalize` records their Central jar hashes
// and the repackager turns them into on-demand `Burdock-Require` downloads instead of inlining them.
def runClient(): Unit =
  cli:
    // The project's `.pyrocosm/flame/config.tel`, resolved from the INVOCATION's working directory
    // (each daemon client has its own), never the daemon process's. Its startup settings, classpath
    // and connection defaults are folded into every launch below.
    val workspace: Workspace.Config = Workspace.config(summon[Cli].workingDirectory.directory())

    // The prompt-history file and entry limit for this project (see `flame.History`): resolved from
    // the same invocation working directory, and Unset (no persistence) unless a `.pyrocosm/flame`
    // directory exists at or above it.
    val history: Workspace.HistoryConfig =
      Workspace.historyConfig(summon[Cli].workingDirectory.directory())

    // This invocation's directory and environment, reported to the session (see `ReplContext`).
    val context: ClientContext =
      ClientContext(summon[Cli].workingDirectory.directory(), environmentPairs(summon[Cli].environment))

    arguments match
      // `flame -<flag>…` — the terminal REPL with options: `-s NAME` joins a session, `--host HOST`
      // (with optional `--port`) connects to a remote server (`flame listen`), and the settings flags
      // enable settings on startup. This case fires only when the first token is a FLAG (`head` begins
      // with `-`), which cannot be a subcommand.
      //
      // It is matched FIRST so that, when the word being completed is a flag, the subcommand patterns
      // below are never evaluated. Matching a `Subcommand` also SUGGESTS it, and a suggestion at the
      // cursor takes precedence over the flag list (`Completion`'s `cursorSuggestions` wins over
      // `flagSuggestions`) — so trying the subcommands first left `flame --exp<TAB>` offering
      // `serve`/`listen`/`install` and no flags at all. Ordering costs nothing at run time: a
      // subcommand never begins with `-`, so no invocation changes meaning.
      case Argument(head) :: _ if head.starts(t"-") =>
        // EVERY flag this command accepts is read here, unconditionally and before any `command`,
        // so that all of them register and are offered together for `flame --<TAB>`. Reading them
        // lazily — at the point each is needed — would register only those on the branch actually
        // taken, and reading them inside `command` would register none of them at all.
        val settings: List[Text]     = startupCommands(flaggedSettings, workspace)
        val basic:    Boolean        = Basic().present
        val host:     Optional[Text] = Host().value.or(workspace.host)
        val port:     Int            = Port().value.or(workspace.port).or(defaultPort)
        // Both flags are read (registering them, and `--join`'s completion) before any branch.
        val intent:   SessionIntent  = sessionIntent(arguments, joinFlag(), Create(), workspace)

        // `flame --basic` — a minimal, self-contained, in-process synchronous REPL (no socket
        // server, no TUI). Checked before `--host`, so it never falls through to a remote/socket
        // connection; it still honours the startup settings. `--join`/`--create` do not apply to
        // its single in-process session, so they are simply ignored here.
        if basic then command(basicRepl(settings))
        else intent match
          case SessionIntent.Invalid(message) => command(sessionArgError(message))
          case intent => host match
            case host: Text => command(connectRemote(host, port, intent, settings, history, context))
            case _          => command(connectSocket(intent, settings, history, context))

      // `flame serve [--port N | -p N]` — the web front-end (default port 8080). `Port()` registers
      // the flag (so it is offered in tab-completion) and reads its value; the pure `Int`
      // interpreter above yields `Unset` for an absent or non-numeric value, so `.or(8080)` falls back.
      //
      // The read is OUTSIDE `command`: in completion mode `execute` returns without running its
      // block at all, so a flag read within it would never register and never be suggested.
      case Serve() :: _ =>
        val port: Int = Port().value.or(workspace.port).or(8080)
        command(httpServe(port))

      // `flame install` — install this command's tab-completions into the user's shell.
      case Install() :: _ =>
        command(installCompletions())

      // `flame listen [--port N | -p N]` — a REPL server bound to a TCP port (not a per-process UNIX
      // socket), so a `flame --host` client on another machine can reach its sessions. Defaults to
      // `defaultPort`; a non-numeric `--port` also falls back to it.
      case Listen() :: _ =>
        val port: Int = Port().value.or(workspace.port).or(defaultPort)
        command(serve(port))

      // Internal: the per-process UNIX-socket REPL server that `connectSocket`/`launchServer`
      // spawns in the background (see `launchServer`). Not a user-facing command — `serve` now
      // serves the web front-end — so it uses a distinct argument the launcher passes itself.
      case Argument("serve-socket") :: Nil =>
        command(serveSocket())

      // `flame` — the terminal REPL (connects to, or starts, a background socket server). Bare `flame`
      // starts a new session, with whatever the workspace's `config.tel` asks for.
      case Nil =>
        sessionIntent(Nil, Join(), Create(), workspace) match
          case SessionIntent.Invalid(message) => command(sessionArgError(message))
          case intent => command(connectSocket(intent, startupCommands(Nil, workspace), history, context))

      case _ =>
        command(Exit.Fail(1))

// Runs a REPL server on the given TCP port and blocks until interrupted.
private def serve(portNumber: Int)(using Stdio, Monitor, Probate, System): Exit =
  given Scalac[3.9, Universe.Classfile] = Scalac(Nil)
  given Classloader = serverClassloader

  safely(urticose.Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
    recover:
      case Bind.Error(_) => Out.println(t"Port $portNumber is unavailable"); Exit.Fail(5)
      case error: Error => Out.println(t"${error.message}"); Exit.Fail(6)

    . protect:
        val sessions = Sessions(Repl.Rendering.Exhibit(ansi = true))
        val service  = sessions.serve(port)
        Out.println(t"Serving a REPL on port $portNumber (Ctrl+C or /quit to stop)")
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
  given Scalac[3.9, Universe.Classfile] = Scalac(Nil)
  given Classloader = serverClassloader

  val repl:   Repl[3.9]         = Repl.make[3.9](Repl.Prelude.empty)
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
            Out.println(t"Async mode is not available in --basic")
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
    case Interrupt.Int => quit.offer(()) yet SignalResponse.Accept

  // Printed from the client (not the daemon) so it reaches the user's terminal; the URL is
  // on its own line so terminals render it as a clean, clickable link.
  Out.println(t"Serving the web REPL (press Ctrl+C to stop):")
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
  import workingDirectories.javaBaseWorkingDirectory

  // The `DaemonService` (which extends `Entrypoint`) carries the daemon's tracked capabilities; seal
  // it to the pure `Entrypoint` the completions installer wants — it outlives this one call.
  given Entrypoint = caps.unsafe.unsafeAssumePure(service)

  recover:
    case error: exoskeleton.Install.Error =>
      Out.println(t"Could not install tab-completions")
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
  given Scalac[3.9, Universe.Classfile] = Scalac(Nil)
  given Classloader = serverClassloader

  val socketPath: Text = socketFile

  // Delete the socket on any exit (a clean stop, a crash, or a kill signal), so it
  // never lingers as a stale file a later client has to clean up.
  val removeSocket: Runnable = () =>
    recover:
      case _: Path.Error | _: Io.Error => ()
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
      case _: Path.Error | _: Io.Error => ()
    . protect:
      val directory = socketDirectory.as[Path on Linux]
      if !directory.existent() then directory.create[Directory]()
      socketPath.as[Path on Linux].wipe()

    val sessions = Sessions(Repl.Rendering.Exhibit(ansi = true))
    val service  = sessions.serve(socketPath)
    Out.println(t"Serving a REPL on $socketPath (Ctrl+C or /quit to stop)")
    sessions.awaitQuit()
    service.stop()

    recover:
      case _: Path.Error | _: Io.Error => ()
    . protect:
      socketPath.as[Path on Linux].wipe()

    Exit.Ok
  catch case error: Throwable =>
    Out.println(t"Could not serve on $socketPath: ${error.toString.tt}")
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
      case _: Path.Error | _: Io.Error => threadContextClassloader
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
          val urls = loader.getURLs.nn.iterator.map(_.nn).filter: entry =>
            safely(ji.File(entry.toURI).getCanonicalPath.nn.tt) != executablePath

          List.from(urls)
        case _ =>
          Nil

      new Classloader(jn.URLClassLoader((url :: bootstrapUrls).stdlib.toArray, threadContextClassloader.java))
  catch case _: Throwable => threadContextClassloader

private def invalidPort(portNumber: Int)(using Stdio): Exit =
  Out.println(t"$portNumber is not a valid TCP port")
  Exit.Fail(2)

private def missingHost(using Stdio): Exit =
  Out.println(t"The --host flag needs the hostname of a flame server to connect to")
  Exit.Fail(2)

private def invalidHost(host: Text)(using Stdio): Exit =
  Out.println(t"'$host' is not a valid hostname")
  Exit.Fail(2)

private def unreachableRemote(host: Text, portNumber: Int)(using Stdio): Exit =
  Out.println(t"Could not connect to $host:$portNumber")
  Exit.Fail(3)

// Runs `body` over a fresh TCP connection to `endpoint` — always closing it afterwards —
// or returns `Unset` if the connection is refused. `Connectable.tcpEndpoint` is honestly tracked
// (`^{online, caps.any}`), and its `caps.any` cannot flow into the fresh capture variable the
// `duplex` loan introduces. The loan confines the connection to `body`, so a pure connectable is
// sound here: seal it for this call. (The domain-socket connectable carries no `caps.any`, so
// `connectDomain` needs no such seal.)
private def connect[result](endpoint: Endpoint[Tcp.Port])(body: Duplex => result): Optional[result] =
  given (Endpoint[Tcp.Port] is Connectable) = caps.unsafe.unsafeAssumePure(Connectable.tcpEndpoint)
  try endpoint.duplex(body) catch case _: ji.IOException => Unset

// As `connect`, but over a UNIX domain socket.
private def connectDomain[result](socket: DomainSocket)(body: Duplex => result): Optional[result] =
  try socket.duplex(body) catch case _: ji.IOException => Unset

private def unreachableSocket(path: Text)(using Stdio): Exit =
  Out.println(t"Could not connect to $path")
  Exit.Fail(3)

private def failedToLaunch(using Stdio): Exit =
  Out.println(t"Could not start a REPL server")
  Exit.Fail(3)

// Reports a misuse of `--join`/`--create` (both at once, or a flag with no name) and fails, without
// attempting any connection.
private def sessionArgError(message: Text)(using Stdio): Exit =
  Out.println(message)
  Exit.Fail(2)

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
        if result.absent && !before.has(candidate) then
          result = connectDomain(DomainSocket(candidate))(body)

    result

// The full paths of the `.sock` files in the socket directory (an absent directory has no children,
// so this is empty on the first run; an undecodable directory path yields nothing).
private def socketPaths(directory: Text): List[Text] =
  recover:
    case _: Path.Error => Nil
  . protect:
    val names: Chain[Text] = directory.as[Path on Linux].children.map(_.encode)
    List.from(names.stdlib.filter(_.ends(t".sock")))

// Connects to a per-process UNIX domain socket. With no server running, launches one
// in the background and attaches to it (so `flame` alone is a self-contained REPL,
// reconnectable later); with exactly one, connects to it; with several, lists them.
// What this client reports to the server for the session's ambient contextual values (see
// `ReplContext`): the invocation's working directory and its environment.
private case class ClientContext(workingDirectory: Text, environment: List[Repl.Pair])

// The invocation's environment as pairs. Ethereal hands the client's variables over as an opaque
// `LazyEnvironment` (a `KEY=VALUE` list behind `variable(name)` alone), so the list is read back
// reflectively; any other `Environment`, or a failure to read, reports nothing, and the server then
// falls back to the daemon's own environment.
private def environmentPairs(environment: Environment): List[Repl.Pair] =
  try
    val field = environment.getClass.getDeclaredField("variables").nn
    field.setAccessible(true)

    field.get(environment) match
      case lines: scala.collection.immutable.List[?] =>
        val pairs: scala.collection.immutable.List[Repl.Pair] = lines.flatMap:
          case line: String =>
            val parts = line.split("=", 2).nn
            if parts.length == 2 then scala.collection.immutable.List(Repl.Pair(parts(0).nn.tt, parts(1).nn.tt))
            else scala.collection.immutable.Nil
          case _ => scala.collection.immutable.Nil

        List.from(pairs)

      case _ => Nil
  catch case _: Exception => Nil

private def connectSocket(intent: SessionIntent, initial: List[Text], history: Workspace.HistoryConfig, context: ClientContext)
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
        case _: Path.Error | _: Io.Error => ()
      . protect:
        path.as[Path on Linux].wipe()
    else live += path

  live.to(List) match
    case Nil =>
      Out.println(t"Starting a REPL server…")
      launchServer()(converse(intent, initial, history, context)(_)).or(failedToLaunch)

    case path :: Nil =>
      connectDomain(DomainSocket(path))(converse(intent, initial, history, context)(_)).or(unreachableSocket(path))

    case paths =>
      Out.println(t"Several REPL servers are running:")

      paths.each: path =>
        Out.println(t"  $path")

      Out.println(t"Stop all but one, or connect to a specific one with 'flame --host localhost'")
      Exit.Fail(7)

// Connects the terminal REPL to a flame server on another host over TCP (`flame --host HOST
// [--port N]`, reaching a server started with `flame listen`), optionally joining a named session
// with `-s`/`--session`. The transport differs from `connectSocket` (a TCP endpoint rather than a
// local UNIX domain socket), but the conversation — the `converse`/`runRepl` loop — is identical, so
// a remote session behaves exactly like a local one.
private def connectRemote(host: Optional[Text], portNumber: Int, intent: SessionIntent, initial: List[Text], history: Workspace.HistoryConfig, context: ClientContext)
    (using Stdio, Monitor, Probate, Console, Environment, System)
:   Exit =
  host.lay(missingHost): hostText =>
    safely(urticose.Port[Tcp](portNumber)).lay(invalidPort(portNumber)): port =>
      recover:
        case Hostname.Error(_, _) => invalidHost(hostText)
      . protect:
          val endpoint: Endpoint[Tcp.Port] = hostText.as[Hostname] on port
          connect(endpoint)(converse(intent, initial, history, context)(_)).or(unreachableRemote(hostText, portNumber))

// The terminal conversation: the REPL interface on Pyrocosm's inline terminal frontend, driven
// by a socket engine. Ctrl+C/Ctrl+D leave the session; `/quit` stops the server first.
private def converse(intent: SessionIntent, initial: List[Text], historyConfig: Workspace.HistoryConfig, context: ClientContext)(duplex: Duplex)
    (using Stdio, Monitor, Probate, Console, Environment)
:   Exit =
  // The kitty keyboard protocol makes the terminal report Shift+Enter distinctly, so the
  // editor can submit on it.
  import terminalFeatures.kittyKeyboardFeature
  import tableStyles.thickTableStyle
  import palettes.solarizedDarkGaugePalette

  // The inline block renders relative to the cursor, so each prompt appears where output has
  // flowed to, right after the previous result, and never docks to a screen edge.
  given InlineAnchoring = InlineAnchoring.Flow

  // The persisted prompt history (see `flame.History`), most recent `limit` entries; a file
  // already over the limit is trimmed on load, so it cannot grow without bound across sessions.
  val loaded: List[Text] = historyConfig.file.lay(Nil: List[Text]): file =>
    val all: sci.List[Text] = flame.History.load(file)
    val kept: sci.List[Text] = all.takeRight(historyConfig.limit)
    if all.length > historyConfig.limit then flame.History.replace(file, List.from(kept))
    List.from(kept)

  recover:
    case Terminal.Error() =>
      Out.println(t"The terminal could not be initialised")
      Exit.Fail(4)

  . protect:
      val frontend: TerminalFrontend = caps.unsafe.unsafeAssumePure(TerminalFrontend(ultimatum.Occupancy.Inline, FlameTheme))
      val engine: SocketEngine = caps.unsafe.unsafeAssumePure(SocketEngine(duplex))

      val options: ReplInterface.Options =
        ReplInterface.Options
          ( session = intent match
              case SessionIntent.Join(name)   => ReplInterface.Session.Join(name)
              case SessionIntent.Create(name) => ReplInterface.Session.Create(name)
              case _                          => ReplInterface.Session.Default,
            context = (context.workingDirectory, context.environment),
            commands = initial,
            history = loaded,
            historyLimit = historyConfig.limit,
            persist = { (line: Text) => historyConfig.file.let { (file: Text) => flame.History.append(file, line) } },
            leave = () => frontend.stop() )

      val interface: ReplInterface = caps.unsafe.unsafeAssumePure(ReplInterface(engine, options))
      engine.start(() => frontend.stop())
      interface.start()
      frontend.run(interface.interface)(interface.handle)
      engine.close()

      interface.failure.lay(Exit.Ok) { (message: Text) => Exit.Fail(10) }

// The `--basic` mode's rendering of a reply: captured output, the `name = value : type` line and
// the diagnostics, as text the terminal wraps itself.
private def replyText(reply: Repl.Reply): Text = reply match
  case Repl.Reply.Ran(_, value, output, tpe, name, diagnostics, _, _, _) =>
    // The value is shown only when there is one, as `name = value : type`, each part omitted
    // when absent, syntax-coloured like code: the binding name as a binding, `=` and `:` as
    // punctuation, the type in the type colour, and a singleton's widening faint after `<:`.
    val valueLine: Text =
      val rendered: Text = value.or(t"")
      if rendered == t"" then t"" else
        val binding: Teletype =
          name.lay(e"") { each => e"$Italic(${palette.scalaTerm}($each)) ${palette.scalaParenthesis}(=) " }

        val typed: Teletype =
          tpe.lay(e""): each =>
            val widened: Teletype =
              each.base.lay(e""): base =>
                e" $Faint(${palette.scalaSymbol}(<:) ${palette.scalaType}($base))"

            e"${palette.scalaSymbol}(:) ${palette.scalaType}(${each.text})$widened"

        t"${e"$binding$rendered$typed".render(xtermTrueColorTermcap)}\n"

    val diag: Text = if diagnostics != t"" then t"$diagnostics\n" else t""
    t"$output$valueLine$diag"

  case Repl.Reply.Threw(_, output, diagnostics, _, _, _) => t"$output$diagnostics\n"
  case Repl.Reply.Rejected(_, diagnostics, _, _)         => t"$diagnostics\n"
  case Repl.Reply.Crashed(_, diagnostics, _, _)          => t"$diagnostics\n"
  case Repl.Reply.Failed(_, message)                     => t"$message\n"
  case _                                                 => t""

private given palette: ScalaSyntaxPalette = SemanticRender.palette
