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
import java.net as jn
import java.nio.channels as jnc

import scala.caps


import ambience.*
import anthology.*
import anticipation.*
import capricious.*
import coaxial.*
import contingency.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import parasite.*
import prepositional.*
import rudiments.*
import denominative.*
import symbolism.*
import denominative.dysasymptotics.linearSize
import stratiform.*
import turbulence.*
import urticose.*
import vacuous.*

import classloaders.threadContextClassloader
import hieroglyph.charDecoders.utf8Decoder
import hieroglyph.textSanitizers.skipSanitizer

// A named collection of independent REPL sessions served over one socket. Each connection is bound to
// a "current" session (a `Repl`), auto-assigned a fresh randomly-named session on connect, and may
// switch to any existing session with a `/session` command. This is the multi-session server that
// wraps the per-session engine (`Repl`); the socket protocol (`serve`/`converse`/`respond`) used to
// live on `Repl` itself, back when there was exactly one shared session.
object Sessions:
  // Vouches that `value`'s tracked captures do not outlive their scope, discarding its capture
  // set (Soundness's codec-thunk seal idiom, rep/DECISIONS.md). The explicitly
  // capture-polymorphic parameter (`value^`) stops a pure expected type propagating into the
  // type argument, which would instead demand purity of the argument itself.
  private def vouchPure[value](value: value^): value = caps.unsafe.unsafeAssumePure(value)

  // The animal names sessions are drawn from — read from Nomenclature's classpath resource, with a
  // small hard-coded fallback should the resource be unreadable.
  private val fallback: List[Text] =
    List(t"aardvark", t"badger", t"cheetah", t"dolphin", t"eagle", t"ferret", t"gazelle", t"heron",
         t"ibis", t"jaguar", t"koala", t"lemur", t"marmot", t"newt", t"otter", t"penguin", t"quokka",
         t"raccoon", t"salmon", t"tapir", t"urchin", t"vulture", t"walrus", t"yak", t"zebra")

  lazy val animals: List[Text] =
    val loaded: Optional[List[Text]] =
      safely(cp"/nomenclature/animals.txt".read[Text].cut(t"\n").map(_.trim).filter(_ != t""))

    loaded.let { list => if list.nil then Unset else list }.or(fallback)

class Sessions[version <: Scalac.Versions]
  ( render: Repl.Rendering = Repl.Rendering.Inspect )
  ( using scalac: Scalac[version, Universe.Classfile], classloader: Classloader,
    temporary: TemporaryDirectory ):

  private var registry: Map[Text, Repl[version]] = Map()
  private val lock:     Mutex                    = Mutex()
  private val quit:     Promise[Unit]            = Promise()

  // Fulfilled when a connected client sends a `Quit` request; the server host `attend`s it to block
  // until then and shut down cleanly.
  def awaitQuit()(using Monitor): Unit = quit.attend()

  // Every session's name, sorted — for the startup display and `/session` tab-completion.
  def names: List[Text] = lock(List.from(registry.keys.stdlib.toList.sortBy(_.s)))

  def session(name: Text): Optional[Repl[version]] = lock(registry.stdlib.get(name).optional)

  // Registers a fresh session under a random animal name not already in use (falling back to a
  // numbered suffix in the astronomically-unlikely event every animal is taken), and returns the name.
  def create(): Text =
    lock:
      val free: List[Text] = Sessions.animals.filter { animal => !registry.stdlib.contains(animal) }

      val name: Text =
        if !free.nil then Random.global.shuffle(free).stdlib.head else
          var n = 2
          val base = Random.global.shuffle(Sessions.animals).stdlib.head
          while registry.stdlib.contains(t"$base$n") do n += 1
          t"$base$n"

      registry = registry.define(name, Repl.make[version](Repl.Prelude.empty, render))
      name

  // Registers a fresh session under the given `name` — the one a client asked for with `--session`
  // or `/session` — unless one of that name already exists. `true` when it was created.
  def open(name: Text): Boolean =
    lock:
      if registry.stdlib.contains(name) then false
      else
        registry = registry.define(name, Repl.make[version](Repl.Prelude.empty, render))
        true

  // Serializes a `Reply` to BinTEL body bytes; a valid reply always type-assigns, so this is total.
  private def encode(reply: Repl.Reply): Data = unsafely(reply.bintel)

  // Starts a TCP server on `port`; each connection is an interactive session over its own current
  // session. Returns a handle whose `stop()` shuts the server down. Coaxial's `listen` is now a
  // scoped loan (`listen(lambda)(block)`) that closes the service when its block returns, and it
  // hands the lambda a kernel-stream `Duplex` rather than a stream-bearing `Connection` — neither
  // fits this "bind now, return a stoppable handle, block elsewhere on `awaitQuit`" shape. So the
  // TCP path binds a raw `ServerSocket` directly (exactly as the domain-socket path below binds a
  // raw NIO channel), keeping the byte-framed protocol wire-identical to the coaxial client.
  def serve(port: Port over Tcp)(using Monitor, System, Probate)
  :   Socket.Service logs CompileEvent raises Bind.Error =
    val server: jn.ServerSocket =
      try jn.ServerSocket(port.number)
      catch case _: ji.IOException => abort(Bind.Error(Bind.Error.Reason.PortInUse))

    @volatile var listening: Boolean = true

    val task = async:
      while listening do
        safely:
          val client: jn.Socket = server.accept().nn

          // Fire-and-forget: the fresh task handle is discarded (and the block yields `()`), so the
          // connection's `client` capability is confined to this per-accept task and never leaks
          // into the enclosing accept loop's capture set.
          async:
            try converse(client.getInputStream.nn, client.getOutputStream.nn)
            finally safely(client.close())

          ()

    // Vouched pure: the stop closure captures the accept task, whose capabilities outlive the
    // service (the caller `stop()`s it inside the same `supervise` scope).
    Sessions.vouchPure:
      Socket.Service: () =>
        listening = false
        safely(server.close())
        safely(task.await())
        ()

  // Serves over a UNIX domain socket at `socketPath`. Coaxial's domain-socket `Connection` does not
  // expose its streams for the bidirectional, asynchronously-written protocol this needs, so the
  // accept loop runs directly over an NIO channel.
  def serve(socketPath: Text)(using Monitor, System, Probate)
  :   Socket.Service logs CompileEvent =
    val address: jn.UnixDomainSocketAddress = jn.UnixDomainSocketAddress.of(socketPath.s).nn

    val channel: jnc.ServerSocketChannel =
      jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn

    channel.configureBlocking(true)
    channel.bind(address)

    @volatile var listening: Boolean = true

    val task = async:
      while listening do
        safely:
          val client: jnc.SocketChannel = channel.accept().nn
          val input  = jnc.Channels.newInputStream(client).nn
          val output = jnc.Channels.newOutputStream(client).nn

          // Fire-and-forget (as above): discard the task handle so `client` stays confined.
          async:
            try converse(input, output) finally safely(client.close())

          ()

    // Vouched pure: the stop closure captures the accept task, whose capabilities outlive the
    // service (the caller `stop()`s it inside the same `supervise` scope).
    Sessions.vouchPure:
      Socket.Service: () =>
        listening = false
        safely(channel.close())
        safely(task.await())
        ()

  private def converse(input: ji.InputStream, output: ji.OutputStream)
    ( using Monitor, System, Probate )
  :   Unit logs CompileEvent =

    // Each connection has a "current" session, created LAZILY on the first request — a bare probe
    // (the no-args CLI opens and immediately closes a connection to test whether a server is live)
    // sends nothing and so leaves no throwaway session behind. A `Session` request switches it.
    @volatile var current: Optional[Text] = Unset

    def currentName: Text = current.or:
      val name = create()
      current = name
      name

    val writes: Mutex = Mutex()
    val out: ji.DataOutputStream = ji.DataOutputStream(ji.BufferedOutputStream(output))

    // Frames one reply onto the socket, serialized by `writes` so the read loop's per-request replies
    // and any out-of-band async fills never interleave. Total: a failed write (a dropped client) is
    // swallowed. This is the ONLY writer, so an async run can push its reply through it at any later
    // time, not just in direct response to a request.
    def send(payload: Data): Unit =
      writes:
        try
          out.writeInt(payload.length)
          out.write(payload.mutable(using Unsafe))
          out.flush()
        catch case _: Throwable => ()

    // Decodes one request and dispatches on the CURRENT session: `tokenize` is stateless (the lexer);
    // `submit`/`complete` run on `current`'s `Repl`; `session` switches/reports sessions; `quit`
    // stops the whole server (all sessions). `Unset` means no reply is sent.
    def respond(message: Data): Optional[Data] =
      safely(Bintel.read[Repl.Request](message)).lay
       (encode(Repl.Reply.Failed(0, t"the request could not be parsed"))):

        case Repl.Request.Tokenize(id, code) =>
          // The scope the line has opened so far is the one thing about a tokenize that depends on
          // the session (its imports, history and classpath); a connection with no session yet — one
          // is created lazily — has no scope to report.
          val scope: List[Repl.ScopeBinding] = session(currentName).lay(Nil)(_.scopeAt(code))

          encode(Repl.Reply.Tokenized(id, Repl.tokenize(code), Repl.incomplete(code),
              Repl.classify(code) == Repl.Verdict.Language, scope))

        case Repl.Request.Submit(id, code) =>
          session(currentName).lay(encode(Repl.Reply.Failed(id, t"no active session"))): repl =>
            if !repl.asyncEnabled then encode(repl.react(id, code))
            else
              // Async mode: acknowledge with `Pending` IMMEDIATELY (sent directly, so it is written
              // before any streamed output), then run on a worker — streaming the run's stdout as
              // `Output` chunks as it appears — and push the final reply when it completes. `respond`
              // returns `Unset` because it has already sent everything itself.
              send(encode(Repl.Reply.Pending(id)))

              async:
                val reply: Repl.Reply =
                  safely(repl.react(id, code, chunk => send(encode(Repl.Reply.Output(id, chunk))))).or
                   (Repl.Reply.Failed(id, t"the submission could not be processed"))

                send(encode(reply))

              Unset

        case Repl.Request.Complete(id, code, offset) =>
          session(currentName).lay(encode(Repl.Reply.Completed(id, Nil))): repl =>
            encode(Repl.Reply.Completed(id, repl.completionsAt(code, offset)))

        case Repl.Request.Session(id, name) =>
          // Empty name only reports (the bare-launch default creates one lazily through
          // `currentName`); a known name switches; an unknown one is STARTED under that name and
          // switched to. The `/session` command and the bare launch use this.
          val wasAbsent: Boolean = current.absent
          val created: Boolean = if name == t"" then wasAbsent else open(name)
          if name != t"" then current = name
          val outcome = if created then Repl.SessionOutcome.Created else Repl.SessionOutcome.Joined
          encode(Repl.Reply.Session(id, currentName, names, outcome))


        case Repl.Request.Join(id, name) =>
          // `--join`: switch to `name` only if it exists; otherwise report `Missing` and touch
          // nothing (NOT `currentName`, which would create a throwaway session on the failure path).
          if session(name).present then
            current = name
            encode(Repl.Reply.Session(id, name, names, Repl.SessionOutcome.Joined))
          else encode(Repl.Reply.Session(id, current.or(t""), names, Repl.SessionOutcome.Missing))

        case Repl.Request.Create(id, name) =>
          // `--create`: start `name` only if it is free; otherwise report `Exists` and switch to
          // nothing, so a name clash is an error rather than a silent join.
          if open(name) then
            current = name
            encode(Repl.Reply.Session(id, name, names, Repl.SessionOutcome.Created))
          else encode(Repl.Reply.Session(id, current.or(t""), names, Repl.SessionOutcome.Exists))

        case Repl.Request.SessionList(id) =>
          // Report only — no `currentName`, so a bare probe (a tab-completion) creates nothing.
          encode(Repl.Reply.SessionList(id, names))

        case Repl.Request.Quit(_) =>
          quit.offer(()) yet Unset

    try
      val in: ji.DataInputStream = ji.DataInputStream(ji.BufferedInputStream(input))
      var continue: Boolean = true

      while continue do
        val length: Int = try in.readInt() catch case _: ji.IOException => -1

        if length < 0 then continue = false
        else
          val bytes: scala.Array[Byte] = new scala.Array[Byte](length)
          in.readFully(bytes)
          val message: Data = bytes.immutable(using Unsafe)

          async:
            val response: Optional[Data] =
              try respond(message)
              catch case error: Throwable => encode(Repl.Reply.Failed(0, error.toString.tt))

            response.let(send)

    catch case _: Throwable => ()
