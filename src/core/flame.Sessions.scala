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
import rudiments.sortingAlgorithms.timsort
import stratiform.*
import obligatory.*
import socketBackends.javaBaseSockets
import turbulence.*
import zephyrine.*
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
      safely(cp"/nomenclature/animals.txt".read[Text].lines.map(_.trim).filter(_ != t""))

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
  def names: List[Text] = lock(registry.keys.to[List].order(_.s))

  def session(name: Text): Optional[Repl[version]] = lock(registry.at(name))

  // Registers a fresh session under a random animal name not already in use (falling back to a
  // numbered suffix in the astronomically-unlikely event every animal is taken), and returns the name.
  def create(): Text =
    lock:
      val free: List[Text] = Sessions.animals.filter { animal => !registry.defines(animal) }

      val name: Text =
        Random.global.shuffle(free).prim.or:
          var n = 2
          val base = Random.global.shuffle(Sessions.animals).prim.or(t"session")
          while registry.defines(t"$base$n") do n += 1
          t"$base$n"

      registry = registry.define(name, Repl.make[version](Repl.Prelude.empty, render))
      name

  // Registers a fresh session under the given `name` — the one a client asked for with `--session`
  // or `/session` — unless one of that name already exists. `true` when it was created.
  def open(name: Text): Boolean =
    lock:
      if registry.defines(name) then false
      else
        registry = registry.define(name, Repl.make[version](Repl.Prelude.empty, render))
        true

  // Closes every session (see `Repl#close`) and forgets them all: without this, each session's
  // warm compiler stays alive for as long as the process does.
  def close(): Unit =
    val closing: List[Repl[version]] = lock:
      val all = registry.values.to[List]
      registry = Map()
      all

    closing.each(_.close())

  // Serializes a `Reply` to BinTEL body bytes; a valid reply always type-assigns, so this is total.
  private def encode(reply: Repl.Reply): Data = unsafely(reply.bintel)

  // Starts a server on `target` — a TCP port or a UNIX domain socket — with each accepted
  // connection an interactive session over its own current session. Returns a handle whose
  // `stop()` shuts the server down.
  //
  // Coaxial's `listen` is a scoped loan (`listen(lambda)(block)`) that closes the service when its
  // block returns, which does not fit this "bind now, return a stoppable handle, block elsewhere
  // on `awaitQuit`" shape. The `Bindable` instance underneath it does: it binds, accepts a
  // `Duplex` per connection, and stops — the same socket machinery the coaxial client connects
  // with, so the two ends cannot drift apart.
  // The evidence is taken as plain `using` parameters rather than through the `logs`/`raises`
  // sugar: a context-function result would introduce a fresh root capability of its own, which
  // could not then flow into `serve`'s.
  private def accept[target](target: target)(using bindable: target is Bindable)
    ( using Monitor, System, Probate, bindable.Input =:= Duplex )
    ( using (CompileEvent is Loggable)^, Tactic[Bind.Error]^ )
  :   Socket.Service^ =

    val binding: bindable.Binding =
      safely(bindable.bind(target, Unset)).lest(Bind.Error(Bind.Error.Reason.PortInUse))

    @volatile var listening: Boolean = true

    val task = async:
      while listening do
        safely:
          val duplex: Duplex = summon[bindable.Input =:= Duplex](bindable.connect(binding))

          // Fire-and-forget: the fresh task handle is discarded (and the block yields `()`), so
          // the connection's capability is confined to this per-accept task and never leaks into
          // the enclosing accept loop's capture set.
          async:
            try converse(duplex) finally safely(duplex.close())

          ()

    // Vouched pure: the stop closure captures the accept task, whose capabilities outlive the
    // service (the caller `stop()`s it inside the same `supervise` scope).
    Sessions.vouchPure:
      Socket.Service: () =>
        listening = false
        safely(bindable.stop(binding))
        safely(task.await())
        ()

  def serve(port: Port over Tcp)(using Monitor, System, Probate)
  :   Socket.Service logs CompileEvent raises Bind.Error =
    accept(port)

  def serve(socketPath: Text)(using Monitor, System, Probate)
  :   Socket.Service logs CompileEvent raises Bind.Error =
    accept(DomainSocket(socketPath))

  // One client's connection: its current session (created lazily, so a probe leaves none
  // behind), its context, and `respond`, which answers a request or, for an asynchronous
  // submission, pushes `Pending`, the streamed output and the final reply through `send` and
  // answers nothing. Both the socket loop and the web front-end drive one of these.
  class Connection(send: Repl.Reply => Unit)(using Monitor, System, Probate):
    @volatile private var current: Optional[Text] = Unset

    def currentName: Text = current.or:
      val name = create()
      current = name
      name

    def name: Optional[Text] = current

    @volatile private var context: Optional[(Text, List[Repl.Pair])] = Unset

    private def applyContext(): Unit = context.let: (directory, environment) =>
      current.let: name =>
        session(name).let: repl =>
          val variables: Map[Text, Text] =
            environment.map { pair => (pair.key, pair.value) }.to[Map]

          ReplContext.set(repl.session, ReplContext.Values(directory, variables))

    def respond(request: Repl.Request): Optional[Repl.Reply] logs CompileEvent = request match
      case Repl.Request.Tokenize(id, code) =>
        val scope: List[Repl.ScopeBinding] = session(currentName).lay(Nil)(_.scopeAt(code))
        Repl.Reply.Tokenized(id, Repl.tokenize(code), Repl.incomplete(code),
            Repl.classify(code) == Repl.Verdict.Language, scope)

      case Repl.Request.Submit(id, code) =>
        session(currentName).lay(Repl.Reply.Failed(id, t"no active session")): repl =>
          if !repl.asyncEnabled then repl.react(id, code)
          else
            send(Repl.Reply.Pending(id))

            async:
              val reply: Repl.Reply =
                safely(repl.react(id, code, chunk => Repl.streamChunks(chunk).each { (stream, text) =>
                  send(Repl.Reply.Output(id, text, stream)) })).or
                 (Repl.Reply.Failed(id, t"the submission could not be processed"))

              send(reply)

            Unset

      case Repl.Request.Complete(id, code, offset) =>
        session(currentName).lay(Repl.Reply.Completed(id, Nil)): repl =>
          Repl.Reply.Completed(id, repl.completionsAt(code, offset))

      case Repl.Request.Session(id, name) =>
        val wasAbsent: Boolean = current.absent
        val created: Boolean = if name == t"" then wasAbsent else open(name)
        if name != t"" then current = name
        val outcome = if created then Repl.SessionOutcome.Created else Repl.SessionOutcome.Joined
        val reply = Repl.Reply.Session(id, currentName, names, outcome)
        applyContext()
        reply

      case Repl.Request.Join(id, name) =>
        if session(name).present then
          current = name
          applyContext()
          Repl.Reply.Session(id, name, names, Repl.SessionOutcome.Joined)
        else Repl.Reply.Session(id, current.or(t""), names, Repl.SessionOutcome.Missing)

      case Repl.Request.Create(id, name) =>
        if open(name) then
          current = name
          applyContext()
          Repl.Reply.Session(id, name, names, Repl.SessionOutcome.Created)
        else Repl.Reply.Session(id, current.or(t""), names, Repl.SessionOutcome.Exists)

      case Repl.Request.SessionList(id) =>
        Repl.Reply.SessionList(id, names)

      case Repl.Request.Context(_, directory, environment) =>
        context = (directory, environment)
        applyContext()
        Unset

      case Repl.Request.Quit(_) =>
        quit.offer(()) yet Unset

  // A connection holds its `send` and the monitor, both of which outlive it (they are the
  // socket's or the page's); vouched pure so it can be kept and called from anywhere.
  def connection(send: Repl.Reply => Unit)(using Monitor, System, Probate): Connection =
    caps.unsafe.unsafeAssumePure(new Connection(send))

  private def converse(duplex: Duplex)(using Monitor, System, Probate)
  :   Unit logs CompileEvent =

    import strategies.throwUnsafely

    val writes: Mutex = Mutex()

    // The one writer, so a reply pushed at any time cannot interleave with another: `Duplex.send`
    // leaves serialization to its caller.
    def send(payload: Data): Unit =
      writes:
        try duplex.send(zephyrine.Stream(LengthPrefix.encode(payload)))
        catch case _: Throwable => ()

    val connection: Connection = this.connection(reply => send(encode(reply)))

    try
      // One record per request, framed by its four-byte big-endian length. A framing or truncation
      // error (a client that vanished mid-record) raises, and the `catch` below ends the
      // conversation exactly as the end of the stream does.
      val requests = duplex.source.chunks.frames[LengthPrefix]

      while requests.hasNext do
        val message: Data = requests.next()

        async:
          val response: Optional[Repl.Reply] =
            try
              safely(Bintel.read[Repl.Request](message)).lay(Repl.Reply.Failed(0, t"the request could not be parsed")): request =>
                connection.respond(request)
            catch case error: Throwable => Repl.Reply.Failed(0, error.toString.tt)

          response.let { reply => send(encode(reply)) }
    catch case _: Throwable => ()

    // Decodes one request and dispatches on the CURRENT session: `tokenize` is stateless (the lexer);
    // `submit`/`complete` run on `current`'s `Repl`; `session` switches/reports sessions; `quit`
    // stops the whole server (all sessions). `Unset` means no reply is sent.
