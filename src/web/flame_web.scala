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

import java.util.concurrent.atomic as juca
import scala.collection.concurrent.TrieMap

import scala.caps

import soundness.*
import perihelion.*
import perihelion.given

import htmlDoms.whatwg.*
import Control.*

import charEncoders.utf8Encoder
import classloaders.threadContextClassloader
import formatting.compactJsonFormatting
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.virtualThreading
import webserverErrorPages.minimalErrorPage

// A web-based front-end for the Flame REPL that mirrors the terminal client's live
// editing: every keystroke is sent to the server, which tokenizes the line with the same
// fast lexer the CLI uses (`Repl.tokenize`, no compiler) and returns the Harlequin accent
// of each token; the browser re-paints the line accordingly. Submitting a line runs it
// through the same typechecked engine (`Repl.react`) as the socket client, and the result
// is appended to a scrollback log. The editor cannot be a `<textarea>` (those can't carry
// styled spans), so it is a contenteditable `<code>` element driven by `replScript`.

// Messages exchanged with the browser as JSON over the WebSocket. Flat case classes (not
// enums) so the JSON shape — `{"kind":…,"seq":…,…}` — is predictable for the JavaScript.

// The JSON REST API's request/response bodies (served alongside the browser UI and WebSocket — see
// the `/api/…` routes in `serveHttp`). Flat case classes, so jacinta derives their JSON codecs
// automatically.
case class ApiEval(code: Text)                        // POST body for `/api/sessions/{name}/eval`
case class ApiComplete(code: Text, offset: Int)       // POST body for `/api/sessions/{name}/complete`
case class ApiCompletion(name: Text, kind: Text, signature: Text)  // one completion candidate
case class ApiCompletions(completions: List[ApiCompletion])        // the completion response
case class ApiSessionsList(sessions: List[Text])      // GET  /api/sessions
case class ApiSessionCreated(session: Text)           // POST /api/sessions
case class ApiError(error: Text)                      // a 400/404 error body

// An evaluation result: `status` is `"ran"` or `"error"`; the remaining fields mirror the plain-text
// REPL result (`name = value : tpe`, plus captured stdout and any diagnostics), each empty when absent.
case class ApiResult
   ( status:      Text,
     value:       Text = t"",
     tpe:         Text = t"",
     output:      Text = t"",
     name:        Text = t"",
     diagnostics: Text = t"",
     base:        Text = t"" )   // the type `tpe` widens to, when `tpe` is a singleton

// The browser-side editor and styling. No `$` (it would interpolate) and `\n` is written
// `\\n` so the served script carries a real newline escape. Selects its two controls by
// tag — the page has exactly one `<pre>` (the log) and one `<code>` (the editor).

// Maps a typed `Repl.Reply` (from `react`) to the flat JSON result the REST API returns. Unlike
// `resultReply`, this carries no highlight tokens and the value is plain text (the API's sessions
// render in `Rendering.Inspect`), so it is safe to serialize straight to JSON.
private def apiResult(reply: Repl.Reply): ApiResult = reply match
  case Repl.Reply.Ran(_, value, output, tpe, name, diagnostics, _, _, _) =>
    ApiResult
     ( t"ran", value.or(t""), tpe.let(_.text).or(t""), output, name.or(t""), plain(diagnostics),
       base = tpe.let(_.base).or(t"") )

  case Repl.Reply.Threw(_, output, diagnostics, _, _, _) =>
    ApiResult(t"error", output = output, diagnostics = plain(diagnostics))

  case Repl.Reply.Rejected(_, diagnostics, _, _) => ApiResult(t"error", diagnostics = plain(diagnostics))
  case Repl.Reply.Crashed(_, diagnostics, _, _)  => ApiResult(t"error", diagnostics = plain(diagnostics))
  case Repl.Reply.Failed(_, message)          => ApiResult(t"error", diagnostics = message)
  case _                                      => ApiResult(t"error")

// The REST API's sessions render as `Inspect`, so a diagnostic is coloured ANSI (types re-rendered
// through stenography); strip the ANSI so the JSON carries clean text with the stenography-rendered
// type names intact.
private def plain(diagnostics: Text): Text = SemanticRender.stripAnsi(diagnostics)

// The `Content-Type` for every JSON API response, and a helper that frames a JSON string as a
// fixed-length body under a given status (`Http.Status` carries an `apply(headers, body)`).
private val jsonHeaders: List[Http.Header] =
  List(Http.Header(t"content-type", t"application/json; charset=utf-8"))

private def jsonResponse(status: Http.Status, body: Text): Http.Response =
  status(jsonHeaders, Http.Body.Fixed(body.in[Data]))

// The web's engine: a connection to the in-process sessions, called directly. Replies to a
// request go to its callback; what the connection pushes of its own accord (an asynchronous
// submission's streamed output and eventual result) goes to the sink, as over the socket.
class LocalEngine(sessions: Sessions[3.9])(using Monitor, System, Probate) extends Engine:
  private val nextId: juca.AtomicInteger = juca.AtomicInteger(1)
  private val callbacks: TrieMap[Int, Repl.Reply -> Unit] = TrieMap()

  @volatile private var sink: Repl.Reply -> Unit = _ => ()

  private val connection: sessions.Connection =
    caps.unsafe.unsafeAssumePure(sessions.connection { (reply: Repl.Reply) => deliver(reply) })

  private def deliver(reply: Repl.Reply): Unit = reply match
    case Repl.Reply.Output(_, _, _) => sink(reply)
    case _ =>
      callbacks.remove(Repl.replyId(reply)) match
        case Some(callback) => callback(reply)
        case None           => sink(reply)

  def request(request: Int => Repl.Request)(reply: Repl.Reply => Unit): Unit =
    val id = nextId.getAndIncrement
    callbacks(id) = caps.unsafe.unsafeAssumePure(reply)
    connection.respond(request(id)).let(deliver)

  def send(request: Int => Repl.Request): Unit =
    connection.respond(request(nextId.getAndIncrement))
    ()

  def pushed(sink: Repl.Reply => Unit): Unit = this.sink = caps.unsafe.unsafeAssumePure(sink)
  def close(): Unit = ()

// The JSON REST API: a plain request/response HTTP API over an INDEPENDENT plain-text session
// registry, served alongside the browser UI. `POST /api/sessions` creates a session; `GET
// /api/sessions` lists them; `POST /api/sessions/{name}/eval` runs the JSON body's `code` on that
// session; `POST /api/sessions/{name}/complete` returns the tab-completion candidates at a cursor
// `offset` in `code`. Routing is exact-string, so the `{name}` path segment is peeled off by
// stripping the fixed prefix and suffix. Anything else is not the API's (`Unset`).
private def restApi(apiSessions: Sessions[3.9])(request: Http.Request)(using Monitor): Optional[Http.Response] =
  request.target match
    case t"/api/sessions" if request.method == Http.Post =>
      jsonResponse(Http.Ok, ApiSessionCreated(apiSessions.create()).in[Json].show)

    case t"/api/sessions" if request.method == Http.Get =>
      jsonResponse(Http.Ok, ApiSessionsList(apiSessions.names).in[Json].show)

    case target if request.method == Http.Post
                   && target.starts(t"/api/sessions/") && target.ends(t"/eval") =>
      val name: Text = target.chomp(t"/api/sessions/").chomp(t"/eval", Rtl)

      apiSessions.session(name).lay
       (jsonResponse(Http.NotFound, ApiError(t"no session named '$name'").in[Json].show)): repl =>
        // Read and parse the body as `{"code": "…"}`; a stream/parse failure yields `Unset` → 400.
        // `react` is synchronous (async mode has no REST analogue), so evaluate directly with a
        // fixed id.
        safely(request.body().memoize.utf8.read[Json].as[ApiEval]) match
          case eval: ApiEval =>
            jsonResponse(Http.Ok, apiResult(repl.react(0, eval.code)).in[Json].show)

          case _ =>
            jsonResponse(Http.BadRequest, ApiError(t"the request body must be JSON: {\"code\": \"…\"}").in[Json].show)

    case target if request.method == Http.Post
                   && target.starts(t"/api/sessions/") && target.ends(t"/complete") =>
      val name: Text = target.chomp(t"/api/sessions/").chomp(t"/complete", Rtl)

      apiSessions.session(name).lay
       (jsonResponse(Http.NotFound, ApiError(t"no session named '$name'").in[Json].show)): repl =>
        // Body is `{"code": "…", "offset": N}`; a stream/parse failure yields `Unset` → 400. The
        // cursor is clamped to the code, then the engine's completions at that point are returned:
        // Scala members/keywords, or the `/`-command completions when `code` starts with `/`.
        safely(request.body().memoize.utf8.read[Json].as[ApiComplete]) match
          case req: ApiComplete =>
            val offset: Int = req.offset.max(0).min(req.code.length)
            val items: List[ApiCompletion] =
              repl.completionsAt(req.code, offset).map: item =>
                ApiCompletion(item.name, item.kind, item.signature)

            jsonResponse(Http.Ok, ApiCompletions(items).in[Json].show)

          case _ =>
            jsonResponse
             (Http.BadRequest,
              ApiError(t"the request body must be JSON: {\"code\": \"…\", \"offset\": N}").in[Json].show)

    case _ => Unset

// Flame's web theme: the same Zed colours as the terminal's (see `FlameColours`).
object FlameWebTheme extends pyrocosm.WebTheme:
  import anticipation.Chroma
  def background: Chroma = Chroma(FlameColours.background)
  def surface: Chroma = Chroma(FlameColours.margin)
  def foreground: Chroma = Chroma(FlameColours.foreground)
  def muted: Chroma = Chroma(FlameColours.subdued)
  def border: Chroma = Chroma(FlameColours.subdued)
  def key: Chroma = Chroma(FlameColours.parameter)
  def reference: Chroma = Chroma(FlameColours.term)
  def figure: Chroma = Chroma(FlameColours.number)
  def units: Chroma = Chroma(FlameColours.comment)
  def link: Chroma = Chroma(FlameColours.string)
  def selection: Chroma = Chroma(FlameColours.selection)

  def tone(tone: pyrocosm.Tone): Chroma = tone match
    case pyrocosm.Tone.Success => Chroma(FlameColours.tpe)
    case pyrocosm.Tone.Failure => Chroma(FlameColours.error)
    case pyrocosm.Tone.Warning => Chroma(FlameColours.parameter)
    case pyrocosm.Tone.Muted   => Chroma(FlameColours.comment)
    case pyrocosm.Tone.Accent  => Chroma(FlameColours.term)
    case pyrocosm.Tone.Info    => Chroma(FlameColours.string)

  def accent(accent: pyrocosm.Token.Accent): Chroma = accent match
    case pyrocosm.Token.Accent.Keyword  => Chroma(FlameColours.keyword)
    case pyrocosm.Token.Accent.Modifier => Chroma(FlameColours.keyword)
    case pyrocosm.Token.Accent.Command  => Chroma(FlameColours.keyword)
    case pyrocosm.Token.Accent.String   => Chroma(FlameColours.string)
    case pyrocosm.Token.Accent.Number   => Chroma(FlameColours.number)
    case pyrocosm.Token.Accent.Term     => Chroma(FlameColours.term)
    case pyrocosm.Token.Accent.Typal    => Chroma(FlameColours.tpe)
    case pyrocosm.Token.Accent.Symbol   => Chroma(FlameColours.symbol)
    case pyrocosm.Token.Accent.Parens   => Chroma(FlameColours.operator)
    case pyrocosm.Token.Accent.Error    => Chroma(FlameColours.error)
    case pyrocosm.Token.Accent.Unparsed => Chroma(FlameColours.foreground)

// Serves the web REPL on `port`: the REPL interface on Pyrocosm's web frontend, a session per
// page, with the REST API answering what the frontend does not. The embedded engine's compile
// classpath comes from the supplied `Classloader`, so each caller passes the loader matching how
// it was launched: the standalone `web` main uses the thread-context loader (correct when run
// from the fat assembly), while the CLI client passes its `serverClassloader` (correct under the
// Burdock/Ethereal launcher). Blocks until `quit` is fulfilled (the CLI completes it on Ctrl+C;
// the standalone `web` main passes one that is never fulfilled and relies on the JVM's own
// signal handling).
def serveHttp(port: Int, quit: Promise[Unit])(using Monitor, System, Probate, Classloader): Unit =
  given Scalac[3.9, Universe.Classfile] = Scalac(Nil)

  // Each page auto-starts a fresh randomly-named session and may `/session`-switch to any other.
  // Results are exhibited as blocks, without terminal colour.
  val sessions = Sessions(Repl.Rendering.Exhibit(ansi = false))

  // The REST API's sessions are an INDEPENDENT registry, rendering results as plain text, so an
  // API caller gets a `value` field it can use. API session names and browser session names never
  // collide (separate `Sessions`).
  val apiSessions = Sessions(Repl.Rendering.Inspect)

  val frontend: pyrocosm.WebFrontend =
    caps.unsafe.unsafeAssumePure(pyrocosm.WebFrontend(port, FlameWebTheme, { (request: Http.Request) => restApi(apiSessions)(request) }))

  async:
    frontend.serve: () =>
      val engine: LocalEngine = caps.unsafe.unsafeAssumePure(LocalEngine(sessions))
      val options = ReplInterface.Options(navigation = true, quitAllowed = false)
      val interface: ReplInterface = caps.unsafe.unsafeAssumePure(ReplInterface(engine, options))
      interface.start()
      (interface.interface, interface.handle)

  java.lang.System.out.nn.println("Serving the web REPL (press Ctrl+C to stop):")
  java.lang.System.out.nn.println(s"  http://localhost:$port/")
  quit.attend()
  frontend.stop()

@main
def web(): Unit =
  given Classloader = threadContextClassloader
  supervise(serveHttp(8080, Promise[Unit]()))
