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

import pyrocosm.{Action, Block, Control, Event, Hints, Inline, Input, Interface, Language, Panel, Token, Tone, hints}
import pyrocosm.Status as Gauge

// The REPL as a Pyrocosm interface: a transcript of submissions and their results, a prompt,
// and, for the web, the sessions to choose from. One of these per session, driven by an
// `Engine`, and shown by whichever frontend: the terminal commits its settled entries to the
// scrollback; the web scrolls. Nothing here names a key, a colour or a pixel.
object ReplInterface:
  // Which session to start in: the server's choice, an existing one, or a new one.
  enum Session:
    case Default
    case Join(name: Text)
    case Create(name: Text)

  case class Options
    ( title:       Text       = t"Flame",
      navigation:  Boolean    = false,             // a panel of sessions (the web)
      quitAllowed: Boolean    = true,              // `/quit` stops the server; not from a page
      session:     Session    = Session.Default,
      context:     Optional[(Text, List[Repl.Pair])] = Unset,   // the working directory and environment
      commands:    List[Text] = Nil,               // submitted first: `/set …`, `/language …`, `/classload …`
      history:     List[Text] = Nil,               // earlier submissions, oldest first
      historyLimit: Int       = 100,
      persist:     Text => Unit = _ => (),         // a submission worth remembering
      leave:       () => Unit = () => () )         // `/quit` and `/disconnect`: the frontend should stop

  // The delay before a completion is asked for after a keystroke, so typing is not a query
  // per character.
  private val ghostDelay: Long = 200L

  private def muted(text: Text): Inline = Inline.Toned(Tone.Muted, Inline.text(text))

  private def blankLastLine(text: Text): Boolean =
    text.contains(t"\n") && text.s.substring(text.s.lastIndexOf('\n') + 1).nn.trim.nn.isEmpty

  private def stripTrailingBlank(text: Text): Text =
    if blankLastLine(text) then text.s.substring(0, text.s.lastIndexOf('\n')).nn.tt else text

class ReplInterface(engine: Engine, options: ReplInterface.Options)(using Monitor, Probate):
  import ReplInterface.*

  val transcript: pyrocosm.Live[List[Block]] = pyrocosm.Live(Nil)
  val sessions:   pyrocosm.Live[List[Block]] = pyrocosm.Live(Nil)

  val field: Control.Field =
    Control.Field
      ( Input(t"repl"), Control.Field.Kind.Code(Language.Scala),
        notification = Control.Field.Notify.Keystrokes,
        history = pyrocosm.Live(options.history) )

  val interface: Interface =
    val navigation: List[Panel] =
      if options.navigation then List(Panel(Panel.Id(t"sessions"), Panel.Role.Navigation, Inline.text(t"Sessions"), sessions, Panel.Priority.Important)) else Nil

    val main: List[Panel] =
      List
        ( Panel(Panel.Id(t"transcript"), Panel.Role.Transcript, Unset, transcript, Panel.Priority.Essential, hints = Hints(hints.Follow, hints.terminal.Border.None)),
          Panel(Panel.Id(t"prompt"), Panel.Role.Prompt, Unset, pyrocosm.Live(Nil: List[Block]), Panel.Priority.Essential, controls = List(field), hints = Hints(hints.terminal.Border.Rules)) )

    Interface(Inline.text(options.title), navigation + main, hints = Hints(hints.terminal.Occupancy.Inline))

  // The entries whose reply is still to come, by request id, with the output streamed so far.
  private val pending: TrieMap[Int, Block] = TrieMap()
  private val outputs: TrieMap[Int, List[Block]] = TrieMap()
  private val codes:   TrieMap[Int, Block] = TrieMap()

  // The sessions offered for selection, by their action.
  private val choices: TrieMap[Text, Action] = TrieMap()

  // Why the session could not start, if it could not: the driver turns it into an exit status.
  @volatile var failure: Optional[Text] = Unset

  @volatile private var generation: Int = 0
  @volatile private var language: Boolean = false

  // The text the field's completions were asked for: they stay while the user types on into
  // the same identifier, and go the moment the text changes otherwise.
  @volatile private var completedFor: Text = t""

  // The scope row as the server last reported it. It stays beneath the text while a keystroke's
  // own report is awaited, rather than vanishing and reappearing with every key.
  @volatile private var scopeRow: List[Block] = Nil
  private var count: Int = 0

  engine.pushed(reply => synchronized(pushed(reply)))

  // ── Startup ────────────────────────────────────────────────────────────────────────────

  def start(): Unit = synchronized:
    val open: Int => Repl.Request = options.session match
      case Session.Default      => Repl.Request.Session(_, t"")
      case Session.Join(name)   => Repl.Request.Join(_, name)
      case Session.Create(name) => Repl.Request.Create(_, name)

    engine.request(open): reply =>
      synchronized:
        reply match
          case Repl.Reply.Session(_, name, names, outcome) =>
            outcome match
              case Repl.SessionOutcome.Missing =>
                failure = Repl.messages.noSession(name)
                notice(Tone.Failure, Repl.messages.noSession(name))
                options.leave()

              case Repl.SessionOutcome.Exists =>
                failure = Repl.messages.sessionExists(name)
                notice(Tone.Failure, Repl.messages.sessionExists(name))
                options.leave()

              case Repl.SessionOutcome.Created =>
                note(Repl.messages.started(name))
                listSessions(names, name)
                begin()

              case _ =>
                note(Repl.messages.session(name))
                listSessions(names, name)
                begin()

          case _ =>
            notice(Tone.Failure, t"No response from the server")

  // The context, then the startup commands one after another, each awaited so its effect
  // (`/set experimental` before a feature) precedes the next.
  private def begin(): Unit =
    options.context.let { (directory, environment) => engine.send(Repl.Request.Context(_, directory, environment)) }

    def run(commands: List[Text]): Unit = commands match
      case Nil            => ()
      case command :: rest => submit(command, () => synchronized(run(rest)))
      case _              => ()

    run(options.commands)

  // ── The transcript ─────────────────────────────────────────────────────────────────────

  private def append(block: Block): Unit = transcript.append(block)

  private def note(text: Text): Unit = append(Block.Paragraph(List(muted(text))))

  private def notice(tone: Tone, text: Text): Unit = append(Block.Notice(tone, Unset, List(Block.paragraph(text))))

  // An entry keeps the frame its line was entered at: a rule above and below the code.
  private def framed(code: Block): List[Block] = List(Block.Rule(Block.Side.Above), code, Block.Rule(Block.Side.Below))

  private def entry(id: Int): Block =
    val code: Block = codes.getOrElse(id, Block.Code(Language.Scala, Nil))
    Block.Group(framed(code) + outputs.getOrElse(id, Nil) + List(Block.Gauge(Gauge.Indeterminate(), Inline.text(t"evaluating"))))

  private def replace(id: Int, settled: Block): Unit =
    pending.get(id).foreach: current =>
      transcript.amend { entries => entries.map { (block: Block) => if block eq current then settled else block } }
      pending(id) = settled

  // The reply carries the whole of the run's output itself, so the chunks streamed while it
  // ran are not kept alongside it.
  private def settle(id: Int, blocks: List[Block]): Unit =
    val code: Block = codes.getOrElse(id, Block.Code(Language.Scala, Nil))
    replace(id, Block.Group(framed(code) + blocks))
    pending.remove(id); outputs.remove(id); codes.remove(id)

  // A reply's content as blocks: those it carries, or, from a server rendering text, its text.
  private def content(reply: Repl.Reply): List[Block] = reply match
    case Repl.Reply.Ran(_, value, output, tpe, name, diagnostics, _, spans, blocks) =>
      if blocks != t"" then Blocks.decode(blocks)
      else
        val shown: List[Inline] = value.lay(Nil: List[Inline]) { (value: Text) => Inline.text(SemanticRender.stripAnsi(value)) }
        val result: List[Block] = if shown.nil && tpe.absent then Nil else Blocks.result(name, if shown.nil then Unset else Inline.Phrase(shown), tpe)
        val warnings: List[Block] = if diagnostics == t"" then Nil else List(Block.Notice(Tone.Warning, Unset, List(Block.paragraph(SemanticRender.stripAnsi(diagnostics)))))
        Blocks.output(output, spans, ansi = false) + result + warnings

    case Repl.Reply.Rejected(_, diagnostics, _, blocks) =>
      if blocks != t"" then Blocks.decode(blocks)
      else List(Block.Notice(Tone.Failure, Unset, List(Block.paragraph(SemanticRender.stripAnsi(diagnostics)))))

    case Repl.Reply.Threw(_, output, diagnostics, _, spans, blocks) =>
      if blocks != t"" then Blocks.decode(blocks)
      else
        val failure: List[Block] = List(Block.Notice(Tone.Failure, Unset, List(Block.paragraph(SemanticRender.stripAnsi(diagnostics)))))
        Blocks.output(output, spans, ansi = false) + failure

    case Repl.Reply.Crashed(_, diagnostics, _, blocks) =>
      if blocks != t"" then Blocks.decode(blocks)
      else List(Block.Notice(Tone.Failure, Unset, List(Block.paragraph(SemanticRender.stripAnsi(diagnostics)))))

    case Repl.Reply.Failed(_, message) =>
      List(Block.Notice(Tone.Failure, Unset, List(Block.paragraph(message))))

    case _ =>
      Nil

  // The submitted line, highlighted as the reply reports it (with its error spans), else as
  // the lexer alone sees it.
  private def code(text: Text, reply: Optional[Repl.Reply]): Block =
    val highlight: List[Repl.Token] = reply match
      case Repl.Reply.Ran(_, _, _, _, _, _, highlight, _, _) => highlight
      case Repl.Reply.Rejected(_, _, highlight, _)           => highlight
      case Repl.Reply.Threw(_, _, _, highlight, _, _)        => highlight
      case Repl.Reply.Crashed(_, _, highlight, _)            => highlight
      case _                                                 => tokens(text)

    Blocks.code(highlight)

  private def tokens(text: Text): List[Repl.Token] =
    if text.starts(t"/") then Blocks.command(text) else Repl.tokenize(text)

  // A submission: a pending entry, the request, and the entry settled by the reply. The entry
  // is made as the request is, once its id is known and before it is sent: an in-process
  // engine answers before `request` returns. `done` runs once the reply is in, for the startup
  // commands' sequencing.
  private def submit(text: Text, done: () => Unit = () => ()): Unit =
    val build: Int => Repl.Request = (id: Int) =>
      codes(id) = code(text, Unset)
      val block = entry(id)
      pending(id) = block
      append(block)
      Repl.Request.Submit(id, text)

    engine.request(build): reply =>
      synchronized:
        reply match
          case Repl.Reply.Pending(_) => ()                                    // the fill comes later, pushed
          case other =>
            val id = Repl.replyId(other)
            codes(id) = code(text, other)
            settle(id, content(other))
            done()

  private def pushed(reply: Repl.Reply): Unit = reply match
    case Repl.Reply.Output(id, chunk, stream) =>
      outputs(id) = outputs.getOrElse(id, Nil) + List(Block.Output(chunk, error = stream == Repl.stderrStream))
      replace(id, entry(id))

    case Repl.Reply.Pending(_) => ()

    case other =>
      val id = Repl.replyId(other)
      if pending.contains(id) then settle(id, content(other))

  // ── Sessions ───────────────────────────────────────────────────────────────────────────

  private def listSessions(names: List[Text], current: Text): Unit =
    if options.navigation then
      val items: List[Block.Item] = names.map: (name: Text) =>
        val action = choices.getOrElseUpdate(name, Action(name))
        val label: List[Inline] =
          if name == current then List(Inline.Toned(Tone.Accent, List(Inline.Symbol(pyrocosm.Glyph.ArrowRight))), Inline.Textual(t" "), Inline.Emphasis(Inline.text(name)))
          else Inline.text(name)
        Block.Item(List(Block.Paragraph(label)), action)

      sessions() = if items.nil then Nil else List(Block.Listing(false, items))

  private def switchTo(name: Text): Unit =
    engine.request(Repl.Request.Session(_, name)): reply =>
      synchronized:
        reply match
          case Repl.Reply.Session(_, current, names, outcome) =>
            if name == t"" then note(Repl.messages.sessionList(current, names))
            else if outcome == Repl.SessionOutcome.Created then note(Repl.messages.started(current))
            else note(Repl.messages.switched(current))
            listSessions(names, current)

          case _ =>
            notice(Tone.Failure, t"No response from the server")

  // ── The prompt ─────────────────────────────────────────────────────────────────────────

  private def naturalLanguage(text: Text): Boolean =
    !text.starts(t"/") && !text.starts(t" ") && !text.starts(t"\t") && text.trim != t""
      && Repl.classify(text) == Repl.Verdict.Language

  private def decorate(text: Text, caret: Int): Unit =
    generation += 1
    val current = generation
    val command = text.starts(t"/")
    val prose = !command && naturalLanguage(text)
    language = prose
    val multiline = text.contains(t"\n")
    val incomplete = !command && !prose && (Repl.incomplete(text) || (multiline && !blankLastLine(text)))

    val detail: List[Block] =
      if prose then List(Block.Paragraph(List(muted(t"reads as natural language; a space before it makes it Scala")))) else Nil

    if text == t"" then scopeRow = Nil

    val extending: Boolean =
      caret == text.length && text.starts(completedFor)
        && text.s.substring(completedFor.length).nn.forall { c => c.isLetterOrDigit || c == '_' }

    val kept: List[Control.Field.Completion] = if extending then field.decoration().completions else Nil
    field.decoration() = Control.Field.Decoration(tokens(text).map(Blocks.token), kept, incomplete, detail + scopeRow)

    if text != t"" then
      engine.request(Repl.Request.Tokenize(_, text)): reply =>
        synchronized:
          reply match
            case Repl.Reply.Tokenized(_, _, _, _, scope) if generation == current =>
              val row: List[Block] =
                if scope.nil then Nil
                else
                  // The scope as a context function: each wrapper's contextual values (outermost
                  // first, parenthesised when there are several) before its own `?=>`, so nested
                  // blocks read `outer ?=> inner ?=>`; the named bindings on a line beneath.
                  val contextual: scala.List[Repl.ScopeBinding] = scope.stdlib.filter(_.contextual)
                  val named: scala.List[Repl.ScopeBinding] = scope.stdlib.filterNot(_.contextual).sortBy(-_.level)

                  def commas(parts: scala.List[scala.List[Token]]): scala.List[Token] =
                    parts.zipWithIndex.flatMap { (part, index) => if index == 0 then part else Token(t", ", Token.Accent.Symbol) :: part }

                  val context: scala.List[Token] =
                    contextual.groupBy(_.level).toList.sortBy(-_._1).zipWithIndex.flatMap { case ((_, group), index) =>
                      val types = commas(group.map { binding => scala.List(Token(binding.tpe, Token.Accent.Typal)) })
                      val grouped = if group.length > 1 then Token(t"(", Token.Accent.Parens) :: types ::: scala.List(Token(t")", Token.Accent.Parens)) else types
                      (if index == 0 then scala.Nil else scala.List(Token(t" ", Token.Accent.Unparsed))) ::: grouped ::: scala.List(Token(t" ?=>", Token.Accent.Symbol)) }

                  val values: scala.List[Token] =
                    commas(named.map { binding => scala.List(Token(binding.name.or(t"_"), Token.Accent.Term, Token.Role.Binding), Token(t": ", Token.Accent.Symbol), Token(binding.tpe, Token.Accent.Typal)) })

                  // Two lines of one paragraph: the contextual values, then the named ones.
                  val lines: scala.List[scala.List[Token]] = scala.List(context, values).filter(_.nonEmpty)
                  val content: scala.List[Inline] = lines.zipWithIndex.flatMap { (tokens, index) =>
                    (if index == 0 then scala.Nil else scala.List(Inline.Break())) :+ Inline.Code(Language.Scala, List.from(tokens)) }

                  List(Block.Paragraph(List.from(content)))

              scopeRow = row
              field.decoration.amend(_.copy(detail = detail + row))

            case _ => ()

      // Completions, a moment after the last keystroke, when the caret ends an identifier.
      if !prose && caret == text.length && text.s.lastOption.exists { c => c.isLetterOrDigit || c == '_' || c == '/' } then
        async:
          snooze(ghostDelay.toDouble*Milli(Second))
          if generation == current then complete(text, caret, current, apply = false)

  private def whole(name: Text): Boolean = name.starts(t"/")

  // Ask for completions; `apply` puts a lone candidate straight into the text (a Tab), while
  // several become the field's list either way.
  private def complete(text: Text, caret: Int, current: Int, apply: Boolean): Unit =
    engine.request(Repl.Request.Complete(_, text, caret)): reply =>
      synchronized:
        reply match
          case Repl.Reply.Completed(_, items) if generation == current =>
            items.stdlib match
              case scala.List(single) if apply =>
                val name = single.name
                val replaced: Text =
                  if whole(name) then name
                  else
                    var start = caret
                    while start > 0 && { val c = text.s.charAt(start - 1); c.isLetterOrDigit || c == '_' } do start -= 1
                    t"${text.keep(start)}$name${text.skip(caret)}"

                field.value() = replaced
                decorate(replaced, replaced.length)

              case _ =>
                val completions: List[Control.Field.Completion] =
                  items.map { (item: Repl.CompletionItem) => Control.Field.Completion(item.name, item.kind, item.signature, whole(item.name)) }
                completedFor = text
                field.decoration.amend(_.copy(completions = completions))

          case _ => ()

  private def remember(text: Text): Unit =
    field.history.amend { history => List.from((history.stdlib :+ text).takeRight(options.historyLimit)) }
    options.persist(text)

  def handle(event: Event): Unit = synchronized:
    event match
      case Event.Edited(_, text, caret) => decorate(text, caret)

      case Event.Key(Keypress.Tab) =>
        val text = field.value()
        if !language && text != t"" then complete(text, text.length, generation, apply = true)

      case Event.Key(_) => ()

      case Event.Submitted(_, submitted) =>
        val text = stripTrailingBlank(submitted)
        scopeRow = Nil
        field.decoration() = Control.Field.Decoration()

        if text.trim != t"" then
          if field.history().stdlib.lastOption != Some(text) then remember(text)

          if text == t"/clear" then transcript() = Nil
          else if text == t"/disconnect" then options.leave()
          else if text == t"/quit" then
            if options.quitAllowed then
              engine.send(Repl.Request.Quit(_))
              options.leave()
            else note(t"A page cannot stop the server; close the tab to leave the session")
          else if text == t"/session" || text.starts(t"/session ") then switchTo(text.skip(t"/session".length).trim)
          else if text.starts(t"/") && !Repl.isCommand(text) then
            append(Block.Group(List(Blocks.code(Blocks.command(text)), Block.Notice(Tone.Warning, Unset, List(Block.paragraph(Repl.messages.unknownCommand(text)))))))
          else if naturalLanguage(text) then
            append(Block.Group(List(Block.paragraph(text), Block.Paragraph(List(muted(t"This reads as natural language, which flame cannot answer yet. To evaluate it as Scala, insert a space before it."))))))
          else submit(text)

      case Event.Pressed(action) =>
        choices.find(_._2 == action).foreach { (name, _) => switchTo(name) }

      case Event.Closed => engine.close()
      case _            => ()
