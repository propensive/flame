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

import scala.caps

import soundness.*

// The terminal client's engine: a flame server at the other end of a socket, spoken to in
// length-framed BinTEL. Requests go out on the caller's thread; a reader task routes each reply
// to the callback registered under its id, and everything else (streamed output, an async
// submission's eventual result) to the push sink. Ids are minted here, so a reply's id is
// meaningful only to this engine.
object SocketEngine:
  // Serializes a `Request` to BinTEL body bytes. A valid request always type-assigns, so the
  // encode is total.
  def encode(request: Repl.Request): Data = unsafely(request.bintel)

class SocketEngine(duplex: Duplex)(using Monitor, Probate) extends Engine:
  private val nextId: Atomic.Int = Atomic(1)

  // The reply callbacks still awaited, by request id: an atomic cell holding an immutable map, so
  // a registration and the reader strand's removal cannot interleave.
  private val callbacks: Atomic.Ref[Map[Int, Repl.Reply -> Unit]] = Atomic.Ref(Map())

  // The one writer: `Duplex.send` leaves serialization to its caller.
  private val writes: Mutex = Mutex()

  @volatile private var sink: Repl.Reply -> Unit = _ => ()
  @volatile private var live: Boolean = true

  private def transmit(request: Repl.Request): Unit = writes:
    if live then
      duplex.send(zephyrine.Stream(LengthPrefix.encode(SocketEngine.encode(request))))

  def request(request: Int => Repl.Request)(reply: Repl.Reply => Unit): Unit =
    val id = nextId.ere(_ + 1)
    callbacks.revise(_.define(id, caps.unsafe.unsafeAssumePure(reply)))
    transmit(request(id))

  def send(request: Int => Repl.Request): Unit = transmit(request(nextId.ere(_ + 1)))
  def pushed(sink: Repl.Reply => Unit): Unit = this.sink = caps.unsafe.unsafeAssumePure(sink)
  def close(): Unit = live = false

  // Reads replies until the server closes the stream (or `close`), then runs `ended`. The
  // reader is a task rather than the caller's thread because `duplex.source` blocks on its
  // first read, and the server sends nothing before it is asked.
  def start(ended: () -> Unit): Unit =
    async:
      import strategies.throwUnsafely

      // One record per reply, framed by its four-byte big-endian length; the iterator ends when
      // the server closes the stream.
      val frames = duplex.source.chunks.frames[LengthPrefix]

      while live && frames.hasNext do
        safely(Bintel.read[Repl.Reply](frames.next())).let: reply =>
          val id = Repl.replyId(reply)

          reply match
            case Repl.Reply.Output(_, _, _) => sink(reply)
            case _ =>
              // Claimed atomically: the callback is removed and read in one transition, so a
              // reply can be delivered only once.
              val callback: Optional[Repl.Reply -> Unit] = callbacks().at(id)
              callbacks.revise(_.omit(id))
              callback.lay(sink(reply))(_(reply))

      live = false
      ended()
