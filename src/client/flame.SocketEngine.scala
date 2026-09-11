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

import java.lang as jl
import java.util.concurrent as juc

import scala.caps
import scala.collection.concurrent.TrieMap
import scala.collection.mutable as scm

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

  // Prefixes BinTEL body bytes with a 4-byte big-endian length, the on-wire frame the server
  // reads with `DataInputStream.readInt` + `readFully`.
  def framed(data: Data): Data =
    val length: Int = data.length
    val bytes: scala.Array[Byte] = new scala.Array[Byte](4 + length)
    bytes(0) = (length >>> 24).toByte
    bytes(1) = (length >>> 16).toByte
    bytes(2) = (length >>> 8).toByte
    bytes(3) = length.toByte
    jl.System.arraycopy(Array.unsafeJvm(data), 0, bytes, 4, length)
    Array.unsafeFrozen(bytes)

  // Reassembles length-prefixed frames from the chunk-at-a-time socket stream, keeping a buffer
  // across calls so a frame split over chunks, or several frames in one chunk, is handled.
  // `next()` yields one frame's body, or `Unset` when the stream ends.
  class FrameReader(chunks: Iterator[Data]^):
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
          val body: Data = Array.from(buffer.slice(4, 4 + length))
          buffer.remove(0, 4 + length)
          body

class SocketEngine(duplex: Duplex)(using Monitor, Probate) extends Engine:
  private val nextId: juc.atomic.AtomicInteger = juc.atomic.AtomicInteger(1)
  private val callbacks: TrieMap[Int, Repl.Reply -> Unit] = TrieMap()

  @volatile private var sink: Repl.Reply -> Unit = _ => ()
  @volatile private var live: Boolean = true

  private def transmit(request: Repl.Request): Unit = synchronized:
    if live then duplex.send(zephyrine.Stream(SocketEngine.framed(SocketEngine.encode(request))))

  def request(request: Int => Repl.Request)(reply: Repl.Reply => Unit): Unit =
    val id = nextId.getAndIncrement
    callbacks(id) = caps.unsafe.unsafeAssumePure(reply)
    transmit(request(id))

  def send(request: Int => Repl.Request): Unit = transmit(request(nextId.getAndIncrement))
  def pushed(sink: Repl.Reply => Unit): Unit = this.sink = caps.unsafe.unsafeAssumePure(sink)
  def close(): Unit = live = false

  // Reads replies until the server closes the stream (or `close`), then runs `ended`. The
  // reader is a task rather than the caller's thread because `duplex.source` blocks on its
  // first read, and the server sends nothing before it is asked.
  def start(ended: () -> Unit): Unit =
    async:
      val frames = SocketEngine.FrameReader(duplex.source.chunks)

      while live do
        frames.next().lay({ live = false }): bytes =>
          safely(Bintel.read[Repl.Reply](bytes)).let: reply =>
            val id = Repl.replyId(reply)

            reply match
              case Repl.Reply.Output(_, _, _) => sink(reply)
              case _ =>
                callbacks.remove(id) match
                  case Some(callback) => callback(reply)
                  case None           => sink(reply)

      ended()
