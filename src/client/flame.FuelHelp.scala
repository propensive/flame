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

import environments.javaBaseEnvironment
import probates.cancelProbate
import pyrocosm.{Advert, Bus, Channel, Machine, Peer, Swarm}

// Fuel's documentation of the project, held here for completions: a copy of the index Fuel
// serves — each documented entity's key (`gossamer.Text#length`) and its one-line `info` — so
// that attaching a line to a candidate is a map lookup, and never waits on another process.
//
// The hub tells Flame where Fuel is: a `fuel` node on this machine advertising a `help`
// service. The index itself comes over a direct peer connection to that service, as fume
// reaches its workers, not through the hub's bus (which floods every node and caps an event
// at 16 KiB). Fuel announces a new model on the bus as an `api` event, which is the cue to
// fetch again. Every fetch runs on its own task; a failure leaves the last index in place.
//
// `Message` mirrors Fuel's `Help.Message` field for field, under the same schema name, so
// the two derive the same fingerprint, which is the protocol the handshake names.
object FuelHelp:
  enum Message:
    case Index(edition: Long, entries: List[Entry])

  case class Entry(key: Text, info: Text)

  private val tool: Text = t"fuel"
  private val service: Text = t"help"
  private val protocol: Text = t"fuel-help"
  private val event: Text = t"api"
  private val port: Int = 8094

  private lazy val codec: Channel.Codec[Message] =
    import Channel.derivation.throwing
    val schema: Tels = Tels.tels[Message](protocol)

    Channel.Codec
      ( protocol, schema, message => Channel.encode(message, schema),
        data => Channel.decode[Message](data) )

  @caps.unsafe.untrackedCaptures
  @volatile
  private var index: Map[Text, Text] = Map()

  @caps.unsafe.untrackedCaptures
  @volatile
  private var edition: Optional[Long] = Unset

  @caps.unsafe.untrackedCaptures
  private val started: Atomic[Boolean] = Atomic(false)

  // The info line for a qualified name, if Fuel has one; `Unset` is the usual answer.
  def lookup(key: Text): Optional[Text] = index.at(key)

  // Holds the index: a copy is fetched now, and again whenever Fuel announces a new model.
  // Called where a REPL interface is made; later calls do nothing.
  def start()(using Monitor): Unit =
    if !started.swap(true) then
      Swarm.of(Flame.name).let: swarm =>
        swarm.subscribe(Swarm.Filter(tools = Set(tool), kinds = Set(event), remote = false)):
          (_, envelope) => envelope.event match
            case Bus.Event.Tool(_, _, payload) =>
              val announced: Optional[Long] = safely(payload.utf8.as[Long])
              if announced != edition then async(fetch())
            case _ => ()

        async(fetch())

  // Fuel's `help` service on this machine, as its node advertises it: the same identity as
  // this node's (one machine, one certificate), and the machine's own token, which both
  // tools read from the same file.
  private def peer(): Optional[Machine] =
    Swarm.of(Flame.name).let: swarm =>
      val self: Text = swarm.node.identity

      val advert: Optional[Advert] =
        swarm.nodes().seek: advert =>
          advert.node.tool == tool && advert.node.identity == self &&
            advert.services.exists(_.name == service)

      advert.let: advert =>
        Machine
          ( advert.node.machine,
            List(t"127.0.0.1"),
            advert.services.seek(_.name == service).let(_.port),
            Peer.parseFingerprint(advert.node.identity),
            Peer.token,
            Nil )

  // Fetches the index once, if Fuel is there: the whole index comes as one message.
  def fetch(): Unit =
    peer().let: machine =>
      val fetched: Optional[Message.Index] =
        Peer.exchange[Message, Optional[Message.Index]](machine, tool, Flame.version, codec, port):
          session =>
            try
              session.receive() match
                case Channel.Frame.Message(index: Message.Index) => index
                case _                                           => Unset
            finally session.close()
        . match
            case scala.Right(index) => index
            case scala.Left(_)      => Unset

      fetched.let: fetched =>
        index = Map(fetched.entries.map { entry => entry.key -> entry.info }*)
        edition = fetched.edition
