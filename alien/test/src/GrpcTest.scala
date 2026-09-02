// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.alien


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import io.grpc.Status

import kse.basics.{given, _}
import kse.flow.{given, _}
import kse.alien.Pb


/** The envelope for the session-stream (correlator) test: an id to correlate by, a kind to
  * dispatch on, and one number of cargo.  kinds: 1 = work request (client asks server),
  * 2 = factor request (server asks client, mid-work), 3 = factor reply, 4 = work reply.
  */
final case class Note(id: String = "", kind: Int = 0, num: Long = 0L) extends Pb.Writable {
  def writeTo(o: Pb.Out): Unit =
    o.string(1, id)
    o.int32(2, kind)
    o.int64(3, num)
}
object Note extends Pb.Companion[Note] {
  val default: Note = Note()
  def readFrom(in: Pb.In, prior: Note): Note = Pb.context("Note"):
    var id = prior.id
    var kind = prior.kind
    var num = prior.num
    while in.next() do in.field match
      case 1 => id = in.string()
      case 2 => kind = in.int32()
      case 3 => num = in.int64()
      case _ => in.skip()
    Note(id, kind, num)
}


@RunWith(classOf[JUnit4])
class GrpcTest {
  import kse.basics.testutilities.TestUtilities.{given, _}
  import kse.basics.{given, _}
  import kse.flow.{given, _}
  import kse.alien.{Pb, Grpc}
  import kse.test.alien.track.{Pt, Track}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  def got[A](ask: Ask[A]): A = ask.fold(a => a)(e => throw new AssertionError("unexpected failure: " + e.toString))

  def codeOf[A](ask: Ask[A]): Status.Code = ask.fold(_ => Status.Code.OK)(e => Grpc.statusOf(e).getCode)

  val svcName = "kse.test.alien.TrackSvc"
  val centerM  = Grpc.unary(svcName, "Center", Track, Pt)
  val pointsM  = Grpc.serverStream(svcName, "Points", Track, Pt)
  val chanM    = Grpc.serverStream(svcName, "PointsChan", Track, Pt)
  val collectM = Grpc.clientStream(svcName, "Collect", Pt, Track)
  val shiftM   = Grpc.bidi(svcName, "Shift", Pt, Pt)

  val chatM = Grpc.bidi("kse.test.alien.ChatSvc", "Chat", Note, Note)

  /** All four call shapes over one service; a track with id "die" fails its point stream partway. */
  def trackService: Grpc.Service =
    Grpc.Service()
      .unary(centerM){ t =>
        if t.pts.length == 0 then Grpc.or(Status.Code.INVALID_ARGUMENT, s"no points in ${t.id}")
        else
          var sx = 0.0
          var sy = 0.0
          var i = 0
          while i < t.pts.length do
            sx += t.pts(i).x
            sy += t.pts(i).y
            i += 1
          Is(Pt(sx / t.pts.length, sy / t.pts.length))
      }
      .serverStream(pointsM){ (t, snd) =>
        if t.id == "die" then
          var i = 0
          while i < 2 && i < t.pts.length do
            snd.send(t.pts(i)) __ Unit
            i += 1
          Grpc.or(Status.Code.NOT_FOUND, "lost the rest")
        else
          var i = 0
          while i < t.pts.length do
            snd.send(t.pts(i)) __ Unit
            i += 1
          Is(())
      }
      .serverStream(chanM){ (t, snd) =>
        snd.from(kse.loom.Chan.Source(t.pts))
      }
      .clientStream(collectM){ inbox =>
        val buf = collection.mutable.ArrayBuffer.empty[Pt]
        inbox.each(p => buf.append(p) __ Unit).map(_ => Track(id = s"${buf.length} pts", pts = buf.toArray))
      }
      .bidi(shiftM){ (inbox, snd) =>
        inbox.each(p => snd.send(Pt(p.x + 1, p.y)) __ Unit)
      }

  @Test
  def grpcPiecesTest(): Unit =
    val t = Track(id = "bytes", pts = Array(Pt(1, 2)))

    T ~ got(Track.parse(new java.io.ByteArrayInputStream(t.toBytes))).id ==== "bytes"

    val m = Grpc.marshal(Track)
    val back = m.parse(m.stream(t))
    T ~ back.id ==== t.id
    T ~ back.pts.toList ==== t.pts.toList
    T ~ Grpc.rawBytes.parse(Grpc.rawBytes.stream(Array[Byte](1, 2, 3))) =**= Array[Byte](1, 2, 3)

    T ~ centerM.getFullMethodName ==== "kse.test.alien.TrackSvc/Center"
    T ~ trackService.definition.getServiceDescriptor.getName ==== svcName

    val e = Grpc.err(Status.Code.NOT_FOUND, "gone") +# "while testing"
    T ~ Grpc.statusOf(e).getCode ==== Status.Code.NOT_FOUND
    T ~ Grpc.statusOf(e).getDescription ==== "gone"
    T ~ Grpc.statusOf(Err("boring")).getCode ==== Status.Code.INTERNAL

  @Test
  def grpcRoundTripTest(): Unit =
    val (host, link) = got(Grpc.loopback(trackService.definition))
    try
      val t3 = Track(id = "tri", pts = Array(Pt(0, 0), Pt(3, 0), Pt(0, 3)))

      T ~ got(Grpc.call(link.channel, centerM, t3)) ==== Pt(1, 1)

      val bad = Grpc.call(link.channel, centerM, Track(id = "hollow"))
      T ~ bad.isAlt ==== true
      T ~ codeOf(bad) ==== Status.Code.INVALID_ARGUMENT
      T ~ bad.fold(_ => "")(e => Grpc.statusOf(e).getDescription) ==== "no points in hollow"

      val seen = collection.mutable.ArrayBuffer.empty[Pt]
      T ~ Grpc.stream(link.channel, pointsM, t3)(p => seen.append(p) __ Unit).isIs ==== true
      T ~ seen.toList ==== t3.pts.toList

      val partial = collection.mutable.ArrayBuffer.empty[Pt]
      val died = Grpc.stream(link.channel, pointsM, t3.copy(id = "die"))(p => partial.append(p) __ Unit)
      T ~ partial.toList ==== t3.pts.take(2).toList
      T ~ codeOf(died) ==== Status.Code.NOT_FOUND

      val up = got(Grpc.upload(link.channel, collectM){ snd =>
        var i = 0
        while i < 5 do
          snd.send(Pt(i, 2 * i)) __ Unit
          i += 1
        Is(())
      })
      T ~ up.id ==== "5 pts"
      T ~ up.pts.toList ==== List.tabulate(5)(i => Pt(i, 2 * i))

      val echoed = collection.mutable.ArrayBuffer.empty[Pt]
      val talk = Grpc.converse(link.channel, shiftM){ (snd, inbox) =>
        Or.Ret {
          var i = 0
          while i < 4 do
            snd.send(Pt(i, i)) __ Unit
            i += 1
          var n = 0
          while n < 4 do
            inbox.next().?.fold{ p =>
              echoed.append(p) __ Unit
              n += 1
            }{ _ =>
              Grpc.or[Unit](Status.Code.DATA_LOSS, "replies ended early").?
            }
        }
      }
      T ~ talk.isIs ==== true
      T ~ echoed.toList ==== List.tabulate(4)(i => Pt(i + 1, i))
    finally
      link.close()
      host.close()

  @Test
  def grpcLifecycleTest(): Unit =
    // gRPC used incidentally: open, call, close, and the JVM is exactly as it was -- twice over.
    val (h1, l1) = got(Grpc.loopback(trackService.definition))
    T ~ got(Grpc.call(l1.channel, centerM, Track(id = "a", pts = Array(Pt(2, 4))))) ==== Pt(2, 4)
    l1.close()
    h1.close()
    T ~ l1.channel.isTerminated ==== true
    T ~ h1.server.isTerminated ==== true

    // a call on a closed link refuses as a value, not a crash
    T ~ Grpc.call(l1.channel, centerM, Track(id = "a", pts = Array(Pt(1, 1)))).isAlt ==== true

    val (h2, l2) = got(Grpc.loopback(trackService.definition))
    T ~ got(Grpc.call(l2.channel, centerM, Track(id = "b", pts = Array(Pt(5, 6))))) ==== Pt(5, 6)
    l2.close()
    h2.close()
    T ~ l2.channel.isTerminated ==== true
    T ~ h2.server.isTerminated ==== true

  @Test
  def grpcChanPumpTest(): Unit =
    import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
    import kse.loom.Chan

    val (host, link) = got(Grpc.loopback(trackService.definition))
    try
      val t3 = Track(id = "tri", pts = Array(Pt(1, 2), Pt(3, 4), Pt(5, 6)))

      // server streams from a channel end (a finite Source here)
      val seen = collection.mutable.ArrayBuffer.empty[Pt]
      T ~ Grpc.stream(link.channel, chanM, t3)(p => seen.append(p) __ Unit).isIs ==== true
      T ~ seen.toList ==== t3.pts.toList

      // client pours replies into a Chan a separate consumer drains; capacity 2 exercises blocking
      val ch = Chan[Pt](2)
      val answer = new ArrayBlockingQueue[AnyRef](1)
      Thread.ofVirtual().start{ () =>
        val buf = collection.mutable.ArrayBuffer.empty[Pt]
        var going = true
        while going do
          ch.recv().fold(p => buf.append(p) __ Unit)(_ => going = false)
        answer.put(buf.toList)
      } __ Unit
      val talk = Grpc.converse(link.channel, shiftM){ (snd, inbox) =>
        var i = 1
        while i <= 3 do
          snd.send(Pt(i, 9)) __ Unit
          i += 1
        snd.complete()
        inbox.into(ch)
      }
      T ~ talk.isIs ==== true
      T ~ answer.poll(5, TimeUnit.SECONDS).asInstanceOf[List[Pt]] ==== List(Pt(2, 9), Pt(3, 9), Pt(4, 9))
    finally
      link.close()
      host.close()

  /** The inverted-authority session: the client orchestrates, but the server must ask the
    * client something before it can answer -- the exchange the transport cannot express as a
    * method call, carried by a [[Grpc.Correlator]] on each side of one bidi stream.
    */
  @Test
  def grpcCorrelatorTest(): Unit =
    import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}

    val chatSvc =
      Grpc.Service()
        .bidi(chatM){ (inbox, snd) =>
          val corr = new Grpc.Correlator[Note, Note](snd, prefix = "s")
          val r = inbox.each{ n =>
            if corr.deliver(n.id, n) then ()
            else if n.kind == 1 then
              // work takes a factor only the client knows: ask, then answer the work
              Thread.ofVirtual().start{ () =>
                val f = corr.request(id => Note(id, 2, n.num)).fold(_.num)(_ => -1000L)
                snd.send(Note(n.id, 4, n.num * f)) __ Unit
              } __ Unit
          }
          corr.fail(Err("session over"))
          r
        }

    val (host, link) = got(Grpc.loopback(chatSvc.definition))
    try
      val answers = new ArrayBlockingQueue[AnyRef](3)
      val talk = Grpc.converse(link.channel, chatM){ (snd, inbox) =>
        val corr = new Grpc.Correlator[Note, Note](snd, prefix = "c")
        var n = 1
        while n <= 3 do
          val k = n
          Thread.ofVirtual().start{ () =>
            answers.put(corr.request(id => Note(id, 1, k)).fold(_.num)(_ => -1L).asInstanceOf[AnyRef])
          } __ Unit
          n += 1
        var replies = 0
        val r = Or.Ret {
          while replies < 3 do
            inbox.next().?.fold{ m =>
              if corr.deliver(m.id, m) then replies += 1
              else if m.kind == 2 then snd.send(Note(m.id, 3, 10L)) __ Unit
            }{ _ =>
              Grpc.or[Unit](Status.Code.DATA_LOSS, "server ended early").?
            }
        }
        corr.fail(Err("done"))
        r
      }
      T ~ talk.isIs ==== true
      val out = List.fill(3)(answers.poll(5, TimeUnit.SECONDS).asInstanceOf[Long]).sorted
      T ~ out ==== List(10L, 20L, 30L)
    finally
      link.close()
      host.close()
}
