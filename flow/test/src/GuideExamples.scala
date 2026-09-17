// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.flow

// Every example in GUIDE-flow.md lives here between a `// guide: name` and `// guide: end` pair,
// so that the guide's code is compiled and run.  `check-guides.py` verifies the two stay identical.

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics.{given, *}
import kse.flow.{given, *}

object GuideExamples {

  // guide: or.plain
  def parsePlain(s: String): Either[String, Int] =
    if s.nonEmpty && s.forall(_.isDigit) then Right(s.toInt) else Left(s"not a number: $s")

  def scalePlain(s: String, record: Int => Unit, log: String => Unit): (Int, Boolean) =
    val x = parsePlain(s).map(_ * 100)
    val v = x.getOrElse(-1)
    val ok = x.exists(_ > 500)
    x match
      case Right(n) => record(n)
      case Left(e)  => log(e)
    (v, ok)
  // guide: end

  // guide: or.kse3
  def parse(s: String): Int Or String =
    if s.nonEmpty && s.forall(_.isDigit) then Is(s.toInt) else Alt(s"not a number: $s")

  def scale(s: String, record: Int => Unit, log: String => Unit): (Int, Boolean) =
    val x = parse(s).map(_ * 100)
    val v = x.getOrElse(_ => -1)
    val ok = x.exists(_ > 500)
    x.fold(record)(log)
    (v, ok)
  // guide: end


  // guide: ask.plain
  def portPlain(s: String): Either[String, Int] =
    try
      val n = s.toInt
      if n < 1 || n > 65535 then Left(s"port out of range: $n") else Right(n)
    catch
      case e: NumberFormatException => Left(s"bad port: ${e.getMessage}")

  def configPortPlain(s: String): Either[String, Int] =
    portPlain(s).left.map(e => s"while reading the server config: $e")
  // guide: end

  // guide: ask.kse3
  def port(s: String): Ask[Int] = Ask:
    val n = s.toInt                                        // an exception becomes the Err
    if n < 1 || n > 65535 then Err ?# s"port out of range: $n"
    n

  def configPort(s: String): Ask[Int] =
    port(s).mapAlt(_ +# "while reading the server config")
  // guide: end


  // guide: early.plain
  def sumPlain(a: String, b: String): Either[String, Int] =
    for
      x <- parsePlain(a)
      y <- parsePlain(b)
    yield x + y

  def totalPlain(items: Array[String], bonus: Option[Int]): Either[String, Int] =
    var t = 0
    var i = 0
    while i < items.length do
      parsePlain(items(i)) match
        case Right(n) => t += n
        case Left(e)  => return Left(e)
      i += 1
    bonus match
      case Some(b) => Right(t + b)
      case None    => Left("bonus is required")
  // guide: end

  // guide: early.kse3
  def sum(a: String, b: String): Int Or String = Or.Ret:
    parse(a).? + parse(b).?

  def total(items: Array[String], bonus: Option[Int]): Ask[Int] = Ask:
    var t = 0
    items.use(): s =>
      t += parse(s).?                                      // a String error leaves as an Err
    t + bonus.?#("bonus is required")
  // guide: end


  def catchingPlain(s: String, risky: () => Int): (Int, Either[Throwable, Int]) =
    // guide: catching.plain
    val n = try s.toInt catch { case _: NumberFormatException => -1 }
    val r = try Right(risky()) catch { case e: Exception => Left(e) }
    // guide: end
    (n, r)

  def catchingKse3(s: String, risky: () => Int): (Int, Int Or Throwable, Ask[Int]) =
    // guide: catching.kse3
    val n = ratchet(-1)(_ => s.toInt)
    val r = safe{ risky() }                                // Int Or Throwable
    val a = nice{ risky() }                                // Ask[Int]: the exception wrapped in an Err
    // guide: end
    (n, r, a)


  def fallbackPlain(s: String): Int =
    // guide: fallback.plain
    try s.toInt
    catch case _: NumberFormatException =>
      try s.toDouble.toInt
      catch case _: NumberFormatException => 0
    // guide: end

  def fallbackKse3(s: String): Int =
    // guide: fallback.kse3
    attempt.safe:
      s.toInt
    .safe:
      s.toDouble.toInt
    .default:
      0
    // guide: end

  def firstGood(a: String, b: String): Int =
    // guide: bang.kse3
    attempt:
      parse(a).! * 2                                       // .! fails this attempt if parse did
    .attempt:
      parse(b).!
    .default:
      -1
    // guide: end

  final class Handle(val name: String) extends AutoCloseable {
    var closed = false
    def read(): Int = if closed then throw new IllegalStateException(s"$name is closed") else name.length
    def close(): Unit = closed = true
  }
  object Handle {
    val opened = collection.mutable.ArrayBuffer.empty[Handle]
    def open(name: String): Handle =
      if name.isEmpty then throw new java.io.IOException("no name")
      val h = new Handle(name)
      opened += h
      h
  }
  final class Server(val h: Handle, val scratch: Int)

  def readPlain(name: String): Int =
    // guide: resource.plain
    val h = Handle.open(name)
    try h.read()
    finally h.close()
    // guide: end

  def readKse3(name: String): Int =
    // guide: resource.kse3
    Resource(Handle.open(name))(_.close())(h => h.read())
    // guide: end

  def readNice(name: String): Ask[Int] =
    // guide: resourcenice.kse3
    Resource.nice(nice{ Handle.open(name) })(Tidy.closes)(h => h.read())
    // guide: end

  def deferred(log: String => Unit): Int =
    // guide: defer.kse3
    defer(log("done")):
      log("working")
      42
    // guide: end


  def owners(): (Int, Int, Boolean, Boolean) =
    // guide: later.kse3
    val later = Resource.closedLater(Handle.open("cfg"))(Tidy.closes)   // sole owner: closes at close(), shutdown, or GC
    val n = later.op(_.read())
    later.close()
    val lease = Resource.leased(Handle.open("db"))(Tidy.closes)         // shared owner: borrowers use op, use, nice
    val m = lease.op(_.read())
    lease.close()                                                        // cleanup runs once the last borrow returns
    // guide: end
    (n, m, later.isOpen, lease.isOpen)


  def openPairPlain(a: String, b: String): (Handle, Handle) =
    // guide: assemble.plain
    val x = Handle.open(a)
    val y =
      try Handle.open(b)
      catch
        case e: Throwable =>
          x.close()
          throw e
    (x, y)
    // guide: end

  def openPair(a: String, b: String): (Handle, Handle) =
    // guide: assemble.kse3
    Resource.assemble:
      val x = Resource.guard(Handle.open(a))(_.close())
      val y = Resource.guard(Handle.open(b))(_.close())
      Resource.release(x, y)
    // guide: end

  def openServer(a: String): Server =
    // guide: assembleop.kse3
    Resource.assemble:
      val tmp = Resource.temp(Handle.open("scratch"))(_.close())     // gone when the block exits, whichever way
      val h = Resource.guard(Handle.open(a))(_.close())              // torn down only if we fail from here on
      val k = tmp.read()
      Resource.releaseOp(h)(new Server(_, k))                        // handed out in h's place, once built
    // guide: end


  def caches(expensive: () => Int, load: String => String): (Int, Int, String, String) =
    // guide: cached.kse3
    val once = Lazy(expensive())              // computed on the first .value, then kept
    val w = Worm.of[String]                   // write once, from any thread
    w.set("eel")
    val cfg = Soft("fish.cfg")(load)          // dropped under memory pressure, recomputed from its source
    // guide: end
    (once.value, once.value, w.get, cfg.value)

  def holds(): (String, String, String) =
    // guide: hold.kse3
    val capitals = Hold.mutable(false)
    val name = Hold.mutable("Salmon")
    val fish = name.mapWith(capitals)((s, c) => if c then s.toUpperCase else s.toLowerCase)
    val a = fish.value                        // "salmon"
    capitals.set(true)
    val b = fish.value                        // "SALMON": an input changed, so it was recomputed
    // guide: end
    (a, b, fish.value)


  def loopsPlain(list: List[String], seen: Int => Unit, f: (String, Int) => Unit): Unit =
    // guide: loops.plain
    var x = 1
    while x < 100 do
      seen(x)
      x *= 2
    var i = 0
    for s <- list do
      f(s, i)
      i += 1
    // guide: end

  def loopsKse3(list: List[String], seen: Int => Unit, f: (String, Int) => Unit): Unit =
    // guide: loops.kse3
    cFor(1)(_ < 100)(_ * 2)(seen)
    iFor(list.iterator)(f)
    // guide: end

  def untilDone(next: () => String, process: String => Unit): Unit =
    // guide: loopbreak.kse3
    loop:
      val line = next()
      loop.stop_?(line == null)
      process(line)
    // guide: end


  def manyPlain(inputs: List[String]): (Either[String, List[Int]], List[String]) =
    // guide: many.plain
    val results = inputs.map(portPlain)
    val all = results.foldRight(Right(Nil): Either[String, List[Int]]): (r, acc) =>
      for x <- r; xs <- acc yield x :: xs
    val bad = results.collect{ case Left(e) => e }
    // guide: end
    (all, bad)

  def many(inputs: List[String]): (Ask[List[Int]], List[Err], Ask[List[Int]]) =
    // guide: many.kse3
    val results = inputs.map(port)
    val all = results.valid                   // every value, or the first error
    val bad = results.errors
    val direct = inputs.validMap(port)        // the same as all, without the intermediate list
    // guide: end
    (all, bad, direct)
}



@RunWith(classOf[JUnit4])
class GuideTest {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import GuideExamples as G

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  @Test
  def orAndAskTest(): Unit =
    for s <- List("7", "77", "eel", "") do
      val (r0, l0) = (collection.mutable.ArrayBuffer.empty[Int], collection.mutable.ArrayBuffer.empty[String])
      val (r1, l1) = (collection.mutable.ArrayBuffer.empty[Int], collection.mutable.ArrayBuffer.empty[String])
      T(s) ~ G.scalePlain(s, r0 += _, l0 += _) ==== G.scale(s, r1 += _, l1 += _)
      T(s) ~ r0 =**= r1
      T(s) ~ l0 =**= l1
    T ~ G.scale("77", _ => (), _ => ()) ==== (7700, true)
    for s <- List("80", "70000", "eel") do
      T(s) ~ G.configPortPlain(s).isRight ==== G.configPort(s).isIs
    T ~ G.configPort("80")    ==== 80
    T ~ G.configPort("70000").fold(_ => "")(_.toString).linesIterator.next() ==== "while reading the server config"
    T ~ G.configPort("70000").fold(_ => "")(_.toString).contains("port out of range: 70000") ==== true
    T ~ G.configPort("eel").fold(_ => "")(_.toString).contains("NumberFormatException") ==== true

  @Test
  def earlyReturnTest(): Unit =
    T ~ G.sum("2", "3")   ==== 5
    T ~ G.sum("2", "cod") ==== Alt("not a number: cod")
    T ~ G.sumPlain("2", "cod") ==== Left("not a number: cod")
    val items = Array("1", "2", "3")
    T ~ G.total(items, Some(10))      ==== 16
    T ~ G.totalPlain(items, Some(10)) ==== Right(16)
    T ~ G.total(items, None).fold(_ => "")(_.toString)      ==== "bonus is required"
    T ~ G.totalPlain(items, None)                          ==== Left("bonus is required")
    T ~ G.total(Array("1", "x"), Some(1)).fold(_ => "")(_.toString) ==== "not a number: x"

  @Test
  def catchingAndFallbackTest(): Unit =
    val boom = () => throw new IllegalStateException("boom")
    val fine = () => 42
    T ~ G.catchingPlain("12", fine)._1 ==== G.catchingKse3("12", fine)._1
    T ~ G.catchingPlain("no", fine)._1 ==== G.catchingKse3("no", fine)._1
    T ~ G.catchingKse3("no", fine)._1  ==== -1
    T ~ G.catchingKse3("1", fine)._2   ==== 42
    T ~ G.catchingKse3("1", boom)._2.fold(_ => "")(_.getMessage) ==== "boom"
    T ~ G.catchingKse3("1", boom)._3.isAlt ==== true
    T ~ G.catchingPlain("1", boom)._2.isLeft ==== true
    for s <- List("12", "12.7", "eel") do
      T(s) ~ G.fallbackPlain(s) ==== G.fallbackKse3(s)
    T ~ G.fallbackKse3("12.7") ==== 12
    T ~ G.firstGood("4", "5")     ==== 8
    T ~ G.firstGood("eel", "5")   ==== 5
    T ~ G.firstGood("eel", "cod") ==== -1

  @Test
  def resourcesTest(): Unit =
    G.Handle.opened.clear()
    T ~ G.readPlain("eel") ==== 3
    T ~ G.readKse3("eel")  ==== 3
    T ~ G.readNice("eel")  ==== 3
    T ~ G.readNice("").isAlt ==== true
    T ~ G.Handle.opened.forall(_.closed) ==== true
    val log = collection.mutable.ArrayBuffer.empty[String]
    T ~ G.deferred(log += _) ==== 42
    T ~ log =**= Array("working", "done")
    T ~ G.owners() ==== (3, 2, false, false)
    G.Handle.opened.clear()
    val (x, y) = G.openPair("a", "bb")
    T ~ (x.closed, y.closed) ==== (false, false)
    T ~ G.openPair("a", "") ==== thrown[java.io.IOException]
    T ~ G.openPairPlain("a", "") ==== thrown[java.io.IOException]
    T ~ G.Handle.opened.count(_.closed) ==== 2
    G.Handle.opened.clear()
    val srv = G.openServer("eel")
    T ~ (srv.h.closed, srv.scratch) ==== (false, 7)
    T ~ G.Handle.opened.count(_.closed) ==== 1
    G.Handle.opened.clear()
    T ~ G.openServer("") ==== thrown[java.io.IOException]
    T ~ G.Handle.opened.map(_.name) =**= Array("scratch")
    T ~ G.Handle.opened.forall(_.closed) ==== true

  @Test
  def cachesAndLoopsTest(): Unit =
    var calls = 0
    val (a, b, w, c) = G.caches(() => { calls += 1; 5 }, s => s.toUpperCase)
    T ~ (a, b, calls, w, c) ==== (5, 5, 1, "eel", "FISH.CFG")
    T ~ G.holds() ==== ("salmon", "SALMON", "SALMON")
    val (s0, s1) = (collection.mutable.ArrayBuffer.empty[Int], collection.mutable.ArrayBuffer.empty[Int])
    val (f0, f1) = (collection.mutable.ArrayBuffer.empty[String], collection.mutable.ArrayBuffer.empty[String])
    G.loopsPlain(List("cod", "eel"), s0 += _, (s, i) => f0 += s"$i:$s")
    G.loopsKse3(List("cod", "eel"), s1 += _, (s, i) => f1 += s"$i:$s")
    T ~ s0 =**= s1
    T ~ s1 =**= Array(1, 2, 4, 8, 16, 32, 64)
    T ~ f0 =**= f1
    T ~ f1 =**= Array("0:cod", "1:eel")
    val lines = Iterator("a", "b", null, "c")
    val got = collection.mutable.ArrayBuffer.empty[String]
    G.untilDone(() => lines.next(), got += _)
    T ~ got =**= Array("a", "b")
    val (all0, bad0) = G.manyPlain(List("80", "443"))
    val (all1, bad1, direct1) = G.many(List("80", "443"))
    T ~ all0 ==== Right(List(80, 443))
    T ~ all1 ==== List(80, 443)
    T ~ direct1 ==== all1
    T ~ bad0.isEmpty ==== bad1.isEmpty
    val (_, bad2, direct2) = G.many(List("80", "eel", "70000"))
    T ~ bad2.length ==== 2
    T ~ direct2.isAlt ==== true
}
