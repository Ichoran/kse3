// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.basics

// Every example in GUIDE-basics.md lives here between a `// guide: name` and `// guide: end` pair,
// so that the guide's code is compiled and run.  `check-guides.py` verifies the two stay identical.

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics.{given, *}
import kse.basics.intervals.*
import kse.basics.labels.*

object GuideExamples {

  def gluePlain(set: java.util.HashSet[Int], record: Int => Unit, x: Int): Int =
    // guide: glue.plain
    val _ = set.add(x)
    val y = x * 2 - 7
    record(y)
    if y < 0 then -y else y
    // guide: end

  def glueKse3(set: java.util.HashSet[Int], record: Int => Unit, x: Int): Int =
    // guide: glue.kse3
    set.add(x) __ Unit
    (x * 2 - 7).tap(record).fixIf(_ < 0)(- _)
    // guide: end


  def arraysPlain(xs: Array[Int]): (Array[Int], Array[Int]) =
    // guide: arrays.plain
    val inner = xs.slice(1, xs.length - 1)
    val negs = xs.zipWithIndex.collect{ case (x, i) if x < 0 => i }
    var i = 2
    while i < xs.length - 2 do
      xs(i) += 1
      i += 1
    // guide: end
    (inner, negs)

  def arraysKse3(xs: Array[Int]): (Array[Int], Array[Int]) =
    // guide: arrays.kse3
    val inner = xs.select(1 to End-1)
    val negs = xs.where(_ < 0)
    xs.alter(2 to End-2)(_ + 1)
    // guide: end
    (inner, negs)


  def copyingPlain(xs: Array[Int]): (Array[Int], Array[Double], Array[Int], Long) =
    // guide: copying.plain
    val ys = xs.clone
    val halved = xs.map(_ / 2.0)
    val padded = xs ++ Array.fill(2)(-1)
    var sum = 0L
    var i = 0
    while i < xs.length do
      sum += xs(i)
      i += 1
    // guide: end
    (ys, halved, padded, sum)

  def copyingKse3(xs: Array[Int]): (Array[Int], Array[Double], Array[Int], Long) =
    // guide: copying.kse3
    val ys = xs.dup()
    val halved = xs.copyWith(_ / 2.0)
    val padded = xs.addRight(2, -1)
    val sum = xs.gather(0L)()((acc, x, _) => acc + x)
    // guide: end
    (ys, halved, padded, sum)

  def assigning(xs: Array[Int], ys: Array[Int]): Unit =
    // guide: assigning.kse3
    xs() = 0                      // every element
    xs(2 to End) = ys             // ys copied in, starting at index 2
    xs(_ == 0) = -1               // every element that is still zero
    xs.set(0 to 1)(i => i * 10)   // computed from the index
    // guide: end

  def chopping(): (Array[Array[Char]], Array[Array[Int]]) =
    // guide: chopping.kse3
    val words = "eel,cod,gar".arr.diced(_ == ',')
    val runs = Array(1, 2, 0, 3, 0, 4).diced(_ == 0)
    // guide: end
    (words, runs)


  def modesPlain(xs: Array[Int], i: Int): (Int, Int) =
    // guide: modes.plain
    val v = if i >= 0 && i < xs.length then xs(i) else -1
    var sum = 0
    var j = 0
    while j < xs.length && xs(j) >= 0 do
      sum += xs(j)
      j += 1
    // guide: end
    (v, sum)

  def modesKse3(xs: Array[Int], i: Int): (Int, Int) =
    // guide: modes.kse3
    val v = xs.clip(i)(-1)
    var sum = 0
    xs.flex.use(): x =>
      shortcut.quit_?(x < 0)
      sum += x
    // guide: end
    (v, sum)


  def intervals(xs: Array[Int]): (Iv, Iv, Iv, Iv, Iv) =
    // guide: intervals.kse3
    val iv = Iv(2, 5)                    // indices 2, 3, 4: i0 is included, iN is not
    val inner = (1 to End-1).of(xs)      // resolved against xs.length into an Iv
    val same = (1 ꓺ -1).of(xs)           // Python-style: the same interval
    val typed = (1 `..` -1).of(xs)       // the same again, from an ordinary keyboard
    val both = Iv(0, 4) & Iv(2, 8)       // Iv(2, 4); | is union
    // guide: end
    (iv, inner, same, typed, both)

  // guide: dispatch.plain
  def middlePlain(s: String, i0: Int, iN: Int): String = s.substring(i0, iN)
  // guide: end

  // guide: dispatch.kse3
  inline def middle[R <: Iv.X | Range](s: String, inline r: R): String =
    Iv.dispatch(r, s)((i0, iN) => s.substring(i0, iN))
  // guide: end


  def countingPlain(n: Int, record: Int => Unit): (Array[Int], Array[Int]) =
    // guide: counting.plain
    var i = 0
    while i < n do
      record(i)
      i += 1
    val squares = Array.tabulate(n)(j => j * j)
    val countdown = (n - 1 to 0 by -1).toArray
    // guide: end
    (squares, countdown)

  def countingKse3(n: Int, record: Int => Unit): (Array[Int], Array[Int]) =
    // guide: counting.kse3
    n.visit(record)
    val squares = n.unfold(j => j * j)
    val countdown = n.whereBy(-1)
    // guide: end
    (squares, countdown)


  def cellsPlain(xs: Array[Int]): (Int, Long) =
    // guide: cells.plain
    var n = 0
    xs.foreach(x => if x > 0 then n += 1)
    val total = new java.util.concurrent.atomic.AtomicLong(0)
    xs.foreach(x => total.addAndGet(x): Unit)
    // guide: end
    (n, total.get)

  def cellsKse3(xs: Array[Int]): (Int, Long) =
    // guide: cells.kse3
    val n = Mu(0)
    xs.foreach(x => if x > 0 then n.++)
    val total = Atom(0L)
    xs.foreach(x => total += x)
    // guide: end
    (n(), total())

  def onceAndCount(initialize: () => Unit): Long =
    // guide: once.kse3
    val hits = Atom.Count()                 // a LongAdder: many threads can bump it cheaply
    val ready = Atom.Toggle()
    4.times:
      hits.++
      if ready.turnOn() then initialize()   // only the first caller sees true
    // guide: end
    hits()


  def linesPlain(lines: Array[String], process: String => Unit): Unit =
    // guide: shortcut.plain
    var stop = false
    var i = 0
    while i < lines.length && !stop do
      val line = lines(i).trim
      if line == "END" then stop = true
      else if line.nonEmpty && !line.startsWith("#") then process(line)
      i += 1
    // guide: end

  def linesKse3(lines: Array[String], process: String => Unit): Unit =
    // guide: shortcut.kse3
    shortcut.quittable:
      var i = 0
      while i < lines.length do
        shortcut.skippable:
          val line = lines(i).trim
          shortcut.quit_?(line == "END")
          shortcut.skip_?(line.isEmpty || line.startsWith("#"))
          process(line)
        i += 1
    // guide: end


  def stringsPlain(n: Int, dirs: Array[String], name: String, xs: Array[Int]): (String, String) =
    // guide: strings.plain
    val msg = s"Found $n file${if n == 1 then "" else "s"} in ${dirs.mkString("[", ", ", "]")}"
    val sb = new java.lang.StringBuilder
    sb.append(name).append(": ")
    for x <- xs do sb.append(x).append(' ')
    val line = sb.toString.trim
    // guide: end
    (msg, line)

  def stringsKse3(n: Int, dirs: Array[String], name: String, xs: Array[Int]): (String, String) =
    // guide: strings.kse3
    val msg = say"Found $n# file//s# in $dirs"
    val line = MkStr: m =>
      m += name
      m += ": "
      xs.visit(): (x, i) =>
        if i > 0 then m += ' '
        m += x
    // guide: end
    (msg, line)

  def margins(): (String, String) =
    // guide: margins.kse3
    val poem = """
      |
      Salmon swim upstream
        to spawn
      """.demargin()
    val same = "  Salmon swim upstream\n    to spawn".dedent()
    // guide: end
    (poem, same)


  def sortingPlain(ages: Array[Int], names: Array[String]): (Array[Int], Array[String]) =
    // guide: sorting.plain
    val order = ages.indices.sortBy(i => ages(i)).toArray
    val byAge = order.map(i => names(i))
    java.util.Arrays.sort(ages)
    // guide: end
    (order, byAge)

  def sortingKse3(ages: Array[Int], names: Array[String]): (Array[Int], Array[String]) =
    // guide: sorting.kse3
    val order = ages.indicesInOrder()
    names.reorder(order)
    ages.sortInOrder()
    // guide: end
    (order, names)

  // guide: order.kse3
  object Descending extends Sorting.Total[Int] {
    inline def leq(a: Int, b: Int): Boolean = a >= b
    val kernels = build()
  }
  // guide: end

  def descending(xs: Array[Int]): Unit =
    // guide: descending.kse3
    xs.sortInOrder()(using Descending)
    // guide: end


  // guide: newtype.plain
  final case class MetersPlain(value: Double) extends AnyVal {
    def feet: Double = value * 3.28084
  }
  // guide: end

  // guide: newtype.kse3
  object Meters extends NewType[Double] {
    extension (m: Type)
      inline def feet: Double = m.value * 3.28084
  }
  // guide: end

  def newtypeUse(): (Double, Double, Boolean) =
    // guide: newtypeuse.kse3
    val m = Meters(2.0)                 // a Double at runtime, a Meters.Type to the compiler
    val cell = Mu(m)                    // a MuDouble, not a boxed cell, thanks to Translucent
    cell.zap(x => Meters(x.value * 2))
    // guide: end
    (m.feet, cell().value, m == Meters(2.0))


  // guide: labels.plain
  def clampPlain(x: Double, lo: Double, hi: Double): Double =
    if x < lo then lo else if x > hi then hi else x
  // guide: end

  // guide: labels.kse3
  def clamp(x: Double, lo: Double \ "lo", hi: Double \ "hi"): Double =
    if x < lo.unlabel then lo.unlabel else if x > hi.unlabel then hi.unlabel else x
  // guide: end

  def labelsUse(): Double =
    // guide: labelsuse.kse3
    val a = clamp(15, 0.0 \ "lo", 10.0 \ "hi")   // 10
    // clamp(15, 10.0 \ "hi", 0.0 \ "lo")        // refused at compile time: the labels don't match
    // guide: end
    a


  def tuples(): ((Int, Char), String, (Int, Char, Boolean), (String, Char)) =
    // guide: tuples.kse3
    val t = (1, 'a')
    val u = t.ops(_ + 1, _.toUpper)             // (2, 'A'): one function per slot
    val s = t.merge((n, c) => c.toString * n)   // "a": the slots become arguments
    val w = t.tup(true)                         // (1, 'a', true)
    val v = t.lens[0].to("one")                 // ("one", 'a')
    // guide: end
    (u, s, w, v)

  def namedTuples(): (String \ "name", (name: String, mass: Double), (name: String, mass: Double), (count: Int)) =
    // guide: named.kse3
    val fish = (name = "eel", mass = 2.5)
    val nm = fish.pluck("name")                 // String \ "name", a labeled value
    val bigger = NamesAndLabels.copyWithUpdateByName(fish, (mass = 3.0))
    val roundTrip = fish.asLabeled.asNamed      // via (String \ "name", Double \ "mass") and back
    val nt = (3 \ "count").nt                  // (count = 3)
    // guide: end
    (nm, bigger, roundTrip, nt)


  def memPlain(): (Double, Array[Double]) =
    // guide: mem.plain
    import java.lang.foreign.{Arena, ValueLayout}
    val seg = Arena.ofAuto().allocate(4 * 8)
    var i = 0L
    while i < 4 do
      seg.setAtIndex(ValueLayout.JAVA_DOUBLE, i, i * 1.5)
      i += 1
    seg.setAtIndex(ValueLayout.JAVA_DOUBLE, 1, 9.0)
    var total = 0.0
    i = 0
    while i < 4 do
      total += seg.getAtIndex(ValueLayout.JAVA_DOUBLE, i)
      i += 1
    val back = seg.toArray(ValueLayout.JAVA_DOUBLE)
    // guide: end
    (total, back)

  def memKse3(): (Double, Array[Double]) =
    // guide: mem.kse3
    val m = Mem.alloc[Double](4)             // off-heap, freed when the GC finds it unreachable
    m.set()(i => i * 1.5)
    m(1) = 9.0
    val total = m.gather(0.0)()((acc, x, _) => acc + x)
    val back = m.copyToArray()               // Array(0.0, 9.0, 3.0, 4.5)
    // guide: end
    (total, back)

  def records(): (Int, Double, Long) =
    // guide: aos.kse3
    type Fish = (id: Int, mass: Double)
    val fish = Mem.AoS.alloc[Fish](3)        // packed records, 12 bytes each, no padding
    fish.id(0) = 7
    fish.mass(0) = 2.5
    fish.mass.set(i => i * 1.25)             // a whole column at once
    // guide: end
    (fish.id(0), fish.mass(2), fish.stride)


  def explaining(t: Throwable): (String, String, Boolean) =
    // guide: explain.kse3
    val report = t.explain()                 // message, frames, causes and suppressed, one line each
    val brief = t.explain(lines = 3)         // the first three lines of each
    val safe = t.catchable                   // false for VM errors like OutOfMemoryError
    // guide: end
    (report, brief, safe)
}


@RunWith(classOf[JUnit4])
class GuideTest {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import GuideExamples as G

  // guide: tests.boilerplate
  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )
  // guide: end

  def testsPlain(): Unit =
    // guide: tests.plain
    assertEquals("almo", "salmon".substring(1, 5))
    assertArrayEquals(Array(1, 2, 0), Array(3, 1, 2).indices.sortBy(i => Array(3, 1, 2)(i)).toArray)
    assertEquals(0.333333333, 1.0 / 3, 1e-9)
    val e = assertThrows(classOf[ArrayIndexOutOfBoundsException], () => Array(1, 2)(5): Unit)
    assertEquals("Index 5 out of bounds for length 2", e.getMessage)
    // guide: end

  def testsKse3(): Unit =
    // guide: tests.kse3
    T ~ "salmon".select(1 to End-1)      ==== "almo"
    T ~ Array(3, 1, 2).indicesInOrder()  =**= Array(1, 2, 0)
    T ~ (1.0 / 3)                        =~~= 0.333333333
    T ~ Array(1, 2)(5)                   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ Iv(2, 5).length                  ==== 3 --: typed[Int]
    T("labels must match") ! """clamp(15, 10.0 \ "hi", 0.0 \ "lo")"""
    T("labels must match") \ """clamp(15, 0.0 \ "lo", 10.0 \ "hi")"""
    // guide: end

  def clamp(x: Double, lo: Double \ "lo", hi: Double \ "hi"): Double = G.clamp(x, lo, hi)

  def same[A](a: Array[A], b: Array[A]): Boolean = a.toList == b.toList

  @Test
  def testingSectionTest(): Unit =
    testsPlain()
    testsKse3()

  @Test
  def glueAndArraysTest(): Unit =
    val (s0, s1) = (new java.util.HashSet[Int], new java.util.HashSet[Int])
    val (r0, r1) = (collection.mutable.ArrayBuffer.empty[Int], collection.mutable.ArrayBuffer.empty[Int])
    for x <- List(1, 5, -3) do T ~ G.gluePlain(s0, r0 += _, x) ==== G.glueKse3(s1, r1 += _, x)
    T ~ s0 ==== s1
    T ~ r0 =**= r1
    val a = Array(-3, 1, 4, -1, 5, 9, -2, 6)
    val b = a.clone
    val (i0, n0) = G.arraysPlain(a)
    val (i1, n1) = G.arraysKse3(b)
    T ~ i0 =**= i1
    T ~ n0 =**= n1
    T ~ a =**= b
    val (y0, h0, p0, m0) = G.copyingPlain(a)
    val (y1, h1, p1, m1) = G.copyingKse3(a)
    T ~ y0 =**= y1
    T ~ h0 =**= h1
    T ~ p0 =**= p1
    T ~ m0 ==== m1
    val xs = Array(5, 6, 7, 8, 9, 10)
    G.assigning(xs, Array(1, 0, 2, 0))
    T ~ xs =**= Array(0, 10, 1, -1, 2, -1)
    val (w, r) = G.chopping()
    T ~ w.map(_.mkString) =**= Array("eel", "cod", "gar")
    T ~ r.map(_.toList)   =**= Array(List(1, 2), List(3), List(4))
    for i <- List(-1, 0, 3, 99) do T ~ G.modesPlain(a, i) ==== G.modesKse3(a, i)

  @Test
  def intervalsAndCountingTest(): Unit =
    val a = Array(-3, 1, 4, -1, 5, 9, -2, 6)
    val (iv, inner, sm, ty, both) = G.intervals(a)
    T ~ iv.pr    ==== "2..5"
    T ~ inner.pr ==== "1..7"
    T ~ sm.pr    ==== "1..7"
    T ~ ty.pr    ==== "1..7"
    T ~ both.pr  ==== "2..4"
    T ~ G.middlePlain("salmon", 1, 5)      ==== "almo"
    T ~ G.middle("salmon", 1 to End-1)     ==== "almo"
    T ~ G.middle("salmon", 2 to 4)         ==== "lmo"
    val (c0, c1) = (collection.mutable.ArrayBuffer.empty[Int], collection.mutable.ArrayBuffer.empty[Int])
    val (q0, d0) = G.countingPlain(5, c0 += _)
    val (q1, d1) = G.countingKse3(5, c1 += _)
    T ~ c0 =**= c1
    T ~ q0 =**= q1
    T ~ d0 =**= d1
    T ~ d1 =**= Array(4, 3, 2, 1, 0)

  @Test
  def cellsShortcutsStringsTest(): Unit =
    val a = Array(-3, 1, 4, -1, 5, 9, -2, 6)
    T ~ G.cellsPlain(a) ==== G.cellsKse3(a)
    var inits = 0
    T ~ G.onceAndCount(() => inits += 1) ==== 4L
    T ~ inits ==== 1
    val lines = Array("# comment", "eel", "", "cod", "END", "gar")
    val (l0, l1) = (collection.mutable.ArrayBuffer.empty[String], collection.mutable.ArrayBuffer.empty[String])
    G.linesPlain(lines, l0 += _)
    G.linesKse3(lines, l1 += _)
    T ~ l0 =**= l1
    T ~ l1 =**= Array("eel", "cod")
    for n <- List(1, 2) do T ~ G.stringsPlain(n, Array("a", "b"), "xs", a) ==== G.stringsKse3(n, Array("a", "b"), "xs", a)
    T ~ G.stringsKse3(2, Array("a", "b"), "xs", Array(1, 2, 3)) ==== ("Found 2 files in [a, b]", "xs: 1 2 3")
    val (poem, sameText) = G.margins()
    T ~ poem ==== sameText
    T ~ poem ==== "Salmon swim upstream\n  to spawn"

  @Test
  def sortingNewtypeLabelsTest(): Unit =
    val ages0 = Array(31, 25, 40, 25)
    val names0 = Array("eel", "cod", "gar", "ide")
    val ages1 = ages0.clone
    val names1 = names0.clone
    val (o0, n0) = G.sortingPlain(ages0, names0)
    val (o1, n1) = G.sortingKse3(ages1, names1)
    T ~ o0 =**= o1
    T ~ n0 =**= n1
    T ~ ages0 =**= ages1
    T ~ o1 =**= Array(1, 3, 0, 2)
    val d = Array(3, 1, 2)
    G.descending(d)
    T ~ d =**= Array(3, 2, 1)
    val (ft, dbl, eq) = G.newtypeUse()
    T ~ ft  ==== G.MetersPlain(2.0).feet
    T ~ dbl ==== 4.0
    T ~ eq  ==== true
    T ~ G.labelsUse() ==== 10.0
    T ~ G.clampPlain(15, 0, 10) ==== 10.0

  @Test
  def tuplesMemExceptionsTest(): Unit =
    val (u, s, w, v) = G.tuples()
    T ~ u ==== (2, 'A')
    T ~ s ==== "a"
    T ~ w ==== (1, 'a', true)
    T ~ v ==== ("one", 'a')
    val (nm, bigger, roundTrip, nt) = G.namedTuples()
    T ~ nm.unlabel ==== "eel"
    T ~ bigger.toTuple    ==== ("eel", 3.0)
    T ~ roundTrip.toTuple ==== ("eel", 2.5)
    T ~ nt.count   ==== 3
    val (t0, b0) = G.memPlain()
    val (t1, b1) = G.memKse3()
    T ~ t0 ==== t1
    T ~ b0 =**= b1
    T ~ b1 =**= Array(0.0, 9.0, 3.0, 4.5)
    T ~ G.records() ==== (7, 2.5, 12L)
    val boom = new RuntimeException("outer", new IllegalStateException("inner"))
    val (report, brief, safe) = G.explaining(boom)
    T ~ report.linesIterator.next() ==== "java.lang.RuntimeException: outer"
    T ~ report.contains("IllegalStateException: inner") ==== true
    T ~ (brief.length < report.length) ==== true
    T ~ safe ==== true
    T ~ new OutOfMemoryError().catchable ==== false
}
