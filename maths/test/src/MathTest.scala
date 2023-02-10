// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences LLC.

package kse.maths.test


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.time._

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable

import sourcecode.{Line, given}


@RunWith(classOf[JUnit4])
class MathTest {
  import java.nio.ByteBuffer
  import java.nio.ByteOrder

  import kse.testutilities.TestUtilities.{_, given}
  import kse.flow.{_, given}
  import kse.maths.{_, given}
  import kse.maths.packed.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue _
  )

  val packedTester   = new PackedTest()
  val temporalTester = new TemporalTest()


  def nlen(s: String) = if s eq null then -1 else s.length
  def nullone[N >: Null](n: N) = if n == null then 1 else -1

  def randomTestWith(rng: Prng, name: String): Unit =
    T(name) ~ rng.isClean ==== true
    var r2 = rng.copy
    nFor(10){ n => T(s"$name iter $n") ~ rng.Z ==== r2.Z }
    T(name) ~ rng.isClean ==== r2.isClean
    T(name) ~ rng.isClean ==== false
    T(name) ~ rng.B ==== r2.B
    T(name) ~ rng.S ==== r2.S
    T(name) ~ rng.C ==== r2.C
    T(name) ~ rng.I ==== r2.I
    T(name) ~ rng.F ==== r2.F
    T(name) ~ rng.L ==== r2.L
    T(name) ~ rng.D ==== r2.D
    nFor(1000){ n => val title = s"$name iter $n"; T(title) ~ { val a = rng.D; 0 <= a && a <= 1 } ==== true }
    var hit0 = false
    var hit42 = false
    nFor(1000){ n =>
      val title = s"$name iter $n"
      T(title) ~ { val a = rng % 43L; if a == 0 then hit0 = true; if a == 42 then hit42 = true; 0 <= a && a <= 42 } ==== true 
    }
    T(name) ~ hit0  ==== true
    T(name) ~ hit42 ==== true
    hit0 = false
    hit42 = false
    r2 = rng.copy
    nFor(1000){ n => 
      val title = s"$name iter $n"
      T(title) ~ { val a = rng % 43; if a == 0 then hit0 = true; a } ==== { val b = r2 % 43; if b == 42 then hit42 = true; b }
    }
    T(name) ~ hit0  ==== true
    T(name) ~ hit42 ==== true

    // Expected number of extreme values is 23
    var nHi = 0
    nFor(1000){ n => if rng.gaussian > 2.0 then nHi += 1 }
    T(name) ~ (nHi >= 10) ==== true
    T(name) ~ (nHi <= 50) ==== true
    var nLo = 10
    nFor(500){ n => r2.gaussianPair((x, y) => { if x < -2 then nLo += 1; if y < -2 then nLo += 1 }) }
    T(name) ~ (nLo >= 10) ==== true
    T(name) ~ (nLo <= 50) ==== true
    val v = r2.gaussianVc
    T(name) ~ rng.gaussian.toFloat ==== v.x
    T(name) ~ rng.gaussian.toFloat ==== v.y

    nFor(10){ n => T(s"$name iter $n") ~ rng.Z ==== { var x = false; r2.useZ(x = _); x } }
    T(name) ~ rng.B ==== { var x: Byte   = 0; r2.useB(x = _); x }
    T(name) ~ rng.S ==== { var x: Short  = 0; r2.useS(x = _); x }
    T(name) ~ rng.C ==== { var x  = 0.toChar; r2.useC(x = _); x }
    T(name) ~ rng.I ==== { var x: Int    = 0; r2.useI(x = _); x }
    T(name) ~ rng.F ==== { var x: Float  = 0; r2.useF(x = _); x }
    T(name) ~ rng.L ==== { var x: Long   = 0; r2.useL(x = _); x }
    T(name) ~ rng.D ==== { var x: Double = 0; r2.useD(x = _); x }
    T(name) ~ (rng % 43)   ==== { var x: Int  = 0; r2.useModI(43 )(x = _); x }
    T(name) ~ (rng % 43L)  ==== { var x: Long = 0; r2.useModL(43L)(x = _); x }
    T(name) ~ rng.gaussian ==== { var x     = 0.0; r2.useGaussian( x = _); x }

    val zs1 = Array.fill(20)(false)
    val zs2 = Array.fill(20)(false)
    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % zs1.length
      val j = i + (rng % (zs1.length - i))
      r2 = rng.copy
      rng.fillRangeZ(zs1, i, j)
      for k <- i until j do
        zs2(k) = r2.Z
      T(title) ~ zs1 =**= zs2
    }

    val bs1 = Array.fill(20)(0: Byte)
    val bs2 = Array.fill(20)(0: Byte)
    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % bs1.length
      val j = i + (rng % (bs1.length - i))
      r2 = rng.copy
      rng.fillRangeB(bs1, i, j)
      for k <- i until j do
        bs2(k) = r2.B
      T(title) ~ bs1 =**= bs2
    }

    val ss1 = Array.fill(20)(0: Short)
    val ss2 = Array.fill(20)(0: Short)
    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % ss1.length
      val j = i + (rng % (ss1.length - i))
      r2 = rng.copy
      rng.fillRangeS(ss1, i, j)
      for k <- i until j do
        ss2(k) = r2.S
      T(title) ~ ss1 =**= ss2
    }

    val cs1 = Array.fill(20)('\u0000')
    val cs2 = Array.fill(20)('\u0000')
    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % cs1.length
      val j = i + (rng % (cs1.length - i))
      r2 = rng.copy
      rng.fillRangeC(cs1, i, j)
      for k <- i until j do
        cs2(k) = r2.C
      T(title) ~ cs1 =**= cs2
    }

    val is1 = Array.fill(20)(0)
    val is2 = Array.fill(20)(0)
    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % is1.length
      val j = i + (rng % (is1.length - i))
      r2 = rng.copy
      rng.fillRangeI(is1, i, j)
      for k <- i until j do
        is2(k) = r2.I
      T(title) ~ is1 =**= is2
    }

    val ls1 = Array.fill(20)(0L)
    val ls2 = Array.fill(20)(0L)
    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % ls1.length
      val j = i + (rng % (ls1.length - i))
      r2 = rng.copy
      rng.fillRangeL(ls1, i, j)
      for k <- i until j do
        ls2(k) = r2.L
      T(title) ~ ls1 =**= ls2
    }

    val fs1 = Array.fill(20)(0.0f)
    val fs2 = Array.fill(20)(0.0f)
    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % fs1.length
      val j = i + (rng % (fs1.length - i))
      r2 = rng.copy
      rng.fillRangeF(fs1, i, j)
      for k <- i until j do
        fs2(k) = r2.F
      T(title) ~ fs1 =**= fs2
    }

    val ds1 = Array.fill(20)(0.0)
    val ds2 = Array.fill(20)(0.0)
    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % ds1.length
      val j = i + (rng % (ds1.length - i))
      r2 = rng.copy
      rng.fillRangeD(ds1, i, j)
      for k <- i until j do
        ds2(k) = r2.D
      T(title) ~ ds1 =**= ds2
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % is1.length
      val j = i + (rng % (is1.length - i))
      r2 = rng.copy
      rng.fillRangeModI(43)(is1, i, j)
      for k <- i until j do
        is2(k) = r2 % 43
      T(title) ~ is1 =**= is2
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % ls1.length
      val j = i + (rng % (ls1.length - i))
      r2 = rng.copy
      rng.fillRangeModL(43L)(ls1, i, j)
      for k <- i until j do
        ls2(k) = r2 % 43L
      T(title) ~ ls1 =**= ls2
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % ds1.length
      val j = i + (rng % (ds1.length - i))
      r2 = rng.copy
      rng.fillRangeGaussian(ds1, i, j)
      for k <- i until j do
        ds2(k) = r2.gaussian
      T(title) ~ ds1 =**= ds2
    }

    T(name) ~ { rng.fillZ(zs1); zs1 } =**= { r2.arrayZ(zs1.length) }
    T(name) ~ { rng.fillB(bs1); bs1 } =**= { r2.arrayB(bs1.length) }
    T(name) ~ { rng.fillS(ss1); ss1 } =**= { r2.arrayS(ss1.length) }
    T(name) ~ { rng.fillC(cs1); cs1 } =**= { r2.arrayC(cs1.length) }
    T(name) ~ { rng.fillI(is1); is1 } =**= { r2.arrayI(is1.length) }
    T(name) ~ { rng.fillL(ls1); ls1 } =**= { r2.arrayL(ls1.length) }
    T(name) ~ { rng.fillF(fs1); fs1 } =**= { r2.arrayF(fs1.length) }
    T(name) ~ { rng.fillD(ds1); ds1 } =**= { r2.arrayD(ds1.length) }
    T(name) ~ { rng.fillModI(43)(is1); is1 } =**= { r2.arrayModI(43)(is1.length) }
    T(name) ~ { rng.fillModL(43)(ls1); ls1 } =**= { r2.arrayModL(43)(ls1.length) }
    T(name) ~ { rng.fillGaussian(ds1); ds1 } =**= { r2.arrayGaussian(ds1.length) }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % zs1.length
      val j = i + (rng % (zs1.length - i))
      r2 = rng.copy
      System.arraycopy(zs1, 0, zs2, 0, zs1.length)
      rng.shuffleRange(zs1, i, j)
      T(title) ~ zs1.slice(i, j).sorted =**= zs2.slice(i, j).sorted
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % bs1.length
      val j = i + (rng % (bs1.length - i))
      r2 = rng.copy
      System.arraycopy(bs1, 0, bs2, 0, bs1.length)
      rng.shuffleRange(bs1, i, j)
      T(title) ~ bs1.slice(i, j).sorted =**= bs2.slice(i, j).sorted
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % ss1.length
      val j = i + (rng % (ss1.length - i))
      r2 = rng.copy
      System.arraycopy(ss1, 0, ss2, 0, ss1.length)
      rng.shuffleRange(ss1, i, j)
      T(title) ~ ss1.slice(i, j).sorted =**= ss2.slice(i, j).sorted
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % cs1.length
      val j = i + (rng % (cs1.length - i))
      r2 = rng.copy
      System.arraycopy(cs1, 0, cs2, 0, cs1.length)
      rng.shuffleRange(cs1, i, j)
      T(title) ~ cs1.slice(i, j).sorted =**= cs2.slice(i, j).sorted
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % is1.length
      val j = i + (rng % (is1.length - i))
      r2 = rng.copy
      System.arraycopy(is1, 0, is2, 0, is1.length)
      rng.shuffleRange(is1, i, j)
      T(title) ~ is1.slice(i, j).sorted =**= is2.slice(i, j).sorted
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % ls1.length
      val j = i + (rng % (ls1.length - i))
      r2 = rng.copy
      System.arraycopy(ls1, 0, ls2, 0, ls1.length)
      rng.shuffleRange(ls1, i, j)
      T(title) ~ ls1.slice(i, j).sorted =**= ls2.slice(i, j).sorted
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % fs1.length
      val j = i + (rng % (fs1.length - i))
      r2 = rng.copy
      System.arraycopy(fs1, 0, fs2, 0, fs1.length)
      rng.shuffleRange(fs1, i, j)
      T(title) ~ fs1.slice(i, j).sorted =**= fs2.slice(i, j).sorted
    }

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % ds1.length
      val j = i + (rng % (ds1.length - i))
      r2 = rng.copy
      System.arraycopy(ds1, 0, ds2, 0, ds1.length)
      rng.shuffleRange(ds1, i, j)
      T(title) ~ ds1.slice(i, j).sorted =**= ds2.slice(i, j).sorted
    }

    val as1 = ds1.map(d => if d >= 0 then Some(d) else None)
    val as2 = ds2.map(d => if d >= 0 then Some(d) else None)
    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % as1.length
      val j = i + (rng % (as1.length - i))
      r2 = rng.copy
      System.arraycopy(as1, 0, as2, 0, as1.length)
      rng.shuffleRange(as1, i, j)
      T(title) ~ as1.slice(i, j).sorted =**= as2.slice(i, j).sorted
    }

    T(name) ~ { rng.shuffle(zs1); zs1.sorted } =**= zs2.sorted
    T(name) ~ { rng.shuffle(bs1); bs1.sorted } =**= bs2.sorted
    T(name) ~ { rng.shuffle(ss1); ss1.sorted } =**= ss2.sorted
    T(name) ~ { rng.shuffle(cs1); cs1.sorted } =**= cs2.sorted
    T(name) ~ { rng.shuffle(is1); is1.sorted } =**= is2.sorted
    T(name) ~ { rng.shuffle(ls1); ls1.sorted } =**= ls2.sorted
    T(name) ~ { rng.shuffle(fs1); fs1.sorted } =**= fs2.sorted
    T(name) ~ { rng.shuffle(ds1); ds1.sorted } =**= ds2.sorted
    T(name) ~ { rng.shuffle(as1); as1.sorted } =**= as2.sorted

    for i <- 0 until rng.stateLength do
      r2.setState(i)(rng.getState(i))
    r2.setCacheAndBits(rng.getCache, rng.getCacheBits)
    T(name) ~ rng.L ==== r2.L
    rng.Z
    for i <- 0 until rng.stateLength do
      r2.setState(i)(rng.getState(i))
    r2.setCacheAndBits(rng.getCache, rng.getCacheBits)
    T(name) ~ rng.I ==== r2.I
    rng.L
    for i <- 0 until rng.stateLength do
      r2.setState(i)(rng.getState(i))
    r2.setCacheAndBits(rng.getCache, rng.getCacheBits)
    T(name) ~ rng.I ==== r2.I

    nFor(20){ n =>
      val title = s"$name iter $n"
      val i = rng % as1.length
      val j = i + (rng % (as1.length - i))
      r2 = rng.copy

      System.arraycopy(zs1, 0, zs2, 0, zs1.length)
      rng.shuffleRange(zs1, i, j)
      zs2.shuffle(i, j)(r2)
      T(title) ~ zs1 =**= zs2
      rng.shuffle(zs1)
      zs2.shuffle(r2)
      T(title) ~ zs1 =**= zs2
      rng.fillRangeZ(zs1, i, j)
      zs2.randomFill(i, j)(r2)
      T(title) ~ zs1 =**= zs2
      rng.fillZ(zs1)
      zs2.randomFill(r2)
      T(title) ~ zs1 =**= zs2

      System.arraycopy(bs1, 0, bs2, 0, bs1.length)
      rng.shuffleRange(bs1, i, j)
      bs2.shuffle(i, j)(r2)
      T(title) ~ bs1 =**= bs2
      rng.shuffle(bs1)
      bs2.shuffle(r2)
      T(title) ~ bs1 =**= bs2
      rng.fillRangeB(bs1, i, j)
      bs2.randomFill(i, j)(r2)
      T(title) ~ bs1 =**= bs2
      rng.fillB(bs1)
      bs2.randomFill(r2)
      T(title) ~ bs1 =**= bs2

      System.arraycopy(ss1, 0, ss2, 0, ss1.length)
      rng.shuffleRange(ss1, i, j)
      ss2.shuffle(i, j)(r2)
      T(title) ~ ss1 =**= ss2
      rng.shuffle(ss1)
      ss2.shuffle(r2)
      T(title) ~ ss1 =**= ss2
      rng.fillRangeS(ss1, i, j)
      ss2.randomFill(i, j)(r2)
      T(title) ~ ss1 =**= ss2
      rng.fillS(ss1)
      ss2.randomFill(r2)
      T(title) ~ ss1 =**= ss2

      System.arraycopy(cs1, 0, cs2, 0, cs1.length)
      rng.shuffleRange(cs1, i, j)
      cs2.shuffle(i, j)(r2)
      T(title) ~ cs1 =**= cs2
      rng.shuffle(cs1)
      cs2.shuffle(r2)
      T(title) ~ cs1 =**= cs2
      rng.fillRangeC(cs1, i, j)
      cs2.randomFill(i, j)(r2)
      T(title) ~ cs1 =**= cs2
      rng.fillC(cs1)
      cs2.randomFill(r2)
      T(title) ~ cs1 =**= cs2

      System.arraycopy(is1, 0, is2, 0, is1.length)
      rng.shuffleRange(is1, i, j)
      is2.shuffle(i, j)(r2)
      T(title) ~ is1 =**= is2
      rng.shuffle(is1)
      is2.shuffle(r2)
      T(title) ~ is1 =**= is2
      rng.fillRangeI(is1, i, j)
      is2.randomFill(i, j)(r2)
      T(title) ~ is1 =**= is2
      rng.fillI(is1)
      is2.randomFill(r2)
      T(title) ~ is1 =**= is2

      System.arraycopy(ls1, 0, ls2, 0, ls1.length)
      rng.shuffleRange(ls1, i, j)
      ls2.shuffle(i, j)(r2)
      T(title) ~ ls1 =**= ls2
      rng.shuffle(ls1)
      ls2.shuffle(r2)
      T(title) ~ ls1 =**= ls2
      rng.fillRangeL(ls1, i, j)
      ls2.randomFill(i, j)(r2)
      T(title) ~ ls1 =**= ls2
      rng.fillL(ls1)
      ls2.randomFill(r2)
      T(title) ~ ls1 =**= ls2

      System.arraycopy(fs1, 0, fs2, 0, fs1.length)
      rng.shuffleRange(fs1, i, j)
      fs2.shuffle(i, j)(r2)
      T(title) ~ fs1 =**= fs2
      rng.shuffle(fs1)
      fs2.shuffle(r2)
      T(title) ~ fs1 =**= fs2
      rng.fillRangeF(fs1, i, j)
      fs2.randomFill(i, j)(r2)
      T(title) ~ fs1 =**= fs2
      rng.fillF(fs1)
      fs2.randomFill(r2)
      T(title) ~ fs1 =**= fs2

      System.arraycopy(ds1, 0, ds2, 0, ds1.length)
      rng.shuffleRange(ds1, i, j)
      ds2.shuffle(i, j)(r2)
      T(title) ~ ds1 =**= ds2
      rng.shuffle(ds1)
      ds2.shuffle(r2)
      T(title) ~ ds1 =**= ds2
      rng.fillRangeD(ds1, i, j)
      ds2.randomFill(i, j)(r2)
      T(title) ~ ds1 =**= ds2
      rng.fillD(ds1)
      ds2.randomFill(r2)
      T(title) ~ ds1 =**= ds2

      System.arraycopy(as1, 0, as2, 0, as1.length)
      rng.shuffleRange(as1, i, j)
      as2.shuffle(i, j)(r2)
      T(title) ~ as1 =**= as2
      rng.shuffle(as1)
      as2.shuffle(r2)
      T(title) ~ as1 =**= as2

      rng.fillRangeModI(43)(is1, i, j)
      is2.randomMod(43)(i, j)(r2)
      T(title) ~ is1 =**= is2
      rng.fillModI(43)(is1)
      is2.randomMod(43)(r2)
      T(title) ~ is1 =**= is2

      rng.fillRangeModL(43)(ls1, i, j)
      ls2.randomMod(43)(i, j)(r2)
      T(title) ~ ls1 =**= ls2
      rng.fillModL(43)(ls1)
      ls2.randomMod(43)(r2)
      T(title) ~ ls1 =**= ls2

      rng.fillRangeGaussian(ds1, i, j)
      ds2.randomGaussian(i, j)(r2)
      T(title) ~ ds1 =**= ds2
      rng.fillGaussian(ds1)
      ds2.randomGaussian(r2)
      T(title) ~ ds1 =**= ds2
    }

    {
      given Prng = rng
      T(name) ~ 1500 .roll ==== 1 + r2 % 1500
      T(name) ~ 1500L.roll ==== 1 + r2 % 1500L
      T(name) ~ (40 d 100) ==== r2.arrayModI(100)(40).foldLeft(0)(_ + _ + 1)
    }

    val tfs = rng.stringFrom("FT", 100)
    T(name) ~ (tfs.count(_ == 'T') > 25) ==== true
    T(name) ~ (tfs.count(_ == 'T') < 75) ==== true
    T(name) ~ tfs.forall(c => c == 'T' || c == 'F') ==== true
    val webs = rng.webString(10000)
    val wmap = webs.groupBy(identity).map{ case (c, vs) => c -> vs.length }
    T(name) ~ wmap.forall(Prng.WebCharacters contains _._1) ==== true
    T(name) ~ Prng.WebCharacters.forall(c => wmap(c) > 0) ==== true
    val texts = rng.textString(10000)
    val tmap = texts.groupBy(identity).map{ case (c, vs) => c -> vs.length }
    T(name) ~ tmap.forall(Prng.TextCharacters contains _._1) ==== true
    T(name) ~ Prng.TextCharacters.forall(c => tmap(c) > 0) ==== true
    val asciis = rng.asciiString(10000)
    val amap = asciis.groupBy(identity).map{ case (c, vs) => c -> vs.length }
    T(name) ~ amap.forall{ case (c, _) => 0 <= c && c < 256 } ==== true
    T(name) ~ (0 until 256).forall(i => amap(i.toChar) > 0) ==== true
    var nsur = 0
    nFor(1000) { n =>
      val title = s"Valid string iteration $n"
      val s = rng.validString(20.roll(using rng))
      var i = 0
      while i < s.length do
        if java.lang.Character.isHighSurrogate(s charAt i) then
          nsur += 1
          T(title) ~ java.lang.Character.isLowSurrogate(s charAt (i+1)) ==== true
          i += 1
        i += 1
    }
    T(name) ~ (nsur > 100) ==== true
    T(name) ~ (nsur < 300) ==== true
    ()


  @Test
  def randomTest(): Unit =
    val prng = Prng(9825719879125879L)
    randomTestWith(prng, "default Prng")

    val shmx = ShiftMix64(29891528751856L)
    randomTestWith(shmx, "ShiftMix64")

    val pcgr = Pcg64(1276561648165L)
    randomTestWith(pcgr, "Pcg64")
    ()


  sealed trait H {
    def bytes: Int
    def addToBuffer(bb: ByteBuffer): Unit
    def addToArray(ab: Array[Byte], index: Int): Int
    final def inBuffer: ByteBuffer =
      val bb = ByteBuffer.wrap(new Array[Byte](bytes)).order(ByteOrder.LITTLE_ENDIAN)
      addToBuffer(bb)
      bb.flip()
      bb
    final def inArray: Array[Byte] =
      val ab = new Array[Byte](bytes)
      addToArray(ab, 0)
      ab
    def hashInto(h: SimpleIncrementalHash): h.type
  }
  case class HZ(z: Boolean) extends H {
    def bytes = 1
    def addToBuffer(bb: ByteBuffer): Unit = bb put (if z then 1: Byte else 0: Byte)
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = if z then 1: Byte else 0: Byte
      index + 1
    def hashInto(h: SimpleIncrementalHash): h.type = h += z
  }
  case class HB(b: Byte) extends H {
    def bytes = 1
    def addToBuffer(bb: ByteBuffer): Unit = bb put b
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = b
      index + 1
    def hashInto(h: SimpleIncrementalHash): h.type = h += b
  }
  case class HS(s: Short) extends H {
    def bytes = 2
    def addToBuffer(bb: ByteBuffer): Unit = bb putShort s
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = (s & 0xFF).toByte
      ab(index + 1) = ((s & 0xFF00) >> 8).toByte
      index + 2
    def hashInto(h: SimpleIncrementalHash): h.type = h += s
  }
  case class HC(c: Char) extends H {
    def bytes = 2
    def addToBuffer(bb: ByteBuffer): Unit = bb putChar c
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = (c & 0xFF).toByte
      ab(index + 1) = ((c & 0xFF00) >> 8).toByte
      index + 2
    def hashInto(h: SimpleIncrementalHash): h.type = h += c
  }
  case class HI(i: Int) extends H {
    def bytes = 4
    def addToBuffer(bb: ByteBuffer): Unit = bb putInt i
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = (i & 0xFF).toByte
      ab(index + 1) = ((i & 0xFF00) >> 8).toByte
      ab(index + 2) = ((i & 0xFF0000) >> 16).toByte
      ab(index + 3) = ((i & 0xFF000000) >> 24).toByte
      index + 4
    def hashInto(h: SimpleIncrementalHash): h.type = h += i
  }
  case class HL(l: Long) extends H {
    def bytes = 8
    def addToBuffer(bb: ByteBuffer): Unit = bb putLong l
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = (l & 0xFFL).toByte
      ab(index + 1) = ((l & 0xFF00L) >> 8).toByte
      ab(index + 2) = ((l & 0xFF0000L) >> 16).toByte
      ab(index + 3) = ((l & 0xFF000000L) >> 24).toByte
      ab(index + 4) = ((l & 0xFF00000000L) >> 32).toByte
      ab(index + 5) = ((l & 0xFF0000000000L) >> 40).toByte
      ab(index + 6) = ((l & 0xFF000000000000L) >> 48).toByte
      ab(index + 7) = ((l & 0xFF00000000000000L) >> 56).toByte
      index + 8
    def hashInto(h: SimpleIncrementalHash): h.type = h += l
  }
  class Hp(val proxy: H) extends H {
    def bytes = proxy.bytes
    def addToBuffer(bb: ByteBuffer): Unit = proxy.addToBuffer(bb)
    def addToArray(ab: Array[Byte], index: Int): Int = proxy.addToArray(ab, index)
    def hashInto(h: SimpleIncrementalHash): h.type = proxy.hashInto(h)
  }
  case class HF(f: Float) extends Hp(HI(java.lang.Float.floatToRawIntBits(f))) {}
  case class HD(d: Double) extends Hp(HL(java.lang.Double.doubleToRawLongBits(d))) {}
  case class Harr(ab: Array[Byte], i0: Int, iN: Int) extends H {
    private[this] val j0 = math.max(i0, 0)
    private[this] val jN = math.min(iN, ab.length)
    val bytes = math.max(0, jN - j0)
    def addToBuffer(bb: ByteBuffer): Unit = bb.put(ab, j0, bytes)
    def addToArray(ab: Array[Byte], index: Int): Int =
      System.arraycopy(this.ab, j0, ab, index, bytes)
      index + bytes
    def hashInto(h: SimpleIncrementalHash): h.type =
      if i0 == 0 && iN == ab.length then h += ab
      else
        h.append(ab, i0, iN)
        h
  }
  case class Hbb(bb: ByteBuffer) extends H {
    val origin = bb.position
    val bytes = bb.remaining
    def addToBuffer(bbb: ByteBuffer): Unit =
      val b = bb.asReadOnlyBuffer
      b order ByteOrder.LITTLE_ENDIAN
      bbb put b
    def addToArray(ab: Array[Byte], index: Int): Int =
      var i = index
      val b = bb.asReadOnlyBuffer
      b order ByteOrder.LITTLE_ENDIAN
      while b.remaining > 0 do
        ab(i) = b.get
        i += 1
      i
    def hashInto(h: SimpleIncrementalHash): h.type =
      val b = bb.asReadOnlyBuffer
      b order ByteOrder.LITTLE_ENDIAN
      h append b
      h
  }
  case class Hstr(s: String) extends H {
    val bytes = 2 * s.length
    def addToBuffer(bb: ByteBuffer): Unit =
      var i = 0
      while i < s.length do
        bb putChar s.charAt(i)
        i += 1
    def addToArray(ab: Array[Byte], index: Int): Int =
      var i = index
      var j = 0
      while j < s.length do
        val c = s charAt j
        ab(i) = (c & 0xFF).toByte
        ab(i+1) = ((c & 0xFF00) >> 8).toByte
        j += 1
        i += 2
      i
    def hashInto(h: SimpleIncrementalHash): h.type = h += s
  }
  case class Hiter(i: scala.collection.Iterable[H]) extends H {
    def bytes = i.foldLeft(0)(_ + _.bytes)
    def addToBuffer(bb: ByteBuffer): Unit = for x <- i do x.addToBuffer(bb)
    def addToArray(ab: Array[Byte], index: Int): Int =
      var j = index
      for x <- i do j = x.addToArray(ab, j)
      j
    def hashInto(h: SimpleIncrementalHash): h.type =
      i.iterator.foreach(_.hashInto(h))
      h
    def take(n: Int): Hiter = new Hiter(i take n)
    def drop(n: Int): Hiter = new Hiter(i drop n)
    def noprox: Hiter = new Hiter(i.map{ case p: Hp => p.proxy; case h => h })
  }

  def randomHs(n: Int, lim: Int = 128)(rng: Prng): Hiter =
    val a = collection.mutable.ArrayBuffer.empty[H]
    given Prng = rng
    nFor(n){ _ =>
      val h: H = (rng % 11) match {
        case 0 => HZ(rng.Z)
        case 1 => HB(rng.B)
        case 2 => HS(rng.S)
        case 3 => HC(rng.C)
        case 4 => HI(rng.I)
        case 5 => HL(rng.L)
        case 6 => HF(rng.F)
        case 7 => HD(rng.D)
        case 8 => val k = lim.roll; val i = rng % (k+1); val j = rng % (k+1); Harr(rng.arrayB(k), i, j)
        case 9 => Hbb(ByteBuffer wrap rng.arrayB(lim.roll))
        case _ => Hstr(rng.validString((lim/2).roll))
      }
      a += h
    }
    Hiter(a)


  def hash32test(
    h: (HZ, HB, HS, HC, HI, HL, HF, HD, Hstr, Harr, Hbb, Hiter),
    title: String,
    i32a: IncrementalHash[Int, Int],
    i32b: IncrementalHash[Int, Int],
    f32: FullHash32
  ): Unit =
    import java.lang.Float.{floatToRawIntBits => f2i}
    import java.lang.Double.{doubleToRawLongBits => d2l}
    val (hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx) = h

    T(title) ~ i32a.begin().appendByte(if hz.z then 1 else 0).result() ==== i32b.begin().append(hz.inBuffer).result()
    T(title) ~ { i32a.begin() += hz.z; i32a.result() } ==== i32b.begin().append(hz.inArray, 0, hz.bytes).result()
    T(title) ~ i32a.begin().result(hz.inBuffer) ==== f32.hash32(hz.inArray)
    T(title) ~ i32a.begin().result(hz.inArray, 0, hz.bytes) ==== f32.hash32(hz.inBuffer)

    T(title) ~ i32a.begin().appendByte(hb.b).result() ==== i32b.begin().append(hb.inBuffer).result()
    T(title) ~ { i32a.begin() += hb.b; i32a.result() } ==== i32b.begin().append(hb.inArray, 0, hb.bytes).result()
    T(title) ~ i32a.begin().result(hb.inBuffer) ==== f32.hash32(hb.inArray)
    T(title) ~ i32a.begin().result(hb.inArray, 0, hb.bytes) ==== f32.hash32(hb.inBuffer)

    T(title) ~ i32a.begin().appendChar((hs.s & 0xFFFF).toChar).result() ==== i32b.begin().append(hs.inBuffer).result()
    T(title) ~ { i32a.begin() += hs.s; i32a.result() } ==== i32b.begin().append(hs.inArray, 0, hs.bytes).result()
    T(title) ~ i32a.begin().result(hs.inBuffer) ==== f32.hash32(hs.inArray)
    T(title) ~ i32a.begin().result(hs.inArray, 0, hs.bytes) ==== f32.hash32(hs.inBuffer)

    T(title) ~ i32a.begin().appendChar(hc.c).result() ==== i32b.begin().append(hc.inBuffer).result()
    T(title) ~ { i32a.begin() += hc.c; i32a.result() } ==== i32b.begin().append(hc.inArray, 0, hc.bytes).result()
    T(title) ~ i32a.begin().result(hc.inBuffer) ==== f32.hash32(hc.inArray)
    T(title) ~ i32a.begin().result(hc.inArray, 0, hc.bytes) ==== f32.hash32(hc.inBuffer)

    T(title) ~ i32a.begin().appendInt(hi.i).result() ==== i32b.begin().append(hi.inBuffer).result()
    T(title) ~ { i32a.begin() += hi.i; i32a.result() } ==== i32b.begin().append(hi.inArray, 0, hi.bytes).result()
    T(title) ~ i32a.begin().result(hi.inBuffer) ==== f32.hash32(hi.inArray)
    T(title) ~ i32a.begin().result(hi.inArray, 0, hi.bytes) ==== f32.hash32(hi.inBuffer)

    T(title) ~ i32a.begin().appendLong(hl.l).result() ==== i32b.begin().append(hl.inBuffer).result()
    T(title) ~ { i32a.begin() += hl.l; i32a.result() } ==== i32b.begin().append(hl.inArray, 0, hl.bytes).result()
    T(title) ~ i32a.begin().result(hl.inBuffer) ==== f32.hash32(hl.inArray)
    T(title) ~ i32a.begin().result(hl.inArray, 0, hl.bytes) ==== f32.hash32(hl.inBuffer)

    T(title) ~ i32a.begin().appendInt(f2i(hf.f)).result() ==== i32b.begin().append(hf.inBuffer).result()
    T(title) ~ { i32a.begin() += hf.f; i32a.result() } ==== i32b.begin().append(hf.inArray, 0, hf.bytes).result()
    T(title) ~ i32a.begin().result(hf.inBuffer) ==== f32.hash32(hf.inArray)
    T(title) ~ i32a.begin().result(hf.inArray, 0, hf.bytes) ==== f32.hash32(hf.inBuffer)

    T(title) ~ i32a.begin().appendLong(d2l(hd.d)).result() ==== i32b.begin().append(hd.inBuffer).result()
    T(title) ~ { i32a.begin() += hd.d; i32a.result() } ==== i32b.begin().append(hd.inArray, 0, hd.bytes).result()
    T(title) ~ i32a.begin().result(hd.inBuffer) ==== f32.hash32(hd.inArray)
    T(title) ~ i32a.begin().result(hd.inArray, 0, hd.bytes) ==== f32.hash32(hd.inBuffer)

    T(title) ~ i32a.begin().result(hh.s, 0, hh.s.length) ==== hh.hashInto(i32b.begin()).result()
    T(title) ~ i32a.begin().append(hh.s, 0, hh.s.length).result() ==== i32b.begin().append(hh.inBuffer).result()
    T(title) ~ hh.hashInto(i32a.begin()).result() ==== i32b.begin().append(hh.inArray, 0, hh.bytes).result()
    T(title) ~ i32a.begin().result(hh.inBuffer) ==== f32.hash32(hh.inArray)
    T(title) ~ i32a.begin().result(hh.inArray, 0, hh.bytes) ==== f32.hash32(hh.inBuffer)
    T(title) ~ hh.hashInto(i32a.begin()).result() ==== f32.hash32(hh.s)
    T(title) ~ i32a.begin(185162).result(hh.s, 0, hh.s.length) ==== f32.hash32(185162, hh.s)

    T(title) ~ i32a.begin().result(hr.ab, hr.i0, hr.iN) ==== hr.hashInto(i32b.begin()).result()
    T(title) ~ i32a.begin().append(hr.ab, hr.i0, hr.iN).result() ==== i32b.begin().append(hr.inBuffer).result()
    T(title) ~ hr.hashInto(i32a.begin()).result() ==== i32b.begin().append(hr.inArray, 0, hr.bytes).result()
    T(title) ~ i32a.begin().result(hr.inBuffer) ==== f32.hash32(hr.inArray)
    T(title) ~ i32a.begin().result(hr.inArray, 0, hr.bytes) ==== f32.hash32(hr.inBuffer)
    T(title) ~ i32a.begin(82351).result(hr.ab, hr.i0, hr.iN) ==== f32.hash32(82351, hr.ab, hr.i0, hr.iN)

    T(title) ~ hq.hashInto(i32a.begin()).result() ==== i32b.begin().append(hq.inBuffer).result()
    T(title) ~ hq.hashInto(i32a.begin()).result() ==== i32b.begin().append(hq.inArray, 0, hq.bytes).result()
    T(title) ~ i32a.begin().result(hq.inBuffer) ==== f32.hash32(hq.inArray)
    T(title) ~ i32a.begin().result(hq.inArray, 0, hq.bytes) ==== f32.hash32(hq.inBuffer)
    T(title) ~ i32a.begin(98158).result(hq.inArray, 0, hq.bytes) ==== f32.hash32(98158, hq.inBuffer)

    T(title) ~ hx.hashInto(i32a.begin()).result() ==== i32b.begin().append(hx.inBuffer).result()
    T(title) ~ hx.hashInto(i32a.begin()).result() ==== i32b.begin().append(hx.inArray, 0, hx.bytes).result()
    T(title) ~ i32a.begin().result(hx.inBuffer) ==== f32.hash32(hx.inArray)
    T(title) ~ i32a.begin().result(hx.inArray, 0, hx.bytes) ==== f32.hash32(hx.inBuffer)

    hx.hashInto(i32a.begin())
    T(title) ~ hx.hashInto(i32a.copy).result() ==== hx.hashInto(i32a).result()


  def hash64test(
    h: (HZ, HB, HS, HC, HI, HL, HF, HD, Hstr, Harr, Hbb, Hiter),
    title: String,
    i64a: IncrementalHash[Long, Long],
    i64b: IncrementalHash[Long, Long],
    f64: FullHash64
  ): Unit =
    import java.lang.Float.{floatToRawIntBits => f2i}
    import java.lang.Double.{doubleToRawLongBits => d2l}
    val (hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx) = h

    T(title) ~ i64a.begin().appendByte(if hz.z then 1 else 0).result() ==== i64b.begin().append(hz.inBuffer).result()
    T(title) ~ { i64a.begin() += hz.z; i64a.result() } ==== i64b.begin().append(hz.inArray, 0, hz.bytes).result()
    T(title) ~ i64a.begin().result(hz.inBuffer) ==== f64.hash64(hz.inArray)
    T(title) ~ i64a.begin().result(hz.inArray, 0, hz.bytes) ==== f64.hash64(hz.inBuffer)

    T(title) ~ i64a.begin().appendByte(hb.b).result() ==== i64b.begin().append(hb.inBuffer).result()
    T(title) ~ { i64a.begin() += hb.b; i64a.result() } ==== i64b.begin().append(hb.inArray, 0, hb.bytes).result()
    T(title) ~ i64a.begin().result(hb.inBuffer) ==== f64.hash64(hb.inArray)
    T(title) ~ i64a.begin().result(hb.inArray, 0, hz.bytes) ==== f64.hash64(hb.inBuffer)

    T(title) ~ i64a.begin().appendChar((hs.s & 0xFFFF).toChar).result() ==== i64b.begin().append(hs.inBuffer).result()
    T(title) ~ { i64a.begin() += hs.s; i64a.result() } ==== i64b.begin().append(hs.inArray, 0, hs.bytes).result()
    T(title) ~ i64a.begin().result(hs.inBuffer) ==== f64.hash64(hs.inArray)
    T(title) ~ i64a.begin().result(hs.inArray, 0, hs.bytes) ==== f64.hash64(hs.inBuffer)

    T(title) ~ i64a.begin().appendChar(hc.c).result() ==== i64b.begin().append(hc.inBuffer).result()
    T(title) ~ { i64a.begin() += hc.c; i64a.result() } ==== i64b.begin().append(hc.inArray, 0, hc.bytes).result()
    T(title) ~ i64a.begin().result(hc.inBuffer) ==== f64.hash64(hc.inArray)
    T(title) ~ i64a.begin().result(hc.inArray, 0, hc.bytes) ==== f64.hash64(hc.inBuffer)

    T(title) ~ i64a.begin().appendInt(hi.i).result() ==== i64b.begin().append(hi.inBuffer).result()
    T(title) ~ { i64a.begin() += hi.i; i64a.result() } ==== i64b.begin().append(hi.inArray, 0, hi.bytes).result()
    T(title) ~ i64a.begin().result(hi.inBuffer) ==== f64.hash64(hi.inArray)
    T(title) ~ i64a.begin().result(hi.inArray, 0, hi.bytes) ==== f64.hash64(hi.inBuffer)

    T(title) ~ i64a.begin().appendLong(hl.l).result() ==== i64b.begin().append(hl.inBuffer).result()
    T(title) ~ { i64a.begin() += hl.l; i64a.result() } ==== i64b.begin().append(hl.inArray, 0, hl.bytes).result()
    T(title) ~ i64a.begin().result(hl.inBuffer) ==== f64.hash64(hl.inArray)
    T(title) ~ i64a.begin().result(hl.inArray, 0, hl.bytes) ==== f64.hash64(hl.inBuffer)

    T(title) ~ i64a.begin().appendInt(f2i(hf.f)).result() ==== i64b.begin().append(hf.inBuffer).result()
    T(title) ~ { i64a.begin() += hf.f; i64a.result() } ==== i64b.begin().append(hf.inArray, 0, hf.bytes).result()
    T(title) ~ i64a.begin().result(hf.inBuffer) ==== f64.hash64(hf.inArray)
    T(title) ~ i64a.begin().result(hf.inArray, 0, hf.bytes) ==== f64.hash64(hf.inBuffer)

    T(title) ~ i64a.begin().appendLong(d2l(hd.d)).result() ==== i64b.begin().append(hd.inBuffer).result()
    T(title) ~ { i64a.begin() += hd.d; i64a.result() } ==== i64b.begin().append(hd.inArray, 0, hd.bytes).result()
    T(title) ~ i64a.begin().result(hd.inBuffer) ==== f64.hash64(hd.inArray)
    T(title) ~ i64a.begin().result(hd.inArray, 0, hd.bytes) ==== f64.hash64(hd.inBuffer)

    T(title) ~ i64a.begin().result(hh.s, 0, hh.s.length) ==== hh.hashInto(i64b.begin()).result()
    T(title) ~ i64a.begin().append(hh.s, 0, hh.s.length).result() ==== i64b.begin().append(hh.inBuffer).result()
    T(title) ~ hh.hashInto(i64a.begin()).result() ==== i64b.begin().append(hh.inArray, 0, hh.bytes).result()
    T(title) ~ i64a.begin().result(hh.inBuffer) ==== f64.hash64(hh.inArray)
    T(title) ~ i64a.begin().result(hh.inArray, 0, hh.bytes) ==== f64.hash64(hh.inBuffer)
    T(title) ~ hh.hashInto(i64a.begin()).result() ==== f64.hash64(hh.s)
    T(title) ~ i64a.begin(81951752315L).result(hh.inBuffer) ==== f64.hash64(81951752315L, hh.s)

    T(title) ~ i64a.begin().result(hr.ab, hr.i0, hr.iN) ==== hr.hashInto(i64b.begin()).result()
    T(title) ~ i64a.begin().append(hr.ab, hr.i0, hr.iN).result() ==== i64b.begin().append(hr.inBuffer).result()
    T(title) ~ hr.hashInto(i64a.begin()).result() ==== i64b.begin().append(hr.inArray, 0, hr.bytes).result()
    T(title) ~ i64a.begin().result(hr.inBuffer) ==== f64.hash64(hr.inArray)
    T(title) ~ i64a.begin().result(hr.inArray, 0, hr.bytes) ==== f64.hash64(hr.inBuffer)
    T(title) ~ i64a.begin(4856718238451L).result(hr.inBuffer) ==== f64.hash64(4856718238451L, hr.ab, hr.i0, hr.iN)

    T(title) ~ hq.hashInto(i64a.begin()).result() ==== i64b.begin().append(hq.inBuffer).result()
    T(title) ~ hq.hashInto(i64a.begin()).result() ==== i64b.begin().append(hq.inArray, 0, hq.bytes).result()
    T(title) ~ i64a.begin().result(hq.inBuffer) ==== f64.hash64(hq.inArray)
    T(title) ~ i64a.begin().result(hq.inArray, 0, hq.bytes) ==== f64.hash64(hq.inBuffer)
    T(title) ~ i64a.begin(998342571158L).result(hq.inBuffer) ==== f64.hash64(998342571158L, hq.inBuffer)

    T(title) ~ hx.hashInto(i64a.begin()).result() ==== i64b.begin().append(hx.inBuffer).result()
    T(title) ~ hx.hashInto(i64a.begin()).result() ==== i64b.begin().append(hx.inArray, 0, hx.bytes).result()
    T(title) ~ i64a.begin().result(hx.inBuffer) ==== f64.hash64(hx.inArray)
    T(title) ~ i64a.begin().result(hx.inArray, 0, hx.bytes) ==== f64.hash64(hx.inBuffer)

    hx.hashInto(i64a.begin())
    T(title) ~ hx.hashInto(i64a.copy).result() ==== hx.hashInto(i64a).result()


  def hash128test(
    h: (HZ, HB, HS, HC, HI, HL, HF, HD, Hstr, Harr, Hbb, Hiter),
    title: String,
    i128a: IncrementalHash[HashCode128, HashCode128],
    i128b: IncrementalHash[HashCode128, HashCode128],
    f128: FullHash128
  ): Unit =
    import java.lang.Float.{floatToRawIntBits => f2i}
    import java.lang.Double.{doubleToRawLongBits => d2l}
    val (hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx) = h

    T(title) ~ i128a.begin().appendByte(if hz.z then 1 else 0).result() ==== i128b.begin().append(hz.inBuffer).result()
    T(title) ~ { i128a.begin() += hz.z; i128a.result() } ==== i128b.begin().append(hz.inArray, 0, hz.bytes).result()
    T(title) ~ i128a.begin().result(hz.inBuffer) ==== f128.hash128(hz.inArray)
    T(title) ~ i128a.begin().result(hz.inArray, 0, hz.bytes) ==== f128.hash128(hz.inBuffer)

    T(title) ~ i128a.begin().appendByte(hb.b).result() ==== i128b.begin().append(hb.inBuffer).result()
    T(title) ~ { i128a.begin() += hb.b; i128a.result() } ==== i128b.begin().append(hb.inArray, 0, hb.bytes).result()
    T(title) ~ i128a.begin().result(hb.inBuffer) ==== f128.hash128(hb.inArray)
    T(title) ~ i128a.begin().result(hb.inArray, 0, hz.bytes) ==== f128.hash128(hb.inBuffer)

    T(title) ~ i128a.begin().appendChar((hs.s & 0xFFFF).toChar).result() ==== i128b.begin().append(hs.inBuffer).result()
    T(title) ~ { i128a.begin() += hs.s; i128a.result() } ==== i128b.begin().append(hs.inArray, 0, hs.bytes).result()
    T(title) ~ i128a.begin().result(hs.inBuffer) ==== f128.hash128(hs.inArray)
    T(title) ~ i128a.begin().result(hs.inArray, 0, hs.bytes) ==== f128.hash128(hs.inBuffer)

    T(title) ~ i128a.begin().appendChar(hc.c).result() ==== i128b.begin().append(hc.inBuffer).result()
    T(title) ~ { i128a.begin() += hc.c; i128a.result() } ==== i128b.begin().append(hc.inArray, 0, hc.bytes).result()
    T(title) ~ i128a.begin().result(hc.inBuffer) ==== f128.hash128(hc.inArray)
    T(title) ~ i128a.begin().result(hc.inArray, 0, hc.bytes) ==== f128.hash128(hc.inBuffer)

    T(title) ~ i128a.begin().appendInt(hi.i).result() ==== i128b.begin().append(hi.inBuffer).result()
    T(title) ~ { i128a.begin() += hi.i; i128a.result() } ==== i128b.begin().append(hi.inArray, 0, hi.bytes).result()
    T(title) ~ i128a.begin().result(hi.inBuffer) ==== f128.hash128(hi.inArray)
    T(title) ~ i128a.begin().result(hi.inArray, 0, hi.bytes) ==== f128.hash128(hi.inBuffer)

    T(title) ~ i128a.begin().appendLong(hl.l).result() ==== i128b.begin().append(hl.inBuffer).result()
    T(title) ~ { i128a.begin() += hl.l; i128a.result() } ==== i128b.begin().append(hl.inArray, 0, hl.bytes).result()
    T(title) ~ i128a.begin().result(hl.inBuffer) ==== f128.hash128(hl.inArray)
    T(title) ~ i128a.begin().result(hl.inArray, 0, hl.bytes) ==== f128.hash128(hl.inBuffer)

    T(title) ~ i128a.begin().appendInt(f2i(hf.f)).result() ==== i128b.begin().append(hf.inBuffer).result()
    T(title) ~ { i128a.begin() += hf.f; i128a.result() } ==== i128b.begin().append(hf.inArray, 0, hf.bytes).result()
    T(title) ~ i128a.begin().result(hf.inBuffer) ==== f128.hash128(hf.inArray)
    T(title) ~ i128a.begin().result(hf.inArray, 0, hf.bytes) ==== f128.hash128(hf.inBuffer)

    T(title) ~ i128a.begin().appendLong(d2l(hd.d)).result() ==== i128b.begin().append(hd.inBuffer).result()
    T(title) ~ { i128a.begin() += hd.d; i128a.result() } ==== i128b.begin().append(hd.inArray, 0, hd.bytes).result()
    T(title) ~ i128a.begin().result(hd.inBuffer) ==== f128.hash128(hd.inArray)
    T(title) ~ i128a.begin().result(hd.inArray, 0, hd.bytes) ==== f128.hash128(hd.inBuffer)

    T(title) ~ i128a.begin().result(hh.s, 0, hh.s.length) ==== hh.hashInto(i128b.begin()).result()
    T(title) ~ hh.hashInto(i128a.begin()).result() ==== i128b.begin().append(hh.inArray, 0, hh.bytes).result()
    T(title) ~ i128a.begin().append(hh.s, 0, hh.s.length).result() ==== i128b.begin().append(hh.inBuffer).result()
    T(title) ~ i128a.begin().result(hh.inBuffer) ==== f128.hash128(hh.inArray)
    T(title) ~ i128a.begin().result(hh.inArray, 0, hh.bytes) ==== f128.hash128(hh.inBuffer)
    T(title) ~ hh.hashInto(i128a.begin()).result() ==== f128.hash128(hh.s)
    T(title) ~ i128a.begin(HashCode128(8197531982571L, 818758916852L)).result(hh.inBuffer) ==== f128.hash128(8197531982571L, 818758916852L, hh.s)

    T(title) ~ i128a.begin().result(hr.ab, hr.i0, hr.iN) ==== hr.hashInto(i128b.begin()).result()
    T(title) ~ i128a.begin().append(hr.ab, hr.i0, hr.iN).result() ==== i128b.begin().append(hr.inBuffer).result()
    T(title) ~ hr.hashInto(i128a.begin()).result() ==== i128b.begin().append(hr.inArray, 0, hr.bytes).result()
    T(title) ~ i128a.begin().result(hr.inBuffer) ==== f128.hash128(hr.inArray)
    T(title) ~ i128a.begin().result(hr.inArray, 0, hr.bytes) ==== f128.hash128(hr.inBuffer)
    T(title) ~ i128a.begin(HashCode128(18957L, 9581L)).result(hr.inArray, 0, hr.bytes) ==== f128.hash128(18957L, 9581L, hr.ab, hr.i0, hr.iN)

    T(title) ~ hq.hashInto(i128a.begin()).result() ==== i128b.begin().append(hq.inBuffer).result()
    T(title) ~ hq.hashInto(i128a.begin()).result() ==== i128b.begin().append(hq.inArray, 0, hq.bytes).result()
    T(title) ~ i128a.begin().result(hq.inBuffer) ==== f128.hash128(hq.inArray)
    T(title) ~ i128a.begin().result(hq.inArray, 0, hq.bytes) ==== f128.hash128(hq.inBuffer)
    T(title) ~ i128a.begin(HashCode128(89247L, 8923467981375L)).result(hq.inBuffer) ==== f128.hash128(89247L, 8923467981375L, hq.inBuffer)

    T(title) ~ hx.hashInto(i128a.begin()).result() ==== i128b.begin().append(hx.inBuffer).result()
    T(title) ~ hx.hashInto(i128a.begin()).result() ==== i128b.begin().append(hx.inArray, 0, hx.bytes).result()
    T(title) ~ i128a.begin().result(hx.inBuffer) ==== f128.hash128(hx.inArray)
    T(title) ~ i128a.begin().result(hx.inArray, 0, hx.bytes) ==== f128.hash128(hx.inBuffer)

    hx.hashInto(i128a.begin())
    T(title) ~ hx.hashInto(i128a.copy).result() ==== hx.hashInto(i128a).result()


  @Test
  def hashTest(): Unit =
    val r = Prng(812795113489L)

    val x32a = new XxHash32
    val x32b = MakeHasher.x32
    val x64a = new XxHash64
    val x64b = MakeHasher.x64
    val m32a = new MurmurHash32
    val m32b = MakeHasher.m32
    val m128a = new MurmurHash128
    val m128b = MakeHasher.m128
    val s32a = new SumHash32
    val s32b = MakeHasher.s32
    val s64a = new SumHash64
    val s64b = MakeHasher.s64
    val o32a = new XorHash32
    val o32b = MakeHasher.o32
    val o64a = new XorHash64
    val o64b = MakeHasher.o64

    nFor(200) { n =>
      def t(x: String) = s"Iteration $n of $x"

      val hz = HZ(r.Z)
      val hb = HB(r.B)
      val hs = HS(r.S)
      val hc = HC(r.C)
      val hi = HI(r.I)
      val hl = HL(r.L)
      val hf = HF(r.F)
      val hd = HD(r.D)

      given Prng = r

      val hh = Hstr(
        4.roll match
          case 1 => r webString 64.roll
          case 2 => r textString 64.roll
          case 3 => r asciiString 64.roll
          case _ => r validString 64.roll
      )
      T ~ hh.inBuffer.array =**= hh.inArray

      val hr = {
        val a = r.arrayB(64.roll)
        val i = r % a.length
        val j = r % (1 + a.length - i)
        Harr(a, i, j)
      }
      T ~ hr.inBuffer.array =**= hr.inArray

      val hq = Hbb(ByteBuffer.wrap(r.arrayB(64.roll)).order(ByteOrder.LITTLE_ENDIAN))
      T ~ hq.inBuffer.array =**= hq.inArray

      val hx = randomHs(24.roll, 64)(r)
      T ~ hx.inBuffer.array =**= hx.inArray

      hash32test((hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx), t("x32"), x32a, x32b, XxHash)
      hash32test((hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx), t("m32"), m32a, m32b, MurmurHash)
      hash32test((hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx), t("s32"), s32a, s32b, SumHash)
      hash32test((hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx), t("o32"), o32a, o32b, XorHash)

      hash64test((hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx), t("x64"), x64a, x64b, XxHash)
      hash64test((hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx), t("s64"), s64a, s64b, SumHash)
      hash64test((hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx), t("o64"), o64a, o64b, XorHash)

      hash128test((hz, hb, hs, hc, hi, hl, hf, hd, hh, hr, hq, hx), t("m128"), m128a, m128b, MurmurHash)

      T ~ { (hx.hashInto(x32a.begin()).result(), hx.hashInto(x64a.begin()).result()) } ==== hx.hashInto(PairHash.of(x32b, x64b)).result()
      T ~ {
        (hx.hashInto(x32a.begin()).result(), hx.hashInto(x64a.begin()).result(), hx.hashInto(m32a.begin()).result())
      } ==== hx.hashInto(TrioHash.of(x32b, x64b, m32b)).result()
      T ~ {
        (
          hx.hashInto(x32a.begin()).result(),
          hx.hashInto(x64a.begin()).result(),
          hx.hashInto(m32a.begin()).result(),
          hx.hashInto(m128a.begin()).result()
        )
      } ==== hx.hashInto(QuadHash.of(x32b, x64b, m32b, m128b)).result()
      T ~ hx.hashInto(x32a.begin(82351)).result() ==== hx.hashInto(PreseededHash.of(82351, x32b).begin()).result()
    }


  @Test
  def functionsTest(): Unit =
    import NumericFunctions._
    // Expected value computed to 20 digits by Mathematica 12.2
    T ~ log2(2) ==== 1.0
    T ~ log2(65536) =~~= 16.0
    T ~ entropy(0.5) ==== 0.5
    T ~ entropy(0.1) =~~= 0.33219280948873623479
    T ~ lnGamma(0.017)   =~~= 4.0649650153354890666
    T ~ lnGamma(1)       ==== 0.0
    T ~ lnGamma(4.1779)  =~~= 2.0196407862685896019
    T ~ lnGamma(41.6)    =~~= 112.54585857443697738
    T ~ lnGamma(92.222)  =~~= 323.66639657465140139
    T ~ lnGamma(19581.1) =~~= 173921.57578088239597
    T ~ gamma(-0.9991) =~~= -1111.5351665121232688
    T ~ gamma(-0.3)    =~~= -4.3268511088251926189
    T ~ gamma(0.017)   =~~= 58.262870179930133017
    T ~ gamma(1)       ==== 1.0
    T ~ gamma(4.1779)  =~~= 7.5356175501277003717
    T ~ gamma(12)      ==== 39916800.0
    T ~ gamma(41.6)    =~~= 7.5517106270987740135e48
    T ~ gamma(92.222)  =~~= 3.6857850985958019759e140
    T ~ lnGammaRat(17.2157, 5.191) =~~= 27.808277449776492112
    T ~ gammaRat(17.2157, 5.191) =~~= 1.1939371003966004721e12
    T ~ lnBeta(17.2157, 5.191) ==== lnBeta(5.191, 17.2157)
    T ~ lnBeta(17.2157, 5.191) =~~= -11.884089449418134043
    T ~ beta(17.2157, 5.191) ==== beta(5.191, 17.2157)
    T ~ beta(4.2157, 2.1) =~~= 0.039940384587432869204
    T ~ beta(16, 12) ==== 1.0/208606320
    T ~ icdfNormal(-0.01) ==== Double.NaN
    T ~ icdfNormal(0)     ==== Double.NegativeInfinity
    T ~ icdfNormal(3e-17) =~~= -8.3652256215814373860
    T ~ icdfNormal(1e-11) =~~= -6.7060231554951362873
    T ~ icdfNormal(2e-11) =~~= -6.6040775904056342687
    T ~ icdfNormal(0.217) =~~= -0.78236516485538705104
    T ~ icdfNormal(0.487) =~~= -0.032591936696663045330
    T ~ icdfNormal(0.501) =~~= 0.0025066308995717640054
    T ~ icdfNormal(0.999) =~~= 3.0902323061678135415
    T ~ icdfNormal(1)     =~~= Double.PositiveInfinity
    T ~ icdfNormal(1.001) =~~= Double.NaN
    T ~ erfInv(-0.999) =~~= -2.3267537655135246706
    T ~ erfInv(1.1e-7) =~~= 9.7484961799803690312e-8
    T ~ erfInv(0.7271) =~~= 0.77527795939043935376
    T ~ erfcInv(1e-6)  =~~= erfInv(0.999999)
    T ~ cdfNormal(-40) ==== 0.0
    {
      given Approximation[Double] = Approximation.OfDouble(1e-12, 1e-86, 1e-86)
      T ~ cdfNormal(-18) ==== 9.7409489189371504826e-73
    }
    T ~ cdfNormal(-4)     =~~= 0.000031671241833119921254
    T ~ cdfNormal(-0.715) =~~= 0.23730452163984734223
    T ~ cdfNormal(0.1111) =~~= 0.54423147562860594608
    T ~ cdfNormal(3.3333) =~~= 0.99957088825464711767
    T ~ cdfNormal(5)      =~~= 0.99999971334842812081
    T ~ cdfNormal(9)      ==== 1.0
    T ~ erf(-2.1919) =~~= -0.99806358259078677079
    T ~ erf(0.71571) =~~= 0.68854168727754878624
    T ~ erfc(0.5715) =~~= 1 - erf(0.5715)
    T ~ cdfStudentT( 1, -31) =~~= 0.010264501477747753863
    T ~ cdfStudentT( 1,  -2) =~~= 0.14758361765043327418
    T ~ cdfStudentT( 1, 0.3) =~~= 0.59277357907774234356
    T ~ cdfStudentT( 1,   5) =~~= 0.93716704181099881619
    T ~ cdfStudentT( 1, 111) =~~= 0.99713242094575090823
    T ~ cdfStudentT( 2, 0.3) =~~= 0.60375716957991119835
    T ~ cdfStudentT( 3, 0.3) =~~= 0.60811835398004048270
    T ~ cdfStudentT( 8, 4.7) =~~= 0.99922926941847198643
    T ~ cdfStudentT(11, 4.7) =~~= 0.99967492880381236615
    T ~ cdfStudentT(21, 1.7) =~~= 0.94805198599193684428
    T ~ cdfStudentT(47, 1.7) =~~= 0.95212972951603268371
    T ~ cdfStudentT(99,2e-3) =~~= 0.50079587174090755526
    T ~ cdfStudentT(99999,1) =~~= 0.84134353620584639525
    T ~ cdfStudentT(99,-5.3) =~~= 3.5169564560858694488e-7
    T ~ icdfStudentT( 1, 0.01) =~~= -31.820515953773958039
    T ~ icdfStudentT( 1, 0.51) =~~= 0.031426266043351147819
    T ~ icdfStudentT( 2, 0.01) =~~= -6.9645567342832741871
    T ~ icdfStudentT( 2, 0.51) =~~= 0.028289929799333551690
    {
      given Approximation[Double] = Approximation.OfDouble(1e-5, 1e-5, 1e-10)
      T ~ icdfStudentT( 4, 0.01) =~~= -3.7469473879791968366
    }
    T ~ icdfStudentT( 4, 0.51) =~~= 0.026670618302639006373
    {
      given Approximation[Double] = Approximation.OfDouble(1e-7, 1e-7, 1e-14)
      T ~ icdfStudentT( 8, 0.01) =~~= -2.8964594477096223339
    }
    T ~ icdfStudentT( 8, 0.51) =~~= 0.025863148428696671896
    T ~ icdfStudentT(99, 0.99) =~~= 2.3646058617869433839
    T ~ icdfStudentT(999, 0.6) =~~= 0.25341458333036736702
    T ~ icdfStudentT(99, 1e-9) =~~= -6.6055176089570498209

    T ~ regularizedLowerIncompleteGamma(0.07, 1e-20) =~~= 0.041290911908887717448
    T ~ regularizedLowerIncompleteGamma(0.07, 0.033) =~~= 0.81511672149112283015
    T ~ regularizedLowerIncompleteGamma(0.07, 4)     =~~= 0.99969382652440138059
    {
      given Approximation[Double] = Approximation.OfDouble(1e-9, 1e-12, 1e-21)
      T ~ regularizedLowerIncompleteGamma(0.47, 1e-20) =~~= 4.4951703002588139673e-10
    }
    T ~ regularizedLowerIncompleteGamma(0.47, 4)     =~~= 0.99580336476430854842
    T ~ regularizedLowerIncompleteGamma(0.47, 9)     =~~= 0.99998060282772796934
    {
      given Approximation[Double] = Approximation.OfDouble(1e-9, 1e-12, 1e-21)
      T ~ regularizedLowerIncompleteGamma(13.2, 0.883) =~~= 8.1273845027969892735e-12
      T ~ regularizedLowerIncompleteGamma(13.2, 2.583) =~~= 2.4160993315339546571e-6
    }
    T ~ regularizedLowerIncompleteGamma(13.2, 3.142) =~~= 0.000019232466583370270554
    T ~ regularizedLowerIncompleteGamma(13.2, 11.18) =~~= 0.31063267851575332291
    T ~ regularizedLowerIncompleteGamma(13.2, 23)    =~~= 0.98955644244159926579
    T ~ regularizedLowerIncompleteGamma(1201, 1135)  =~~= 0.026750925047326441730
    T ~ regularizedLowerIncompleteGamma(1201, 1203)  =~~= 0.52682710399911528314
    T ~ regularizedLowerIncompleteGamma(1201, 1341)  =~~= 0.99995233982626005229
    T ~ (regularizedUpperIncompleteGamma(0.07, 0.033) + regularizedLowerIncompleteGamma(0.07, 0.033)) =~~= 1.0
    T ~ (regularizedUpperIncompleteGamma(0.47, 4.000) + regularizedLowerIncompleteGamma(0.47, 4.000)) =~~= 1.0
    T ~ (regularizedUpperIncompleteGamma(13.2, 11.18) + regularizedLowerIncompleteGamma(13.2, 11.18)) =~~= 1.0
    T ~ (regularizedUpperIncompleteGamma(1201, 1341 ) + regularizedLowerIncompleteGamma(1201, 1341 )) =~~= 1.0
    T ~ cdfChiSq(1.5, 2.19) =~~= 0.76688085944971560316
    T ~ cdfChiSq(18, 12.91) =~~= 0.20310828640315452276

    T ~ regularizedIncompleteBeta(0.07, 0.02)(0.071) =~~= 0.18593534106366527947
    T ~ regularizedIncompleteBeta(0.07, 0.02)(0.371) =~~= 0.21397610838266902617
    T ~ regularizedIncompleteBeta(0.07, 0.02)(0.914) =~~= 0.25664695068689865766
    T ~ regularizedIncompleteBeta(0.07, 0.22)(0.071) =~~= 0.64617485077966580454
    T ~ regularizedIncompleteBeta(0.07, 0.22)(0.371) =~~= 0.73952089487991753006
    T ~ regularizedIncompleteBeta(0.07, 0.22)(0.914) =~~= 0.85414522525020009779
    T ~ regularizedIncompleteBeta(1.41, 0.22)(0.071) =~~= 0.0042908987496014291472
    T ~ regularizedIncompleteBeta(1.41, 0.22)(0.371) =~~= 0.052269750631912289345
    T ~ regularizedIncompleteBeta(1.41, 0.22)(0.914) =~~= 0.35825787195523729569
    T ~ regularizedIncompleteBeta(1.41, 1.47)(0.071) =~~= 0.038150936089382751716
    T ~ regularizedIncompleteBeta(1.41, 1.47)(0.371) =~~= 0.35638900943325702726
    T ~ regularizedIncompleteBeta(1.41, 1.47)(0.914) =~~= 0.95868177007335130767
    T ~ regularizedIncompleteBeta(9.41, 7.77)(0.071) =~~= 1.1715491684031259097e-7
    T ~ regularizedIncompleteBeta(9.41, 7.77)(0.371) =~~= 0.069515424418849154010
    T ~ regularizedIncompleteBeta(9.41, 7.77)(0.914) =~~= 0.99996117847935196603
    T ~ regularizedIncompleteBeta(94.1, 94.2)(0.384) =~~= 0.00064973938015974858055
    T ~ regularizedIncompleteBeta(94.1, 94.2)(0.501) =~~= 0.51384518267594726173
    T ~ regularizedIncompleteBeta(94.1, 94.2)(0.599) =~~= 0.99700121042712086905
    T ~ regularizedIncompleteBeta(500, 1)(0.98)      =~~= 0.000041023985145472588934
    T ~ cdfFDist(5, 7)(14.142) =~~= 0.99847312214627792997
    T ~ cdfFDist(18, 11)(0.29) =~~= 0.0097966801396012063383


  @Test
  def extensionsTest(): Unit =
    val b: Byte = -8
    val b2: Byte = 32
    val bmin = Byte.MinValue
    T ~ (b +# b)                 ==== -16  --: typed[Byte]
    T ~ (b +# (-125: Byte))      ==== -128 --: typed[Byte]
    T ~ (99.toByte +# 99.toByte) ==== 127  --: typed[Byte]
    T ~ (b -# b)                 ==== 0    --: typed[Byte]
    T ~ (b -# (125: Byte))       ==== -128 --: typed[Byte]
    T ~ (125.toByte -# b)        ==== 127  --: typed[Byte]
    T ~ (b *# b)                 ==== 64   --: typed[Byte]
    T ~ (b *# (40: Byte))        ==== -128 --: typed[Byte]
    T ~ (b *# (-30: Byte))       ==== 127  --: typed[Byte]
    T ~ (b /# (2: Byte))         ==== -4   --: typed[Byte]
    T ~ (b /# (0: Byte))         ==== Byte.MinValue
    T ~ (b2 /# (0: Byte))        ==== Byte.MaxValue
    T ~ ((0: Byte) /# (0: Byte)) ==== 0
    T ~ ((-128:Byte)/#(-1:Byte)) ==== 127  --: typed[Byte]
    T ~ (b %# (3: Byte))         ==== -2   --: typed[Byte]
    T ~ (b %# (0: Byte))         ==== 0
    T ~ (b +! b)                 ==== -16  --: typed[Byte]
    T ~ (b +! (-125: Byte))      ==== thrown[ArithmeticException]
    T ~ (b -! b)                 ==== 0    --: typed[Byte]
    T ~ (b -! (125: Byte))       ==== thrown[ArithmeticException]
    T ~ (b *! b)                 ==== 64   --: typed[Byte]
    T ~ (b2 *! b2)               ==== thrown[ArithmeticException]
    T ~ (b2 /! b)                ==== -4   --: typed[Byte]
    T ~ ((-128:Byte)/!(-1:Byte)) ==== thrown[ArithmeticException]
    T ~ b.clamp(-12, -4)   ==== -8         --: typed[Byte]
    T ~ b.clamp(13, 100)   ==== 13         --: typed[Byte]
    T ~ b.clamp(-99, -9)   ==== -9         --: typed[Byte]
    T ~ b.clamp(55, -99)   ==== 55
    T ~ b.checkIn(-12, -4) ==== -8         --: typed[Byte]
    T ~ b.checkIn(13, 100) ==== thrown[ArithmeticException]        
    T ~ b.in(-12, 4)       ==== true
    T ~ b.in(13, 100)      ==== false
    T ~ b.in(-99, -9)      ==== false
    T ~ b.in(44, -12)      ==== false
    T ~ b.sq               ==== 64.0       --: typed[Double]
    T ~ b.sign             ==== -1         --: typed[Int]
    T ~ (-b).toByte.sign   ==== 1
    T ~ (b/10).toByte.sign ==== 0
    T ~ b.u                ==== b          --: typed[UByte]
    T ~ b.unsigned         ==== b          --: typed[UByte]
    T ~ b.toUByte          ==== b          --: typed[UByte]
    T ~ b.toUInt           ==== 248        --: typed[UInt]
    T ~ b.toULong          ==== 248L       --: typed[ULong]
    T ~ b.clampNeg         ==== 8          --: typed[Byte]
    T ~ bmin.clampNeg      ==== 127        --: typed[Byte]
    T ~ b.clampToUByte     ==== 0          --: typed[UByte]
    T ~ b2.clampToUByte    ==== b2         --: typed[UByte]
    T ~ b.clampToChar      ==== '\u0000'   --: typed[Char]
    T ~ b2.clampToChar     ==== ' '        --: typed[Char]
    T ~ b.clampToUInt      ==== 0          --: typed[UInt]
    T ~ b2.clampToUInt     ==== b2         --: typed[UInt]
    T ~ b.clampToULong     ==== 0          --: typed[ULong]
    T ~ b2.clampToULong    ==== b2         --: typed[ULong]
    T ~ b.checkedNeg       ==== 8          --: typed[Byte]
    T ~ bmin.checkedNeg    ==== thrown[ArithmeticException]
    T ~ b2.checkedToUByte  ==== b2         --: typed[UByte]
    T ~ b.checkedToUByte   ==== thrown[ArithmeticException]
    T ~ b2.checkedToChar   ==== b2.toChar  --: typed[Char]
    T ~ b.checkedToChar    ==== thrown[ArithmeticException]
    T ~ b2.checkedToUInt   ==== b2         --: typed[UInt]
    T ~ b.checkedToUInt    ==== thrown[ArithmeticException]
    T ~ b2.checkedToULong  ==== b2         --: typed[ULong]
    T ~ b.checkedToULong   ==== thrown[ArithmeticException]
    T ~ b.hexString        ==== "F8"
    T ~ b.hiHexString      ==== "F8"
    T ~ b.loHexString      ==== "f8"

    val s: Short = -88
    val s2: Short = -888
    val s3: Short = 200
    val s4: Short = 2000
    val smin = Short.MinValue
    T ~ (s +# s)                        ==== -176   --: typed[Short]
    T ~ (s +# (-32760: Short))          ==== -32768 --: typed[Short]
    T ~ (29999.toShort +# 9999.toShort) ==== 32767  --: typed[Short]
    T ~ (s -# s)                        ==== 0      --: typed[Short]
    T ~ (s -# (32760: Short))           ==== -32768 --: typed[Short]
    T ~ (32760.toShort -# s)            ==== 32767  --: typed[Short]
    T ~ (s *# s)                        ==== 7744   --: typed[Short]
    T ~ (s *# (400: Short))             ==== -32768 --: typed[Short]
    T ~ (s *# (-400: Short))            ==== 32767  --: typed[Short]
    T ~ (s /# (2: Short))               ==== -44    --: typed[Short]
    T ~ (Short.MinValue /# (-1: Short)) ==== 32767  --: typed[Short]
    T ~ (s /# (0: Short))               ==== Short.MinValue
    T ~ (s3 /# (0: Short))              ==== Short.MaxValue
    T ~ ((0: Short) /# (0: Short))      ==== 0
    T ~ (s %# (3: Short))               ==== -1     --: typed[Short]
    T ~ (s %# (0: Short))               ==== 0
    T ~ (s +! s)                        ==== -176   --: typed[Short]
    T ~ (s +! (-32760: Short))          ==== thrown[ArithmeticException]
    T ~ (s -! s2)                       ==== 800    --: typed[Short]
    T ~ (s -! (32760: Short))           ==== thrown[ArithmeticException]
    T ~ (s *! s)                        ==== 7744   --: typed[Short]
    T ~ (s2 *! s4)                      ==== thrown[ArithmeticException]
    T ~ (s /! (2: Short))               ==== -44    --: typed[Short]
    T ~ (Short.MinValue /! (-1: Short)) ==== thrown[ArithmeticException]
    T ~ s.clamp(-915, -4)    ==== -88               --: typed[Short]
    T ~ s.clamp(-44, 333)    ==== -44               --: typed[Short]
    T ~ s.clamp(-99, -91)    ==== -91               --: typed[Short]
    T ~ s.clamp(333, -44)    ==== 333               --: typed[Short]
    T ~ s.checkIn(-915, -4)  ==== -88               --: typed[Short]
    T ~ s.checkIn(-44, 333)  ==== thrown[ArithmeticException]
    T ~ s.in(-915, -4)       ==== true
    T ~ s.in(-44, 333)       ==== false
    T ~ s.in(-99, -91)       ==== false
    T ~ s.in(-4, -915)       ==== false
    T ~ s.sq                 ==== 7744.0            --: typed[Double]
    T ~ s.sign               ==== -1                --: typed[Int]
    T ~ (-s).toShort.sign    ==== 1
    T ~ (s/100).toShort.sign ==== 0
    T ~ s.toUByte            ==== UByte(168)        --: typed[UByte]
    T ~ s.toUInt             ==== 65448             --: typed[UInt]
    T ~ s.toULong            ==== 65448L            --: typed[ULong]
    T ~ s.clampNeg           ==== 88                --: typed[Short]
    T ~ smin.clampNeg        ==== Short.MaxValue    --: typed[Short]
    T ~ s.clampToByte        ==== s                 --: typed[Byte]
    T ~ s2.clampToByte       ==== Byte.MinValue     --: typed[Byte]
    T ~ s3.clampToByte       ==== Byte.MaxValue     --: typed[Byte]
    T ~ s.clampToUByte       ==== 0                 --: typed[UByte]
    T ~ s3.clampToUByte      ==== UByte(200)        --: typed[UByte]
    T ~ s4.clampToUByte      ==== UByte.MaxValue    --: typed[UByte]
    T ~ s.clampToChar        ==== 0                 --: typed[Char]
    T ~ s4.clampToChar       ==== '\u07D0'          --: typed[Char]
    T ~ s.clampToUInt        ==== 0                 --: typed[UInt]
    T ~ s4.clampToUInt       ==== 2000              --: typed[UInt]
    T ~ s.clampToULong       ==== 0                 --: typed[ULong]
    T ~ s4.clampToULong      ==== 2000              --: typed[ULong]
    T ~ s.checkedNeg         ==== 88                --: typed[Short]
    T ~ smin.checkedNeg      ==== thrown[ArithmeticException]
    T ~ s.checkedToByte      ==== s                 --: typed[Byte]
    T ~ s2.checkedToByte     ==== thrown[ArithmeticException]
    T ~ s3.checkedToUByte    ==== UByte(200)        --: typed[UByte]
    T ~ s.checkedToUByte     ==== thrown[ArithmeticException]
    T ~ s4.checkedToChar     ==== '\u07D0'          --: typed[Char]
    T ~ s.checkedToChar      ==== thrown[ArithmeticException]
    T ~ s4.checkedToUInt     ==== s4                --: typed[UInt]
    T ~ s.checkedToUInt      ==== thrown[ArithmeticException]
    T ~ s4.checkedToULong    ==== s4                --: typed[ULong]
    T ~ s.checkedToULong     ==== thrown[ArithmeticException]
    T ~ s.hexString          ==== "FFA8"
    T ~ s.hiHexString        ==== "FFA8"
    T ~ s.loHexString        ==== "ffa8"
    T ~ (1: Short).hexString ==== "0001"

    val c = 'n'
    val c2 = '\u00EE'
    val c3 = '\u2072'
    val c4 = '\uABCD'
    T ~ c.clamp('a', 'w')   ==== 'n'        --: typed[Char]
    T ~ c.clamp('A', 'W')   ==== 'W'        --: typed[Char]
    T ~ c.clamp('q', 'z')   ==== 'q'        --: typed[Char]
    T ~ c.clamp('z', 'A')   ==== 'z'        --: typed[Char]
    T ~ c.checkIn('a', 'w') ==== 'n'        --: typed[Char]
    T ~ c.checkIn('A', 'W') ==== thrown[ArithmeticException]
    T ~ c.in('a', 'w')      ==== true
    T ~ c.in('A', 'W')      ==== false
    T ~ c.in('q', 'z')      ==== false
    T ~ c.in('w', 'a')      ==== false
    T ~ c.toUByte           ==== c.toByte   --: typed[UByte]
    T ~ c.toUInt            ==== c.toInt    --: typed[UInt]
    T ~ c.toULong           ==== c.toLong   --: typed[ULong]
    T ~ c.clampToByte       ==== 110        --: typed[Byte]
    T ~ c2.clampToByte      ==== 127        --: typed[Byte]
    T ~ c.clampToUByte      ==== 110        --: typed[UByte]
    T ~ c2.clampToUByte     ==== UByte(238) --: typed[UByte]
    T ~ c3.clampToUByte     ==== UByte(255) --: typed[UByte]
    T ~ c.clampToShort      ==== 110        --: typed[Short]
    T ~ c3.clampToShort     ==== 0x2072     --: typed[Short]
    T ~ c4.clampToShort     ==== 0x7FFF     --: typed[Short]
    T ~ c.clampToUInt       ==== UInt(110)  --: typed[UInt]
    T ~ c.clampToULong      ==== ULong(110) --: typed[ULong]
    T ~ c.checkedToByte     ==== 110        --: typed[Byte]
    T ~ c2.checkedToByte    ==== thrown[ArithmeticException]
    T ~ c.checkedToUByte    ==== 110        --: typed[UByte]
    T ~ c3.checkedToUByte   ==== thrown[ArithmeticException]
    T ~ c.checkedToShort    ==== 110        --: typed[Short]
    T ~ c4.checkedToShort   ==== thrown[ArithmeticException]
    T ~ c.hexString         ==== "006E"
    T ~ c.hiHexString       ==== "006E"
    T ~ c.loHexString       ==== "006e"

    val i = -8888
    val i2 = -88
    val i3 = -888888
    val i4 = 200
    val i5 = 2000
    val i6 = 200000
    T ~ (i +# i)                       ==== -17776       --: typed[Int]
    T ~ (i +# -2147480000)             ==== Int.MinValue --: typed[Int]
    T ~ (2000000000 +# 150000000)      ==== Int.MaxValue --: typed[Int]
    T ~ (i -# i)                       ==== 0            --: typed[Int]
    T ~ (i -# 2147480000)              ==== Int.MinValue --: typed[Int]
    T ~ (2147480000 -# i)              ==== Int.MaxValue --: typed[Int]
    T ~ (i *# i)                       ==== 78996544     --: typed[Int]
    T ~ (i *# 241617)                  ==== Int.MinValue --: typed[Int]
    T ~ (i *# (-241617))               ==== Int.MaxValue --: typed[Int]
    T ~ (Int.MinValue *# Int.MinValue) ==== Int.MaxValue --: typed[Int]
    T ~ (Int.MaxValue *# Int.MinValue) ==== Int.MinValue --: typed[Int]
    T ~ (Int.MinValue *# Int.MaxValue) ==== Int.MinValue --: typed[Int]
    T ~ (Int.MaxValue *# Int.MaxValue) ==== Int.MaxValue --: typed[Int]
    T ~ (i /# 2)                       ==== -4444        --: typed[Int]
    T ~ (Int.MinValue /# -1)           ==== Int.MaxValue --: typed[Int]
    T ~ (i /# 0)                       ==== Int.MinValue
    T ~ (i4 /# 0)                      ==== Int.MaxValue
    T ~ (0 /# 0)                       ==== 0
    T ~ (i %# 3)                       ==== -2           --: typed[Int]
    T ~ (i %# 0)                       ==== 0
    T ~ (i +! i)                       ==== -17776       --: typed[Int]
    T ~ (2000000000 +! 150000000)      ==== thrown[ArithmeticException]
    T ~ (i -! i2)                      ==== -8800        --: typed[Int]
    T ~ (2147480000 -! i)              ==== thrown[ArithmeticException]
    T ~ (i *! i)                       ==== 78996544     --: typed[Int]
    T ~ (i *! 241617)                  ==== thrown[ArithmeticException]
    T ~ (i /! 2)                       ==== -4444        --: typed[Int]
    T ~ (Int.MinValue /! -1)           ==== thrown[ArithmeticException]
    T ~ i.clamp(-9999, -7777)   ==== -8888
    T ~ i.clamp(-9999, -9876)   ==== -9876
    T ~ i.clamp(-7777, 12345)   ==== -7777
    T ~ i.clamp(-7777, -9999)   ==== -7777
    T ~ i.checkIn(-9999, -7777) ==== -8888
    T ~ i.checkIn(-9999, -9876) ==== thrown[ArithmeticException]
    T ~ i.in(-9999, -7777)      ==== true
    T ~ i.in(-9999, -9876)      ==== false
    T ~ i.in(-7777, 12345)      ==== false
    T ~ i.in(-7777, -9999)      ==== false
    T ~ 1067030938.bitsF        ==== 1.2f
    T ~ i.sq                    ==== 78996544.0     --: typed[Double]
    T ~ i.sign                  ==== -1             --: typed[Int]
    T ~ (-i).sign               ==== 1
    T ~ (i/10000).sign          ==== 0
    T ~ i.u                     ==== i              --: typed[UInt]
    T ~ i.unsigned              ==== i              --: typed[UInt]
    T ~ i.toUByte               ==== (i & 0xFF)     --: typed[UByte]
    T ~ i.toUInt                ==== i              --: typed[UInt]
    T ~ i.toULong               ==== 4294958408L    --: typed[ULong]
    T ~ i.clampNeg              ==== 8888           --: typed[Int]
    T ~ Int.MinValue.clampNeg   ==== Int.MaxValue   --: typed[Int]
    T ~ i.clampToByte           ==== -128           --: typed[Byte]
    T ~ i2.clampToByte          ==== i2             --: typed[Byte]
    T ~ i4.clampToByte          ==== 127            --: typed[Byte]
    T ~ i2.clampToUByte         ==== 0              --: typed[UByte]
    T ~ i4.clampToUByte         ==== UByte(200)     --: typed[UByte]
    T ~ i5.clampToUByte         ==== UByte(255)     --: typed[UByte]
    T ~ i.clampToShort          ==== i              --: typed[Short]
    T ~ i3.clampToShort         ==== Short.MinValue --: typed[Short]
    T ~ i6.clampToShort         ==== Short.MaxValue --: typed[Short]
    T ~ i.clampToChar           ==== '\u0000'       --: typed[Char]
    T ~ i5.clampToChar          ==== '\u07D0'       --: typed[Char]
    T ~ i6.clampToChar          ==== '\uFFFF'       --: typed[Char]
    T ~ i.clampToUInt           ==== 0              --: typed[UInt]
    T ~ i6.clampToUInt          ==== i6             --: typed[UInt]
    T ~ i.clampToULong          ==== 0              --: typed[ULong]
    T ~ i6.clampToULong         ==== i6             --: typed[ULong]
    T ~ i.checkedNeg            ==== 8888           --: typed[Int]
    T ~ Int.MinValue.checkedNeg ==== thrown[ArithmeticException]
    T ~ i2.checkedToByte        ==== i2             --: typed[Byte]
    T ~ i.checkedToByte         ==== thrown[ArithmeticException]
    T ~ i4.checkedToUByte       ==== UByte(200)     --: typed[UByte]
    T ~ i.checkedToUByte        ==== thrown[ArithmeticException]
    T ~ i.checkedToShort        ==== i              --: typed[Short]
    T ~ i3.checkedToShort       ==== thrown[ArithmeticException]
    T ~ i5.checkedToChar        ==== '\u07D0'       --: typed[Char]
    T ~ i.checkedToChar         ==== thrown[ArithmeticException]
    T ~ i6.checkedToUInt        ==== i6             --: typed[UInt]
    T ~ i.checkedToUInt         ==== thrown[ArithmeticException]
    T ~ i6.checkedToULong       ==== i6             --: typed[ULong]
    T ~ i.checkedToULong        ==== thrown[ArithmeticException]
    val ix = 0x12345DCA
    T ~ ix.leadingZeros  ==== 3
    T ~ ix.trailingZeros ==== 1
    T ~ ix.bitCount      ==== 14
    T ~ ix.highBit       ==== 0x10000000
    T ~ ix.lowBit        ==== 0x00000002
    T ~ ix.rotl(8)       ==== 0x345DCA12
    T ~ ix.rotr(12)      ==== 0xDCA12345
    T ~ ix.hexString     ==== "12345DCA"
    T ~ ix.hiHexString   ==== "12345DCA"
    T ~ ix.loHexString   ==== "12345dca"

    val l = 42L
    val l2 = -88L
    val l3 = -888L
    val l4 = -88888L
    val l5 = -88888888888L
    val l6 = 200L
    val l7 = 20000L
    val l8 = 200000000L
    val l9 = 2000000000000L
    val lbiga = 77158763929L
    val lbigb = 119537721L
    val loka = 14197294936951L
    val lokb = 649657L
    val ldna = 4294967298L
    val ldnb = 2147483647L
    T ~ (l +# l)                          ==== 84L
    T ~ (l +# 9223372036854775800L)       ==== Long.MaxValue
    T ~ (-9223372036854775800L +# -10L)   ==== Long.MinValue
    T ~ (l -# l)                          ==== 0L
    T ~ (l -# (-9223372036854775800L))    ==== Long.MaxValue
    T ~ ((-9223372036854775800L) -# l)    ==== Long.MinValue
    T ~ (l *# l)                          ==== 1764L
    T ~ (219604096115589901L *# l)        ==== Long.MaxValue
    T ~ ((-219604096115589901L) *# l)     ==== Long.MinValue
    T ~ (lbiga *# lbigb)                  ==== Long.MaxValue
    T ~ (lbigb *# lbiga)                  ==== Long.MaxValue
    T ~ (lbiga *# -lbigb)                 ==== Long.MinValue
    T ~ (-lbiga *# lbigb)                 ==== Long.MinValue
    T ~ (-lbiga *# -lbigb)                ==== Long.MaxValue
    T ~ (loka *# lokb)                    ==== Long.MaxValue
    T ~ (lokb *# loka)                    ==== Long.MaxValue
    T ~ (-loka *# lokb)                   ==== -Long.MaxValue
    T ~ (loka *# -lokb)                   ==== -Long.MaxValue
    T ~ (-lokb *# loka)                   ==== -Long.MaxValue
    T ~ (lokb *# -loka)                   ==== -Long.MaxValue
    T ~ (-loka *# -lokb)                  ==== Long.MaxValue
    T ~ (-lokb *# -loka)                  ==== Long.MaxValue
    T ~ (ldna *# ldnb)                    ==== Long.MaxValue - 1
    T ~ (ldnb *# ldna)                    ==== Long.MaxValue - 1
    T ~ (-ldna *# ldnb)                   ==== 1 - Long.MaxValue
    T ~ (-ldnb *# ldna)                   ==== 1 - Long.MaxValue
    T ~ (ldna *# -ldnb)                   ==== 1 - Long.MaxValue
    T ~ (ldnb *# -ldna)                   ==== 1 - Long.MaxValue
    T ~ (-ldna *# -ldnb)                  ==== Long.MaxValue - 1
    T ~ (-ldnb *# -ldna)                  ==== Long.MaxValue - 1
    T ~ (l /# l)                          ==== 1L
    T ~ (Long.MinValue /# -1L)            ==== Long.MaxValue
    T ~ (l /# 0L)                         ==== Long.MaxValue
    T ~ (l2 /# 0L)                        ==== Long.MinValue
    T ~ (0L /# 0L)                        ==== 0L
    T ~ (l %# 5L)                         ==== 2L
    T ~ (l %# 0L)                         ==== 0L
    T ~ (l +! l)                          ==== 84L
    T ~ (l +! 9223372036854775800L)       ==== thrown[ArithmeticException]
    T ~ (l2 -! l3)                        ==== 800L
    T ~ ((-9223372036854775800L) -! l)    ==== thrown[ArithmeticException]
    T ~ (l *! l)                          ==== 1764L
    T ~ (lbiga *! lbigb)                  ==== thrown[ArithmeticException]
    T ~ (l /! 2)                          ==== 21L
    T ~ (Long.MinValue /! -1L)            ==== thrown[ArithmeticException]
    T ~ l.clamp(-100L, 100L)       ==== 42L
    T ~ l.clamp(-100L, -10L)       ==== -10L
    T ~ l.clamp(72L, 43210L)       ==== 72L
    T ~ l.clamp(100L, -100L)       ==== 100L
    T ~ l.checkIn(-100L, 100L)     ==== 42L
    T ~ l.checkIn(-100L, -10L)     ==== thrown[ArithmeticException]
    T ~ l.in(-100L, 100L)          ==== true
    T ~ l.in(-100L, -10L)          ==== false
    T ~ l.in(72L, 43210L)          ==== false
    T ~ l.in(100L, -100L)          ==== false
    T ~ 4608083138725491507L.bitsD ==== 1.2
    T ~ l.sq                       ==== 1764.0 --: typed[Double]
    T ~ l.sign                     ==== 1      --: typed[Long]
    T ~ (-l).sign                  ==== -1
    T ~ (l/100).sign               ==== 0
    T ~ lbiga.u                    ==== lbiga            --: typed[ULong]
    T ~ lbiga.unsigned             ==== lbiga            --: typed[ULong]
    T ~ lbiga.toUByte              ==== UByte(153)       --: typed[UByte]
    T ~ lbiga.toUInt               ==== UInt(0xF7054D99) --: typed[UInt]
    T ~ lbiga.toULong              ==== lbiga            --: typed[ULong]
    T ~ l2.clampNeg                ==== 88               --: typed[Long]
    T ~ Long.MinValue.clampNeg     ==== Long.MaxValue    --: typed[Long]
    T ~ l.clampToByte              ==== 42               --: typed[Byte]
    T ~ l2.clampToByte             ==== l2               --: typed[Byte]
    T ~ l3.clampToByte             ==== Byte.MinValue    --: typed[Byte]
    T ~ l6.clampToByte             ==== Byte.MaxValue    --: typed[Byte]
    T ~ l.clampToUByte             ==== l                --: typed[UByte]
    T ~ l2.clampToUByte            ==== 0                --: typed[UByte]
    T ~ l6.clampToUByte            ==== UByte(200)       --: typed[UByte]
    T ~ l7.clampToUByte            ==== UByte.MaxValue   --: typed[UByte]
    T ~ l.clampToShort             ==== l                --: typed[Short]
    T ~ l2.clampToShort            ==== l2               --: typed[Short]
    T ~ l4.clampToShort            ==== Short.MinValue   --: typed[Short]
    T ~ l8.clampToShort            ==== Short.MaxValue   --: typed[Short]
    T ~ l.clampToChar              ==== '*'              --: typed[Char]
    T ~ l2.clampToChar             ==== '\u0000'         --: typed[Char]
    T ~ l8.clampToChar             ==== '\uFFFF'         --: typed[Char]
    T ~ l.clampToInt               ==== l                --: typed[Int]
    T ~ l4.clampToInt              ==== l4               --: typed[Int]
    T ~ l5.clampToInt              ==== Int.MinValue     --: typed[Int]
    T ~ l8.clampToInt              ==== l8               --: typed[Int]
    T ~ l9.clampToInt              ==== Int.MaxValue     --: typed[Int]
    T ~ l.clampToUInt              ==== l                --: typed[UInt]
    T ~ l2.clampToUInt             ==== 0                --: typed[UInt]
    T ~ l8.clampToUInt             ==== l8               --: typed[UInt]
    T ~ l9.clampToUInt             ==== UInt.MaxValue    --: typed[UInt]
    T ~ l.clampToULong             ==== l                --: typed[ULong]
    T ~ l2.clampToULong            ==== 0L               --: typed[ULong]
    T ~ l9.clampToULong            ==== l9               --: typed[ULong]
    T ~ l2.checkedNeg              ==== 88               --: typed[Long]
    T ~ Long.MinValue.checkedNeg   ==== thrown[ArithmeticException]
    T ~ l.checkedToByte            ==== 42               --: typed[Byte]
    T ~ l3.checkedToByte           ==== thrown[ArithmeticException]
    T ~ l6.checkedToUByte          ==== UByte(200)       --: typed[UByte]
    T ~ l2.checkedToUByte          ==== thrown[ArithmeticException]
    T ~ l2.checkedToShort          ==== l2               --: typed[Short]
    T ~ l4.checkedToShort          ==== thrown[ArithmeticException]
    T ~ l.checkedToChar            ==== '*'              --: typed[Char]
    T ~ l2.checkedToChar           ==== thrown[ArithmeticException]
    T ~ l4.checkedToInt            ==== l4               --: typed[Int]
    T ~ l5.checkedToInt            ==== thrown[ArithmeticException]
    T ~ l8.checkedToUInt           ==== l8               --: typed[UInt]
    T ~ l9.checkedToUInt           ==== thrown[ArithmeticException]
    T ~ l9.checkedToULong          ==== l9               --: typed[ULong]
    T ~ l2.checkedToULong          ==== thrown[ArithmeticException]
    val lx = 0x123456789AB0FEDCL
    T ~ lx.leadingZeros  ==== 3
    T ~ lx.trailingZeros ==== 2
    T ~ lx.bitCount      ==== 32
    T ~ lx.highBit       ==== 0x1000000000000000L
    T ~ lx.lowBit        ==== 0x0000000000000004L
    T ~ lx.rotl(8)       ==== 0x3456789AB0FEDC12L
    T ~ lx.rotr(12)      ==== 0xEDC123456789AB0FL
    T ~ lx.hexString     ==== "123456789AB0FEDC"
    T ~ lx.hiHexString   ==== "123456789AB0FEDC"
    T ~ lx.loHexString   ==== "123456789ab0fedc"

    val f = 1.2f
    val fnan = Float.NaN
    T ~ f.trunc             ==== 1f
    T ~ (-f).trunc          ==== -1f
    T ~ f.sq                ==== 1.44f
    T ~ f.sign              ==== 1f
    T ~ f.ulp               ==== java.lang.Math.ulp(f)
    T ~ f.next              ==== f + java.lang.Math.ulp(f)
    T ~ f.prev              ==== f - java.lang.Math.ulp(f)
    T ~ f.nan               ==== false
    T ~ fnan.nan            ==== true
    T ~ f.inf               ==== false
    T ~ fnan.inf            ==== false
    T ~ (f/0f).inf          ==== true
    T ~ (-f/0f).inf         ==== true
    T ~ f.finite            ==== true
    T ~ fnan.finite         ==== false
    T ~ (f/0f).finite       ==== false
    T ~ (-f/0f).finite      ==== false
    T ~ f.clamp(0.7f, 3.5f) ==== 1.2f
    T ~ f.clamp(0.1f, 0.2f) ==== 0.2f
    T ~ f.clamp(2.3f, 3.4f) ==== 2.3f
    T ~ f.clamp(5.6f, 0.4f) ==== 5.6f
    T ~ f.clamp(0.7f, fnan) ==== fnan
    T ~ f.clamp(fnan, 3.5f) ==== fnan
    T ~ f.clamp(fnan, fnan) ==== fnan
    T ~ fnan.clamp(1f, 32f) ==== fnan
    T ~ fnan.clamp(32f, 1f) ==== fnan
    T ~ f.in(0.7f, 3.5f)    ==== true
    T ~ f.in(0.1f, 0.2f)    ==== false
    T ~ f.in(2.3f, 3.4f)    ==== false
    T ~ f.in(5.6f, 0.4f)    ==== false
    T ~ f.in(0.7f, fnan)    ==== false
    T ~ f.in(fnan, 3.5f)    ==== false
    T ~ f.in(fnan, fnan)    ==== false
    T ~ fnan.in(1f, 32f)    ==== false
    T ~ fnan.in(32f, 1f)    ==== false
    T ~ f.checkIn(0.7f, 4f) ==== f
    T ~ 0.7f.checkIn(f, 4f) ==== thrown[ArithmeticException]
    T ~ fnan.checkIn(0f,1f) ==== thrown[ArithmeticException]
    T ~ f.checkIn(0f, fnan) ==== thrown[ArithmeticException]
    T ~ f.checkIn(fnan, 4f) ==== thrown[ArithmeticException]
    T ~ f.closeTo(f+1e-7f)  ==== true
    T ~ f.closeTo(f+1e-5f)  ==== false
    T ~ f.closeTo(1f,1f,1f) ==== true
    T ~ f.bitsI             ==== 1067030938
    T ~ f.f64               ==== 1.2f.toDouble
    T ~ f.f64               ==== typed[Double]

    T ~ Array(1f, 2f, 4f).isIncreasing  ==== true
    T ~ Array(1f, 4f, 2f).isIncreasing  ==== false
    T ~ Array(1f, 2f, 4f).bisect(3f)    =~~= 1.5f
    T ~ Array(1f, 2f, 4f).bisect(5f)    ==== Float.PositiveInfinity
    T ~ Array(1f, 2f, 4f).bisect(2f)    ==== 1f
    T ~ Array(1f, 2f, 4f).bisect(-1f)   ==== Float.NegativeInfinity
    T ~ Array[Float]().bisect(0f)       ==== Float.NaN
    T ~ Array(1f).bisect(0f)            ==== Float.NegativeInfinity
    T ~ Array(1f).bisect(1f)            ==== 0f
    T ~ Array(1f).bisect(2f)            ==== Float.PositiveInfinity
    T ~ Array(1f, Float.NaN).bisect(2f) ==== Float.NaN
    T ~ Array(1f, 2f).bisect(Float.NaN) ==== Float.NaN

    val d = 1.2
    val dnan = Double.NaN
    T ~ d.trunc          ==== 1.0
    T ~ (-d).trunc       ==== -1.0
    T ~ d.sq             ==== 1.44
    T ~ d.cube           ==== 1.728
    T ~ d.sqrt           =~~= 1.0954451150103322269
    T ~ d.cbrt           =~~= 1.0626585691826110660
    T ~ d.hypot(1/d)     =~~= 1.4609738000540750438
    T ~ d.pow(d)         =~~= 1.2445647472039777218
    T ~ d.log            =~~= 0.18232155679395462621
    T ~ d.log2           =~~= 0.26303440583379383358
    T ~ d.log10          =~~= 0.079181246047624827723
    T ~ d.exp            =~~= 3.3201169227365474895
    T ~ d.exp2           =~~= 2.2973967099940700136
    T ~ d.exp10          =~~= 15.848931924611134852
    T ~ (1/d).entropy    =~~= 0.21919533819482819465
    T ~ d.sin            =~~= 0.93203908596722634967
    T ~ d.cos            =~~= 0.36235775447667357764
    T ~ d.tan            =~~= 2.5721516221263189354
    T ~ (1/d).asin       =~~= 0.98511078333774565961
    T ~ (1/d).acos       =~~= 0.58568554345715095962
    T ~ d.atan           =~~= 0.87605805059819342311
    T ~ d.atan2(1/d)     =~~= 0.96380866274848865959
    T ~ d.sinh           =~~= 1.5094613554121726964
    T ~ d.cosh           =~~= 1.8106555673243747931
    T ~ d.tanh           =~~= 0.83365460701215525867
    T ~ d.rad2deg        =~~= 68.754935415698785052
    T ~ d.rad2rev        =~~= 0.19098593171027440292
    T ~ d.deg2rad        =~~= 0.020943951023931954923
    T ~ d.rev2rad        =~~= 7.5398223686155037723
    T ~ d.gamma          =~~= 0.91816874239976061064
    T ~ d.lnGamma        =~~= -0.085374090003315849720
    T ~ d.erf            =~~= 0.91031397822963538024
    T ~ d.erfc           =~~= 0.089686021770364619762
    T ~ (1/d).erfInv     =~~= 0.97792452561403138647
    T ~ (1/d).erfcInv    =~~= 0.14879534452690386305
    T ~ d.cdfNormal      =~~= 0.88493032977829173198
    T ~ (1/d).icdfNormal =~~= 0.96742156610170103955
    T ~ d.sign           ==== 1.0
    T ~ d.rint           ==== 1.0
    T ~ d.ulp            ==== java.lang.Math.ulp(d)
    T ~ d.next           ==== d + java.lang.Math.ulp(d)
    T ~ d.prev           ==== d - java.lang.Math.ulp(d)
    T ~ d.nan            ==== false
    T ~ (d/0.0).nan      ==== false
    T ~ dnan.nan         ==== true
    T ~ d.inf            ==== false
    T ~ (d/0.0).inf      ==== true
    T ~ ((-d)/0.0).inf   ==== true
    T ~ dnan.inf         ==== false
    T ~ d.finite         ==== true
    T ~ (d/0.0).finite   ==== false
    T ~ dnan.finite      ==== false
    T ~ d.clamp(0.7, 3.5)     ==== 1.2
    T ~ d.clamp(0.1, 0.2)     ==== 0.2
    T ~ d.clamp(2.3, 3.4)     ==== 2.3
    T ~ d.clamp(5.6, 0.4)     ==== 5.6
    T ~ d.clamp(0.7, dnan)    ==== dnan
    T ~ d.clamp(dnan, 3.5)    ==== dnan
    T ~ d.clamp(dnan, dnan)   ==== dnan
    T ~ dnan.clamp(1.0, 32.0) ==== dnan
    T ~ dnan.clamp(32.0, 1.0) ==== dnan
    T ~ d.in(0.7, 3.5)        ==== true
    T ~ d.in(0.1, 0.2)        ==== false
    T ~ d.in(2.3, 3.4)        ==== false
    T ~ d.in(5.6, 0.4)        ==== false
    T ~ d.in(0.7, dnan)       ==== false
    T ~ d.in(dnan, 3.5)       ==== false
    T ~ d.in(dnan, dnan)      ==== false
    T ~ fnan.in(1.0, 32.0)    ==== false
    T ~ fnan.in(32.0, 1.0)    ==== false
    T ~ d.checkIn(0.7, 3.5)   ==== d
    T ~ 0.7.checkIn(d, 3.5)   ==== thrown[ArithmeticException]
    T ~ dnan.checkIn(d, d)    ==== thrown[ArithmeticException]
    T ~ d.checkIn(0.1, dnan)  ==== thrown[ArithmeticException]
    T ~ d.checkIn(dnan, 3.5)  ==== thrown[ArithmeticException]
    T ~ d.closeTo(d+1e-13)    ==== true
    T ~ d.closeTo(d+1e-8)     ==== false
    T ~ d.closeTo(1.0,1,1)    ==== true
    T ~ d.bitsL               ==== 4608083138725491507L
    T ~ d.f32                 ==== 1.2f
    T ~ d.f32                 ==== typed[Float]

    val dd = Array(5.0, 4.0, 3.0, 2.0, 1.0)
    T ~ Array(1.0, 2.0).accumulateInto(dd) =**= Array(1.0, 3.0, 3.0, 2.0, 1.0)
    T ~ Array(3.0).accumulateInto(dd, 4)   =**= Array(1.0, 3.0, 3.0, 2.0, 3.0)
    T ~ Array(3.0).accumulateInto(dd,1,3)  =**= Array(1.0, 6.0, 3.0, 2.0, 3.0)
    T ~ Array(1.0, 2.0, 4.0).accumulate    =**= Array(1.0, 3.0, 7.0)
    T ~ Array(1.0, 2.0, 4.0).isIncreasing  ==== true
    T ~ Array(1.0, 4.0, 2.0).isIncreasing  ==== false
    T ~ Array(1.0, 2.0, 4.0).bisect(3.0)   =~~= 1.5
    T ~ Array(1.0, 2.0, 4.0).bisect(5.0)   ==== Double.PositiveInfinity
    T ~ Array(1.0, 2.0, 4.0).bisect(2.0)   ==== 1.0
    T ~ Array(1.0, 2.0, 4.0).bisect(-1.0)  ==== Double.NegativeInfinity
    T ~ Array[Double]().bisect(0.0)        ==== Double.NaN
    T ~ Array(1.0).bisect(0.0)             ==== Double.NegativeInfinity
    T ~ Array(1.0).bisect(1.0)             ==== 0.0
    T ~ Array(1.0).bisect(2.0)             ==== Double.PositiveInfinity
    T ~ Array(1.0, Double.NaN).bisect(2.0) ==== Double.NaN
    T ~ Array(1.0, 2.0).bisect(Double.NaN) ==== Double.NaN


  @Test
  def unsignedMathTest(): Unit =
    val b = UByte(200)
    val c = UByte(14)
    val bz = UByte(0)
    T ~ b               ==== UByte.wrap(-56: Byte)
    T ~ UByte.MaxValue  ==== UByte(255)  --: typed[UByte]
    T ~ b.s             ==== b           --: typed[Byte]
    T ~ b.unwrap        ==== b           --: typed[Byte]
    T ~ b.signed        ==== b           --: typed[Byte]
    T ~ b.toByte        ==== b           --: typed[Byte]
    T ~ (b + UByte(9))  ==== UInt(209)   --: typed[UInt]
    T ~ (b + UInt(99))  ==== UInt(299)   --: typed[UInt]
    T ~ (b + ULong(99)) ==== ULong(299)  --: typed[ULong]
    T ~ (b +# UByte(9)) ==== UByte(209)  --: typed[UByte]
    T ~ (b +# b)        ==== UByte(255)  --: typed[UByte]
    T ~ (b +! UByte(9)) ==== UByte(209)  --: typed[UByte]
    T ~ (b +! b)        ==== thrown[ArithmeticException]
    T ~ (b - UByte(9))  ==== UInt(191)   --: typed[UInt]
    T ~ (b - UInt(9))   ==== UInt(191)   --: typed[UInt]
    T ~ (b - ULong(9))  ==== ULong(191)  --: typed[ULong]
    T ~ (b -# UByte(2)) ==== UByte(198)  --: typed[UByte]
    T ~ (UByte(2) -# b) ==== UByte(0)    --: typed[UByte]
    T ~ (b -! UByte(2)) ==== UByte(198)  --: typed[UByte]
    T ~ (UByte(2) -! b) ==== thrown[ArithmeticException]
    T ~ (c * c)         ==== UInt(196)   --: typed[UInt]
    T ~ (b * UInt(9))   ==== UInt(1800)  --: typed[UInt]
    T ~ (b * ULong(9))  ==== ULong(1800) --: typed[ULong]
    T ~ (c *# c)        ==== UByte(196)  --: typed[UByte]
    T ~ (b *# c)        ==== UByte(255)  --: typed[UByte]
    T ~ (c *! c)        ==== UByte(196)  --: typed[UByte]
    T ~ (b *! c)        ==== thrown[ArithmeticException]
    T ~ (b / c)         ==== UInt(14)    --: typed[UInt]
    T ~ (b / UInt(14))  ==== UInt(14)    --: typed[UInt]
    T ~ (b / ULong(9L)) ==== ULong(22)   --: typed[ULong]
    T ~ (b /# c)        ==== UByte(14)   --: typed[UByte]
    T ~ (b /# bz)       ==== UByte(255)
    T ~ (bz /# bz)      ==== bz
    T ~ (b /! c)        ==== UByte(14)   --: typed[UByte]
    T ~ (b % c)         ==== UInt(4)     --: typed[UInt]
    T ~ (b % UInt(14))  ==== UInt(4)     --: typed[UInt]
    T ~ (b % ULong(14)) ==== ULong(4)    --: typed[ULong]
    T ~ (b %# c)        ==== UByte(4)    --: typed[UByte]
    T ~ (b %# bz)       ==== bz
    T ~ (b | c)         ==== UByte(206)  --: typed[UByte]
    T ~ (b & c)         ==== UByte(8)    --: typed[UByte]
    T ~ (b ^ c)         ==== UByte(198)  --: typed[UByte]
    T ~ (b < c)         ==== false
    T ~ (b < b)         ==== false
    T ~ (c < b)         ==== true
    T ~ (b <= c)        ==== false
    T ~ (b <= b)        ==== true
    T ~ (c <= b)        ==== true
    T ~ (b >= c)        ==== true
    T ~ (b >= b)        ==== true
    T ~ (c >= b)        ==== false
    T ~ (b > c)         ==== true
    T ~ (b > b)         ==== false
    T ~ (c > b)         ==== false
    T ~ (b >> 2)        ==== UInt(50)    --: typed[UInt]
    T ~ (b >> UInt(2))  ==== UInt(50)    --: typed[UInt]
    T ~ (b << 2)        ==== UInt(800)   --: typed[UInt]
    T ~ (b << UInt(2))  ==== UInt(800)   --: typed[UInt]
    T ~ (b max c)       ==== b           --: typed[UByte]
    T ~ (c max b)       ==== b           --: typed[UByte]
    T ~ (b min c)       ==== c           --: typed[UByte]
    T ~ (c min b)       ==== c           --: typed[UByte]
    T ~ c.clamp(bz, b)  ==== c           --: typed[UByte]
    T ~ b.clamp(c, bz)  ==== c
    T ~ b.clamp(bz, c)  ==== c
    T ~ bz.clamp(c, b)  ==== c
    T ~ c.checkIn(bz,b) ==== c           --: typed[UByte]
    T ~ bz.checkIn(c,b) ==== thrown[ArithmeticException]
    T ~ c.in(bz, b)     ==== true
    T ~ b.in(bz, c)     ==== false
    T ~ bz.in(b, c)     ==== false
    T ~ b.toShort       ==== 200         --: typed[Short]
    T ~ b.toChar        ==== '\u00C8'    --: typed[Char]
    T ~ b.toInt         ==== 200         --: typed[Int]
    T ~ b.toUInt        ==== 200         --: typed[UInt]
    T ~ b.toLong        ==== 200L        --: typed[Long]
    T ~ b.toULong       ==== 200L        --: typed[ULong]
    T ~ b.toFloat       ==== 200f        --: typed[Float]
    T ~ b.toDouble      ==== 200.0       --: typed[Double]
    T ~ b.clampToByte   ==== 127         --: typed[Byte]
    T ~ c.clampToByte   ==== c           --: typed[Byte]
    T ~ c.checkedToByte ==== c           --: typed[Byte]
    T ~ b.checkedToByte ==== thrown[ArithmeticException]
    T ~ b.pr            ==== "200"
    T ~ b.hexString     ==== "C8"
    T ~ b.hiHexString   ==== "C8"
    T ~ b.loHexString   ==== "c8"

    val h = UInt(0x22000070)
    val i = UInt(0xA0000000)
    val j = UInt(51810)
    val i2 = UInt(42)
    val i3 = UInt(200)
    val i4 = UInt(4200)
    val i5 = UInt(42000)
    val iz = UInt(0)
    T ~ i                ==== UInt.wrap(-1610612736)
    T ~ UInt.MaxValue    ==== UInt(0xFFFFFFFF)    --: typed[UInt]
    T ~ i.s              ==== i                   --: typed[Int]
    T ~ i.unwrap         ==== i                   --: typed[Int]
    T ~ i.signed         ==== i                   --: typed[Int]
    T ~ i.toInt          ==== i                   --: typed[Int]
    T ~ (i + j)          ==== UInt(0xA000CA62)    --: typed[UInt]
    T ~ (i + UByte(200)) ==== UInt(0xA00000C8)    --: typed[UInt]
    T ~ (i + ULong(200)) ==== ULong(0xA00000C8L)  --: typed[ULong]
    T ~ (i +# j)         ==== (i + j)             --: typed[UInt]
    T ~ (i +# i)         ==== UInt.MaxValue       --: typed[UInt]
    T ~ (i +! j)         ==== (i + j)             --: typed[UInt]
    T ~ (i +! i)         ==== thrown[ArithmeticException]
    T ~ (i - j)          ==== UInt(0x9FFF359E)    --: typed[UInt]
    T ~ (i - UByte(200)) ==== UInt(0x9FFFFF38)    --: typed[UInt]
    T ~ (i - ULong(200)) ==== ULong(0x9FFFFF38L)  --: typed[ULong]
    T ~ (i -# j)         ==== (i - j)             --: typed[UInt]
    T ~ (j -# i)         ==== 0                   --: typed[UInt]
    T ~ (i -! j)         ==== (i - j)             --: typed[UInt]
    T ~ (j -! i)         ==== thrown[ArithmeticException]
    T ~ (j * j)          ==== UInt(0x9FFECD84)    --: typed[UInt]
    T ~ (j * UByte(200)) ==== UInt(10362000)      --: typed[UInt]
    T ~ (j * ULong(200)) ==== ULong(10362000)     --: typed[ULong]
    T ~ (j *# j)         ==== (j * j)             --: typed[UInt]
    T ~ (j *# i)         ==== UInt.MaxValue       --: typed[UInt]
    T ~ (j *! j)         ==== (j * j)             --: typed[UInt]
    T ~ (j *! i)         ==== thrown[ArithmeticException]
    T ~ (i / j)          ==== UInt(51811)         --: typed[UInt]
    T ~ (j / UByte(200)) ==== UInt(259)           --: typed[UInt]
    T ~ (j / ULong(200)) ==== ULong(259)          --: typed[ULong]
    T ~ (i /# j)         ==== (i / j)             --: typed[UInt]
    T ~ (i /# iz)        ==== UInt.MaxValue
    T ~ (iz /# iz)       ==== UInt(0)
    T ~ (i /! j)         ==== (i / j)             --: typed[UInt]
    T ~ (i % j)          ==== UInt(26650)         --: typed[UInt]
    T ~ (i % UByte(200)) ==== UInt(160)           --: typed[UInt]
    T ~ (i % ULong(200)) ==== ULong(160)          --: typed[ULong]
    T ~ (i %# j)         ==== (i % j)             --: typed[UInt]
    T ~ (i %# iz)        ==== UInt(0)
    T ~ (i | h)          ==== UInt(0xA2000070)    --: typed[UInt]
    T ~ (i & h)          ==== UInt(0x20000000)    --: typed[UInt]
    T ~ (i ^ h)          ==== UInt(0x82000070)    --: typed[UInt]
    T ~ (i < j)          ==== false
    T ~ (i < i)          ==== false
    T ~ (j < i)          ==== true
    T ~ (i <= j)         ==== false
    T ~ (i <= i)         ==== true
    T ~ (j <= i)         ==== true
    T ~ (i >= j)         ==== true
    T ~ (i >= i)         ==== true
    T ~ (j >= i)         ==== false
    T ~ (i > j)          ==== true
    T ~ (i > i)          ==== false
    T ~ (j > i)          ==== false
    T ~ (i >> 3)         ==== UInt(0x14000000)    --: typed[UInt]
    T ~ (i >> UInt(3))   ==== UInt(0x14000000)    --: typed[UInt]
    T ~ (j << 17)        ==== UInt(0x94C40000)    --: typed[UInt]
    T ~ (j << UInt(17))  ==== UInt(0x94C40000)    --: typed[UInt]
    T ~ (i max j)        ==== i
    T ~ (j max i)        ==== i
    T ~ (i min j)        ==== j
    T ~ (j min i)        ==== j
    T ~ j.clamp(iz, i)   ==== j                   --: typed[UInt]
    T ~ j.clamp(i, iz)   ==== i
    T ~ iz.clamp(j, i)   ==== j
    T ~ i.clamp(iz, j)   ==== j
    T ~ j.checkIn(iz, i) ==== j                   --: typed[UInt]
    T ~ j.checkIn(i, iz) ==== thrown[ArithmeticException]
    T ~ i.checkIn(iz, j) ==== thrown[ArithmeticException]
    T ~ iz.checkIn(j, i) ==== thrown[ArithmeticException]
    T ~ j.in(iz, i)      ==== true
    T ~ j.in(i, iz)      ==== false
    T ~ iz.in(j, i)      ==== false
    T ~ i.in(iz, j)      ==== false
    T ~ (i + j).toByte         ==== 0x62                --: typed[Byte]
    T ~ (i + j).toUByte        ==== UByte(0x62)         --: typed[UByte]
    T ~ (i + j).toShort        ==== -13726              --: typed[Short]
    T ~ (i + j).toChar         ==== '\uCA62'            --: typed[Char]
    T ~ (i + j).toInt          ==== -1610560926         --: typed[Int]
    T ~ (i + j).toLong         ==== 0xA000CA62L         --: typed[Long]
    T ~ (i + j).toULong        ==== ULong(0xA000CA62)   --: typed[ULong]
    T ~ i.toFloat              ==== 2.68435456e9f       --: typed[Float]
    T ~ i.toDouble             ==== 2.68435456e9        --: typed[Double]
    T ~ i2.clampToByte         ==== i2                  --: typed[Byte]
    T ~ i3.clampToByte         ==== Byte.MaxValue       --: typed[Byte]
    T ~ i.clampToByte          ==== Byte.MaxValue       --: typed[Byte]
    T ~ i3.clampToUByte        ==== UByte(200)          --: typed[UByte]
    T ~ i4.clampToUByte        ==== UByte.MaxValue      --: typed[UByte]
    T ~ i.clampToUByte         ==== UByte.MaxValue      --: typed[UByte]
    T ~ i4.clampToShort        ==== i4                  --: typed[Short]
    T ~ i5.clampToShort        ==== Short.MaxValue      --: typed[Short]
    T ~ i.clampToShort         ==== Short.MaxValue      --: typed[Short]
    T ~ i4.clampToChar         ==== '\u1068'            --: typed[Char]
    T ~ h.clampToChar          ==== '\uFFFF'            --: typed[Char]
    T ~ i.clampToChar          ==== '\uFFFF'            --: typed[Char]
    T ~ i5.clampToInt          ==== i5                  --: typed[Int]
    T ~ i.clampToInt           ==== Int.MaxValue        --: typed[Int]
    T ~ i2.checkedToByte       ==== i2                  --: typed[Byte]
    T ~ i3.checkedToByte       ==== thrown[ArithmeticException]
    T ~ i3.checkedToUByte      ==== UByte(200)          --: typed[UByte]
    T ~ i4.checkedToUByte      ==== thrown[ArithmeticException]
    T ~ i4.checkedToShort      ==== i4                  --: typed[Short]
    T ~ i5.checkedToShort      ==== thrown[ArithmeticException]
    T ~ i5.checkedToChar       ==== '\uA410'            --: typed[Char]
    T ~ h.checkedToChar        ==== thrown[ArithmeticException]
    T ~ i5.checkedToInt        ==== i5                  --: typed[Int]
    T ~ i.checkedToInt         ==== thrown[ArithmeticException]
    T ~ i.pr                   ==== "2684354560"
    T ~ i.hexString            ==== "A0000000"
    T ~ i.hiHexString          ==== "A0000000"
    T ~ i.loHexString          ==== "a0000000"

    val l = ULong(0xA2D03579B4E6FC81L)
    val k = ULong(3425191356L)
    val m = ULong.MaxValue
    val lsm = ULong(0x7FFFFFFFFFFFFFFDL)
    val lbg = ULong(0x8000000000000001L)
    val lba = ULong(67280421310721L)
    val lbb = ULong(274177L)
    val lsa = ULong(28394589873902L)
    val lsb = ULong(649657L)
    val l2 = ULong(0x20)
    val l3 = ULong(200)
    val l4 = ULong(40000)
    val l5 = ULong(4000000000L)
    val l6 = ULong(2000000000000L)
    val lz = ULong(0)
    T ~ l            ==== ULong.wrap(-6714808247567057791L)
    T ~ l.s          ==== l                    --: typed[Long]
    T ~ l.unwrap     ==== l                    --: typed[Long]
    T ~ l.signed     ==== l                    --: typed[Long]
    T ~ l.toLong     ==== l                    --: typed[Long]
    T ~ (l + k)      ==== 0xA2D0357A810F423DL  --: typed[ULong]
    T ~ (l + i)      ==== 0xA2D0357A54E6FC81L  --: typed[ULong]
    T ~ (l + b)      ==== 0xA2D03579B4E6FD49L  --: typed[ULong]
    T ~ (l +# k)     ==== (l + k)              --: typed[ULong]
    T ~ (lsm +# lbg) ==== ((-1L).u - 1L.u)     --: typed[ULong]
    T ~ (lbg +# lsm) ==== ((-1L).u - 1L.u)     --: typed[ULong]
    T ~ (k +# k)     ==== (2 * k.signed)       --: typed[ULong] 
    T ~ (l +# l)     ==== ULong.MaxValue       --: typed[ULong]
    T ~ (l +# lsm)   ==== ULong.MaxValue       --: typed[ULong]
    T ~ (lsm +# l)   ==== ULong.MaxValue       --: typed[ULong]
    T ~ (l +! k)     ==== (l + k)              --: typed[ULong]
    T ~ (l +! l)     ==== thrown[ArithmeticException]
    T ~ (lsm +! l)   ==== thrown[ArithmeticException]
    T ~ (l - k)      ==== 0xA2D03578E8BEB6C5L  --: typed[ULong]
    T ~ (l - i)      ==== 0xA2D0357914E6FC81L  --: typed[ULong]
    T ~ (l - b)      ==== 0xA2D03579B4E6FBB9L  --: typed[ULong]
    T ~ (l -# k)     ==== (l - k)              --: typed[ULong]
    T ~ (k -# l)     ==== 0L                   --: typed[ULong]
    T ~ (lsm -# lbg) ==== 0L                   --: typed[ULong]
    T ~ (lbg -# lsm) ==== 4L                   --: typed[ULong]
    T ~ (1L.u -# k)  ==== 0L                   --: typed[ULong]
    T ~ (k -# 1L.u)  ==== 3425191355L          --: typed[ULong]
    T ~ (l -! k)     ==== (l - k)              --: typed[ULong]
    T ~ (k -! l)     ==== thrown[ArithmeticException]
    T ~ (k * k)      ==== 0xA2D035797DBEE210L  --: typed[ULong]
    T ~ (k * i)      ==== 0x7F992B9580000000L  --: typed[ULong]
    T ~ (k * b)      ==== 0x9F7F767AE0L        --: typed[ULong]
    T ~ (k *# k)     ==== (k * k)              --: typed[ULong]
    T ~ (k *# l)     ==== ULong.MaxValue
    T ~ (l *# k)     ==== ULong.MaxValue
    T ~ (l *# l)     ==== ULong.MaxValue
    T ~ (k *# 1L.u)  ==== k
    T ~ (lba *# lbb) ==== ULong.MaxValue
    T ~ (lbb *# lba) ==== ULong.MaxValue
    T ~ (lsa *# lsb) ==== (ULong.MaxValue - 1.u)
    T ~ (lsb *# lsa) ==== (ULong.MaxValue - 1.u)
    T ~ (k *! k)     ==== (k * k)              --: typed[ULong]
    T ~ (k *! l)     ==== thrown[ArithmeticException]
    T ~ (lba *! lbb) ==== thrown[ArithmeticException]
    T ~ (lsb *! lsa) ==== (lsb * lsa)
    T ~ (l / k)      ==== k                    --: typed[ULong]
    T ~ (l / i)      ==== 4370486671L          --: typed[ULong]
    T ~ (l / b)      ==== 58659679130712469L   --: typed[ULong]
    T ~ (l /# k)     ==== k                    --: typed[ULong]
    T ~ (l /# lz)    ==== ULong.MaxValue
    T ~ (lz /# lz)   ==== lz
    T ~ (l /! k)     ==== (l / k)              --: typed[ULong]
    T ~ (l % k)      ==== 925375089L           --: typed[ULong]
    T ~ (l % b)      ==== 25L                  --: typed[ULong]
    T ~ (l % i3)     ==== 25L                  --: typed[ULong]
    T ~ (l %# k)     ==== (l % k)              --: typed[ULong]
    T ~ (l %# lz)    ==== lz
    T ~ (l | k)      ==== 0xA2D03579FCEEFDBDL  --: typed[ULong]
    T ~ (l & k)      ==== 0x84204480L          --: typed[ULong]
    T ~ (l ^ k)      ==== 0xA2D0357978CEB93DL  --: typed[ULong]
    T ~ (l < k)      ==== false
    T ~ (l < l)      ==== false
    T ~ (k < l)      ==== true
    T ~ (l <= k)     ==== false
    T ~ (l <= l)     ==== true
    T ~ (k <= l)     ==== true
    T ~ (l >= k)     ==== true
    T ~ (l >= l)     ==== true
    T ~ (k >= l)     ==== false
    T ~ (l > k)      ==== true
    T ~ (l > l)      ==== false
    T ~ (k > l)      ==== false
    T ~ (l >> 16)    ==== 0xA2D03579B4E6L      --: typed[ULong]
    T ~ (l>>UInt(8)) ==== 0xA2D03579B4E6FCL    --: typed[ULong]
    T ~ (l << 8)     ==== 0xD03579B4E6FC8100L  --: typed[ULong]
    T ~ (l<<UInt(4)) ==== 0x2D03579B4E6FC810L  --: typed[ULong]
    T ~ (l max k)       ==== l                 --: typed[ULong]
    T ~ (k max l)       ==== l                 --: typed[ULong]
    T ~ (l min k)       ==== k                 --: typed[ULong]
    T ~ (k min l)       ==== k                 --: typed[ULong]
    T ~ k.clamp(lz, l)  ==== k                 --: typed[ULong]
    T ~ lz.clamp(k, l)  ==== k
    T ~ lz.clamp(l, k)  ==== l
    T ~ l.clamp(lz, k)  ==== k
    T ~ k.checkIn(lz,l) ==== k                 --: typed[ULong]
    T ~ lz.checkIn(k,l) ==== thrown[ArithmeticException]
    T ~ l.checkIn(lz,k) ==== thrown[ArithmeticException]
    T ~ k.in(lz, l)     ==== true
    T ~ lz.in(k, l)     ==== false
    T ~ l.in(lz, k)     ==== false
    T ~ k.in(l, lz)     ==== false
    T ~ l.toByte        ==== 0x81.toByte       --: typed[Byte]
    T ~ l.toUByte       ==== UByte(0x81)       --: typed[UByte]
    T ~ l.toShort       ==== 0xFC81.toShort    --: typed[Short]
    T ~ l.toChar        ==== '\uFC81'          --: typed[Char]
    T ~ l.toInt         ==== 0xB4E6FC81.toInt  --: typed[Int]
    T ~ l.toUInt        ==== UInt(0xB4E6FC81)  --: typed[UInt]
    T ~ l.toFloat       ==== 1.1731935826142495E19f --: typed[Float]
    T ~ l.toDouble      ==== 1.1731935826142495E19  --: typed[Double]
    T ~ l2.clampToByte  ==== l2                --: typed[Byte]
    T ~ l3.clampToByte  ==== Byte.MaxValue     --: typed[Byte]
    T ~ l.clampToByte   ==== Byte.MaxValue     --: typed[Byte]
    T ~ l3.clampToUByte ==== UByte(200)        --: typed[UByte]
    T ~ l4.clampToUByte ==== UByte.MaxValue    --: typed[UByte]
    T ~ l.clampToUByte  ==== UByte.MaxValue    --: typed[UByte]
    T ~ l3.clampToShort ==== l3                --: typed[Short]
    T ~ l4.clampToShort ==== Short.MaxValue    --: typed[Short]
    T ~ l.clampToShort  ==== Short.MaxValue    --: typed[Short]
    T ~ l4.clampToChar  ==== '\u9C40'          --: typed[Char]
    T ~ l5.clampToChar  ==== '\uFFFF'          --: typed[Char]
    T ~ l.clampToChar   ==== '\uFFFF'          --: typed[Char]
    T ~ l4.clampToInt   ==== l4                --: typed[Int]
    T ~ l5.clampToInt   ==== Int.MaxValue      --: typed[Int]
    T ~ l.clampToInt    ==== Int.MaxValue      --: typed[Int]
    T ~ l5.clampToUInt  ==== UInt(0xEE6B2800)  --: typed[UInt]
    T ~ l6.clampToUInt  ==== UInt.MaxValue     --: typed[UInt]
    T ~ l.clampToUInt   ==== UInt.MaxValue     --: typed[UInt]
    T ~ l6.clampToLong  ==== l6                --: typed[Long]
    T ~ l.clampToLong   ==== Long.MaxValue     --: typed[Long]
    T ~ l2.checkedToByte  ==== l2                --: typed[Byte]
    T ~ l3.checkedToByte  ==== thrown[ArithmeticException]
    T ~ l3.checkedToUByte ==== UByte(200)        --: typed[UByte]
    T ~ l4.checkedToUByte ==== thrown[ArithmeticException]
    T ~ l3.checkedToShort ==== l3                --: typed[Short]
    T ~ l4.checkedToShort ==== thrown[ArithmeticException]
    T ~ l4.checkedToChar  ==== '\u9C40'          --: typed[Char]
    T ~ l5.checkedToChar  ==== thrown[ArithmeticException]
    T ~ l4.checkedToInt   ==== l4                --: typed[Int]
    T ~ l5.checkedToInt   ==== thrown[ArithmeticException]
    T ~ l5.checkedToUInt  ==== UInt(0xEE6B2800)  --: typed[UInt]
    T ~ l6.checkedToUInt  ==== thrown[ArithmeticException]
    T ~ l6.checkedToLong  ==== l6                --: typed[Long]
    T ~ l.checkedToLong   ==== thrown[ArithmeticException]
    T ~ l.pr            ==== "11731935826142493825"
    T ~ l.hexString     ==== "A2D03579B4E6FC81"
    T ~ l.hiHexString   ==== "A2D03579B4E6FC81"
    T ~ l.loHexString   ==== "a2d03579b4e6fc81"


  @Test
  def dualvaluedMathTest(): Unit =
    val v = 1.2f ~> 3.1f
    val u = -1.5f ~> 0.7f
    T ~ (v == Vc.F(1.2f, 3.1f))    ==== true
    T ~ (v === Vc.D(1.2, 3.1))     ==== true
    T ~ (1.0f ~> 0.0f)             ==== Vc.wrap(0x000000003F800000L)
    T ~ (0.0f ~> 1.0f)             ==== Vc.wrap(0x3F80000000000000L)
    T ~ (1.0f ~> 0.0f).unwrap      ==== 0x000000003F800000L
    T ~ Vc(0, 0)                   ==== Vc.zero
    T ~ Vc.NaN.x.nan               ==== true
    T ~ Vc.NaN.y.nan               ==== true
    T ~ v.x                        ==== 1.2f
    T ~ v.y                        ==== 3.1f
    T ~ v.xTo(5.4f).x              ==== 5.4f
    T ~ v.xTo(5.4f).y              ==== 3.1f
    T ~ v.xFn(_ + 1).x             ==== 2.2f
    T ~ v.xFn(_ + 1).y             ==== 3.1f
    T ~ v.yTo(0.3f).x              ==== 1.2f
    T ~ v.yTo(0.3f).y              ==== 0.3f
    T ~ v.yFn(_ - 1).x             ==== 1.2f
    T ~ v.yFn(_ - 1).y             ==== 2.1f
    T ~ Vc.zero.isZero             ==== true
    T ~ Vc(-0f, 0f).isZero         ==== true
    T ~ Vc(0f, -0f).isZero         ==== true
    T ~ Vc.wrap(1).isZero          ==== false
    T ~ Vc.NaN.isZero              ==== false
    T ~ v.isZero                   ==== false
    T ~ Vc.zero.isFinite           ==== true
    T ~ v.isFinite                 ==== true
    T ~ Vc.NaN.isFinite            ==== false
    T ~ Vc(Float.NaN, 0f).isFinite ==== false
    T ~ Vc(0f, Float.NaN).isFinite ==== false
    T ~ Vc( 1.0f/0f, 0f).isFinite  ==== false
    T ~ Vc(-1.0f/0f, 0f).isFinite  ==== false
    T ~ Vc(0f,  1.0f/0f).isFinite  ==== false
    T ~ Vc(0f, -1.0f/0f).isFinite  ==== false
    T ~ v.isNaN                    ==== false
    T ~ Vc.NaN.isNaN               ==== true
    T ~ Vc(Float.NaN, 0f).isNaN    ==== true
    T ~ Vc(0f, Float.NaN).isNaN    ==== true
    T ~ Vc( 1.0f/0f, 0f).isNaN     ==== false
    T ~ Vc(-1.0f/0f, 0f).isNaN     ==== false
    T ~ Vc(0f,  1.0f/0f).isNaN     ==== false
    T ~ Vc(0f, -1.0f/0f).isNaN     ==== false
    T ~ v.swapped.x                ==== 3.1f
    T ~ v.swapped.y                ==== 1.2f
    T ~ v.cw.x                     ==== 3.1f
    T ~ v.cw.y                     ==== -1.2f
    T ~ v.ccw.x                    ==== -3.1f
    T ~ v.ccw.y                    ==== 1.2f
    T ~ (-v).x                     ==== -1.2f
    T ~ (-v).y                     ==== -3.1f
    T ~ v.rotate(0.37f).x          =~~= -0.00221502436414f
    T ~ v.rotate(0.37f).y          =~~= 3.32415328974f
    T ~ v.theta.f32                =~~= 1.20146266912f
    T ~ v.lenSq.f32                =~~= 11.05f
    T ~ v.len                      =~~= 3.32415402772f
    T ~ (v + 2f).x                 =~~= 3.2f
    T ~ (v + 2f).y                 =~~= 5.1f
    T ~ (v + 2f)                   ==== (2f + v)
    T ~ v.+(1f, -1f).x             =~~= 2.2f
    T ~ v.+(1f, -1f).y             =~~= 2.1f
    T ~ (v + Vc(1, -1)).x          =~~= 2.2f
    T ~ (v + Vc(1, -1)).y          =~~= 2.1f
    T ~ (v - 2f).x                 =~~= -0.8f
    T ~ (v - 2f).y                 =~~= 1.1f
    T ~ (v - 2f)                   ==== -(2f - v)
    T ~ v.-(-1, 1).x               =~~= 2.2f
    T ~ v.-(-1, 1).y               =~~= 2.1f
    T ~ (v - Vc(-1, 1)).x          =~~= 2.2f
    T ~ (v - Vc(-1, 1)).y          =~~= 2.1f
    T ~ (v * 2f).x                 =~~= 2.4f
    T ~ (v * 2f).y                 =~~= 6.2f
    T ~ (v * 2f)                   ==== (2f * v)
    T ~ v.*(-1.5f, 0.7f).f32       =~~= 0.37f
    T ~ (v * u)                    ==== v.*(-1.5f, 0.7f)
    T ~ v.X(-1.5f, 0.7f).f32       =~~= 5.49f
    T ~ (v X u)                    ==== v.X(-1.5f, 0.7f)
    T ~ v.proj(-1.5f, 0.7f).x      =~~= -0.202554744525f
    T ~ v.proj(-1.5f, 0.7f).y      =~~= 0.0945255474453f
    T ~ (v proj u)                 ==== v.proj(-1.5f, 0.7f)
    T ~ v.orth(-1.5f, 0.7f).x      =~~= 1.40255474453f
    T ~ v.orth(-1.5f, 0.7f).y      =~~= 3.00547445255f
    T ~ (v orth u)                 ==== v.orth(-1.5f, 0.7f)
    T ~ v.hat.x                    =~~= 0.360994102558f
    T ~ v.hat.y                    =~~= 0.932568098274f
    T ~ v.normDot(-1.5f, 0.7f).f32 =~~= 0.0672427248161f
    T ~ (v normDot u)              ==== v.normDot(-1.5f, 0.7f)
    T ~ v.distSq(-1.5f, 0.7f).f32  =~~= 13.05f
    T ~ (v distSq u)               ==== v.distSq(-1.5f, 0.7f)
    T ~ v.dist(-1.5f, 0.7f)        =~~= 3.61247837364f
    T ~ (v dist u)                 ==== v.dist(-1.5f, 0.7f)
    T ~ v.angle(-1.5f, 0.7f).f32   =~~= 1.50350282465f
    T ~ (v angle u)                ==== v.angle(-1.5f, 0.7f)
    T ~ (u angle v)                ==== -(v angle u)
    T ~ v.pr                       ==== "[1.2 3.1]"
    T ~ v.prf("%.2e")              ==== "[1.20e+00 3.10e+00]"

    val pm = 3.5f +- 0.2f
    val qm = 1.9f +- 0.5f
    T ~ pm                      ==== PlusMinus(3.5f, 0.2f)
    T ~ (pm === (3.5f +- -0.2f))==== true
    T ~ (1f +- 0f)              ==== PlusMinus.wrap(0x3F80000000000000L)
    T ~ (0f +- 1f)              ==== PlusMinus.wrap(0x000000003F800000L)
    T ~ (1f +- 0f).unwrap       ==== 0x3F80000000000000L
    T ~ pm                      ==== PlusMinus.D(3.5, 0.2)
    T ~ (1f +- 0f)              ==== PlusMinus.exact(1f)
    T ~ pm.value                ==== 3.5f
    T ~ pm.error                ==== 0.2f
    T ~ pm.valueTo(1.9f).value  ==== 1.9f
    T ~ pm.valueTo(1.9f).error  ==== 0.2f
    T ~ pm.valueFn(_ + 1).value ==== 4.5f
    T ~ pm.valueFn(_ + 1).error ==== 0.2f
    T ~ pm.errorTo(0.6f).value  ==== 3.5f
    T ~ pm.errorTo(0.6f).error  ==== 0.6f
    T ~ pm.errorFn(_ - 1).value ==== 3.5f
    T ~ pm.errorFn(_ - 1).error ==== 0.8f
    T ~ (-pm).value             ==== -3.5f
    T ~ (-pm).error             ==== 0.2f
    T ~ (pm + 1).value          ==== 4.5f
    T ~ (pm + 1).error          ==== 0.2f
    T ~ (1f + pm)               ==== (pm + 1f)
    T ~ (pm + qm).value         ==== 5.4f
    T ~ (pm + qm).error         =~~= 0.538516480713f
    T ~ (pm - 1).value          ==== 2.5f
    T ~ (pm - 1).error          ==== 0.2f
    T ~ (1f - pm)               ==== -(pm - 1f)
    T ~ (pm - qm).value         ==== 1.6f
    T ~ (pm - qm).error         ==== (pm + qm).error
    T ~ (pm * 2f).value         ==== 7.0f
    T ~ (pm * 2f).error         ==== 0.4f
    T ~ (2f * pm)               ==== (pm * 2f)
    T ~ (pm * qm).value         =~~= 6.65f
    T ~ (pm * qm).error         =~~= 1.79078195211f
    T ~ (pm / 2f).value         ==== 1.75f
    T ~ (pm / 2f).error         ==== 0.1f
    T ~ (2f / pm).value         =~~= 0.571428571429f
    T ~ (2f / pm).error         =~~= 0.0326531f
    T ~ (pm / qm).value         =~~= 1.84210526316f
    T ~ (pm / qm).error         =~~= 0.496061f
    T ~ pm.sq.value             =~~= (pm * pm).value
    T ~ pm.sq.error             =~~= (pm * pm).error
    T ~ pm.sqrt.value           =~~= 1.87082869339f
    T ~ pm.sqrt.error           =~~= 0.0534522f
    T ~ pm.pow(1.7f).value      =~~= 8.41231788372f
    T ~ pm.pow(1.7f).error      =~~= 0.817197f
    T ~ pm.pr                   ==== "3.5 +- 0.2"
    T ~ pm.prf("%.2e")          ==== "3.50e+00 +- 2.00e-01"

    val fr = 20 over 3
    val f2 = -36 over 70
    val badf = Int.MinValue over 1
    val ovrf = (1 over 2).overflowed
    T ~ fr             ==== ((20L << 32) | 3)
    T ~ Frac(20, 3)    ==== fr
    T ~ Frac(-18, 35)  ==== f2
    T ~ Frac.MinValue  ==== (-Int.MaxValue over 1)
    T ~ Frac.MaxValue  ==== (Int.MaxValue over 1)
    T ~ fr.unwrap      ==== ((20L << 32) | 3)
    T ~ fr.unwrap      ==== typed[Long]
    T ~ fr.numerator   ==== 20
    T ~ fr.denominator ==== 3
    T ~ fr.numer       ==== 20
    T ~ fr.denom       ==== 3
    T ~ fr.numer       ==== typed[Int]
    T ~ fr.denom       ==== typed[Int]
    T ~ fr.numerL      ==== 20
    T ~ fr.denomL      ==== 3
    T ~ fr.numerL      ==== typed[Long]
    T ~ fr.denomL      ==== typed[Long]
    T ~ badf.numer     ==== -Int.MaxValue
    T ~ badf.denom     ==== 1
    T ~ fr.isExact               ==== true
    T ~ badf.isExact             ==== false
    T ~ fr.inchecked               ==== false
    T ~ badf.inchecked             ==== true
    T ~ fr.overflowBit           ==== 0L
    T ~ badf.overflowBit         ==== 0x80000000L
    T ~ fr.overflowed.numer      ==== 20
    T ~ fr.overflowed.denom      ==== 3
    T ~ fr.overflowed.isExact    ==== false
    T ~ fr.noOverflow            ==== fr
    T ~ badf.noOverflow.isExact  ==== true
    T ~ badf.overflowed          ==== badf
    T ~ fr.toDouble ==== 6.66666666666666666666
    T ~ fr.toFloat  ==== 6.6666666666666f
    T ~ fr.f64      ==== fr.toDouble
    T ~ fr.f32      ==== fr.toFloat
    T ~ fr.abs           ==== fr
    T ~ (f2.abs != f2)   ==== true
    T ~ f2.abs.numer     ==== -f2.numer
    T ~ f2.abs.denom     ==== f2.denom
    T ~ badf.abs.numer   ==== -badf.numer
    T ~ badf.abs.inchecked ==== true
    T ~ ovrf.abs.inchecked ==== true
    T ~ (-fr).numer      ==== -fr.numer
    T ~ (-fr).denom      ==== fr.denom
    T ~ (-f2)            ==== (18 over 35)
    T ~ (-badf).numer    ==== -badf.numer
    T ~ (-badf).inchecked  ==== true
    T ~ (-ovrf).inchecked  ==== true
    T ~ fr.toInt         ==== 6
    T ~ f2.toInt         ==== 0
    T ~ fr.floor         ==== 6
    T ~ f2.floor         ==== -1
    T ~ fr.ceil          ==== 7
    T ~ f2.ceil          ==== 0
    T ~ fr.round         ==== 7
    T ~ f2.round         ==== -1
    T ~ (fr + 2)            ==== (26 over 3)
    T ~ (2 + fr)            ==== (fr + 2)
    T ~ (fr + f2)           ==== (646 over 105)
    T ~ ((9 over 20) + f2)  ==== (-9 over 140)
    T ~ (badf + f2)         ==== badf
    T ~ (fr + ovrf).inchecked ==== true
    T ~ (ovrf + fr).inchecked ==== true
    T ~ (ovrf + 2).inchecked  ==== true
    T ~ (2 + ovrf).inchecked  ==== true
    val bigplus = (2000000000 over 1) + (2000000001 over 2)
    T ~ bigplus.inchecked  ==== true
    T ~ bigplus.numer    ==== Int.MaxValue
    val largeplus = (2000000000 over 3) + (2000000001 over 4)
    T ~ (largeplus.numer < Int.MaxValue) ==== true
    T ~ largeplus.denom                  ==== 1
    T ~ largeplus.inchecked                ==== true
    val impreciseplus = (1293815714 over 318571605) + (781517561 over 2018675117)
    T ~ impreciseplus.inchecked        ==== true
    T ~ impreciseplus.denom          ==== 299463314
    T ~ (impreciseplus + fr).inchecked ==== true
    T ~ (fr + impreciseplus).inchecked ==== true
    val bittyplus = (3 over 2000000000) + (2 over 2000000001)
    T ~ (bittyplus.denom < Int.MaxValue) ==== true
    T ~ bittyplus.numer                  ==== 1
    T ~ bittyplus.inchecked                ==== true
    val zeroplus = (1 over 2000000000) + (-1 over 2000000001)
    T ~ zeroplus ==== (0 over 1).overflowed
    val bigplusinrange = (2000000000 over 1) + Int.MinValue
    T ~ bigplusinrange         ==== (-147483648 over 1)
    T ~ bigplusinrange.inchecked ==== false
    T ~ (fr - 2)            ==== (14 over 3)
    T ~ (2 - fr)            ==== (-14 over 3)
    T ~ ((9 over 20) - f2)  ==== (27 over 28)
    T ~ (badf - f2)         ==== badf
    T ~ (fr - ovrf).inchecked ==== true
    T ~ (ovrf - fr).inchecked ==== true
    T ~ (ovrf - 2).inchecked  ==== true
    T ~ (2 - ovrf).inchecked  ==== true
    val bigminus = (2000000000 over 1) - (-2000000001 over 2)
    T ~ bigminus.inchecked  ==== true
    T ~ bigminus.numer    ==== Int.MaxValue
    val largeminus = (-2000000000 over 3) - (2000000001 over 4)
    T ~ (largeminus.numer > -Int.MaxValue) ==== true
    T ~ largeminus.denom                   ==== 1
    T ~ largeminus.inchecked                 ==== true
    T ~ (largeplus + largeminus)           ==== 0x80000001L
    val impreciseminus = (1293815714 over 318571605) - (781517561 over 2018675117)
    T ~ impreciseminus.inchecked        ==== true
    T ~ impreciseminus.denom          ==== 299463314
    T ~ (impreciseminus - fr).inchecked ==== true
    T ~ (fr - impreciseminus).inchecked ==== true
    val bittyminus = (3 over 2000000000) - (-2 over 2000000001)
    T ~ (bittyminus.denom < Int.MaxValue) ==== true
    T ~ bittyminus.numer                  ==== 1
    T ~ bittyminus.inchecked                ==== true
    val zerominus = (1 over 2000000000) - (1 over 2000000001)
    T ~ zerominus ==== (0 over 1).overflowed
    val bigminusinrange = (-2000000000 over 1) - Int.MinValue
    T ~ bigminusinrange         ==== (147483648 over 1)
    T ~ bigminusinrange.inchecked ==== false
    T ~ (fr * 2)            ==== (40 over 3)
    T ~ (f2 * 5)            ==== (-18 over 7)
    T ~ (5 * fr)            ==== (fr * 5)
    T ~ (fr * f2)           ==== (-24 over 7)
    T ~ (badf * fr)         ==== badf
    T ~ (fr * ovrf).inchecked ==== true
    T ~ (ovrf * fr).inchecked ==== true
    T ~ (ovrf * 2).inchecked  ==== true
    T ~ (2 * ovrf).inchecked  ==== true
    val bigmuli = fr * 1000000000
    T ~ bigmuli ==== (Int.MaxValue over 1).overflowed
    val bigmulf = fr * (2000000001 over 2)
    T ~ bigmulf ==== (Int.MaxValue over 1).overflowed
    val largemul = fr * (400000000 over 3)
    T ~ (largemul.numer < Int.MaxValue) ==== true
    T ~ largemul.denom                  ==== 1
    T ~ largemul.inchecked                ==== true
    val imprecisemul = (1293815714 over 318571605) * (781517561 over 2018675117)
    T ~ imprecisemul.inchecked        ==== true
    T ~ imprecisemul.denom          ==== 871166007
    T ~ (imprecisemul * fr).inchecked ==== true
    T ~ (fr * imprecisemul).inchecked ==== true
    val bittymul = f2 * (2 over 1917918581)
    T ~ (bittymul.denom < Int.MaxValue) ==== true
    T ~ bittymul.numer                  ==== -1
    T ~ bittymul.inchecked                ==== true
    val zeromul = (1 over 10100101) * (-1 over 10101001)
    T ~ zeromul ==== (0 over 1).overflowed
    T ~ (fr / 2)            ==== (10 over 3)
    T ~ (f2 / 2)            ==== (-9 over 35)
    T ~ (2 / fr)            ==== (3 over 10)
    T ~ (fr / f2)           ==== (-350 over 27)
    T ~ (badf / -f2)        ==== badf
    T ~ (fr / ovrf).inchecked ==== true
    T ~ (ovrf / fr).inchecked ==== true
    T ~ (ovrf / 2).inchecked  ==== true
    T ~ (2 / ovrf).inchecked  ==== true
    val tinydivi = f2 / -2000000000
    T ~ tinydivi ==== (0 over 1).overflowed
    val bigdivf = (1000000 over 3) / (3 over 2000000)
    T ~ bigdivf ==== (Int.MaxValue over 1).overflowed
    val largediv = fr / (3 over 200000000)
    T ~ (largediv.numer < Int.MaxValue) ==== true
    T ~ largediv.denom                  ==== 1
    T ~ largediv.inchecked                ==== true
    val imprecisediv = (1293815714 over 318571605) / (2018675117 over 781517561)
    T ~ imprecisediv ==== imprecisemul
    val bittydiv = f2 / (1917918581 over 2)
    T ~ bittydiv ==== bittymul
    val zerodiv = (1 over 10100101) / (-10101001 over 1)
    T ~ zerodiv ==== zeromul
    val f3 = 91271528 over 19857151
    val f4 = 91956397 over 20006152
    T ~ fr.reciprocal            ==== (3 over 20)
    T ~ f2.reciprocal            ==== (-35 over 18)
    T ~ badf.reciprocal.inchecked  ==== true
    T ~ ovrf.reciprocal.inchecked  ==== true
    T ~ f2.reciprocal.reciprocal ==== f2
    T ~ (0 over 1).reciprocal    ==== (1 over 0)
    T ~ (fr =~= fr)           ==== true
    T ~ (ovrf =~= (1 over 2)) ==== true
    T ~ !(fr =~= f2)          ==== true
    T ~ !(f3 =~= f4)          ==== true
    T ~ (f2 < fr)             ==== true
    T ~ (fr < fr)             ==== false
    T ~ (fr < f2)             ==== false
    T ~ (f3 < f4)             ==== false
    T ~ (f4 < f3)             ==== true
    T ~ (-f3 < -f4)           ==== true
    T ~ (-f4 < -f3)           ==== false
    T ~ (f2 <= fr)            ==== true
    T ~ (fr <= fr)            ==== true
    T ~ (fr <= f2)            ==== false
    T ~ (f3 <= f4)            ==== false
    T ~ (f4 <= f3)            ==== true
    T ~ (-f3 <= -f4)          ==== true
    T ~ (-f4 <= -f3)          ==== false
    T ~ (f2 >= fr)            ==== false
    T ~ (fr >= fr)            ==== true
    T ~ (fr >= f2)            ==== true
    T ~ (f3 >= f4)            ==== true
    T ~ (f4 >= f3)            ==== false
    T ~ (-f3 >= -f4)          ==== false
    T ~ (-f4 >= -f3)          ==== true
    T ~ (f2 > fr)             ==== false
    T ~ (fr > fr)             ==== false
    T ~ (fr > f2)             ==== true
    T ~ (f3 > f4)             ==== true
    T ~ (f4 > f3)             ==== false
    T ~ (-f3 > -f4)           ==== false
    T ~ (-f4 > -f3)           ==== true
    T ~ (f2 max fr)           ==== fr
    T ~ (fr max f2)           ==== fr
    T ~ (f2 max ovrf)         ==== ovrf
    T ~ (fr max ovrf)         ==== fr.overflowed
    T ~ (ovrf max f2)         ==== ovrf
    T ~ (ovrf max fr)         ==== fr.overflowed
    T ~ (f3 max f4)           ==== f3
    T ~ (f4 max f3)           ==== f3
    T ~ (f2 min fr)           ==== f2
    T ~ (fr min f2)           ==== f2
    T ~ (f2 min ovrf)         ==== f2.overflowed
    T ~ (fr min ovrf)         ==== ovrf
    T ~ (ovrf min f2)         ==== f2.overflowed
    T ~ (ovrf min fr)         ==== ovrf
    T ~ (f3 min f4)           ==== f4
    T ~ (f4 min f3)           ==== f4
    T ~ fr.pr                 ==== "20/3"
    T ~ f2.pr                 ==== "-18/35"
    T ~ ovrf.pr               ==== "~(1/2)"
    T ~ List(fr, f2, f3, f4, ovrf).sorted ==== List(f2, ovrf, f4, f3, fr)
    T ~ Frac.approx(fr.f64) ==== fr
    T ~ Frac.approx(f2.f64) ==== f2
    T ~ Frac.approx(2.5)    ==== (5 over 2)
    T ~ Frac.approx(1e20)   ==== (Int.MaxValue over 1)
    T ~ Frac.approx(-1e20)  ==== (-Int.MaxValue over 1)
    T ~ Frac.approx(Int.MaxValue.toDouble - 0.7, markInchecked = true) ==== (Int.MaxValue - 1 over 1).overflowed
    T ~ Frac.approx(0.3125) ==== (10 over 32)
    T ~ Frac.approx(-1.0/0) ==== (-1 over 0)
    T ~ Frac.approx(0.0/0)  ==== (0 over 1).overflowed
    T ~ Frac.scaleClamped(100000000000L, -15 over 15)         ==== -100000000000L
    T ~ Frac.scaleClamped(100000000000L, Int.MaxValue over 1) ==== Long.MaxValue
    T ~ Frac.scaleClamped(100000000000L, Int.MinValue over 1) ==== Long.MinValue
    T ~ Frac.scaleExactly(100000000000L, -15 over 15)         ==== -100000000000L
    T ~ Frac.scaleExactly(100000000000L, Int.MaxValue over 1) ==== thrown[ArithmeticException]
    T ~ Frac.scaleExactly(100000000000L, Int.MinValue over 1) ==== thrown[ArithmeticException]

  @Test
  def packedBitTest(): Unit = packedTester.packedBitTest()

  @Test
  def packedHexTest(): Unit = packedTester.packedHexTest()

  @Test
  def packedPrimitiveTest(): Unit = packedTester.packedPrimitiveTest()

  @Test
  def temporalTestCreation(): Unit = temporalTester.testCreation()

  @Test
  def temporalTestDuration(): Unit = temporalTester.testDuration()

  @Test
  def temporalTestNanoDuration(): Unit = temporalTester.testNanoDuration()

  @Test
  def temporalTestDoubleDuration(): Unit = temporalTester.testDoubleDuration()

  @Test
  def temporalTestNanoInstant(): Unit = temporalTester.testNanoInstant()

  @Test
  def temporalTestDoubleInstant(): Unit = temporalTester.testDoubleInstant()

  @Test
  def temporalTestInstant(): Unit = temporalTester.testInstant()

  @Test
  def temporalTestLocalDateTime(): Unit = temporalTester.testLocalDateTime()

  @Test
  def temporalTestOffsetDateTime(): Unit = temporalTester.testOffsetDateTime()

  @Test
  def temporalTestZonedDateTime(): Unit = temporalTester.testZonedDateTime()

  @Test
  def temporalTestFileTime(): Unit = temporalTester.testFileTime()
}
object MathsTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
