// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022 Rex Kerr and Calico Life Sciences LLC.

package kse.flow.test

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

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
    T ~ cdfFDist(5, 7)(14.142) =~~= 0.99847312214627792997
    T ~ cdfFDist(18, 11)(0.29) =~~= 0.0097966801396012063383

    val b: Byte = -8
    T ~ b.clamp(-12, -4)   ==== (-8: Byte)
    T ~ b.clamp(13, 100)   ==== (13: Byte)
    T ~ b.clamp(-99, -9)   ==== (-9: Byte)
    T ~ b.clamp(55, -99)   ==== (55: Byte)
    T ~ b.in(-12, 4)       ==== true
    T ~ b.in(13, 100)      ==== false
    T ~ b.in(-99, -9)      ==== false
    T ~ b.in(44, -12)      ==== false
    T ~ b.toUInt           ==== 248
    T ~ b.toUInt           ==== typed[Int]
    T ~ b.toULong          ==== 248L
    T ~ b.toULong          ==== typed[Long]
    T ~ b.sq               ==== 64.0
    T ~ b.sq               ==== typed[Double]
    T ~ b.sign             ==== -1
    T ~ (-b).toByte.sign   ==== 1
    T ~ (b/10).toByte.sign ==== 0
    T ~ b.sign             ==== typed[Int]

    val s: Short = -88
    T ~ s.clamp(-915, -4)    ==== (-88: Short)
    T ~ s.clamp(-44, 333)    ==== (-44: Short)
    T ~ s.clamp(-99, -91)    ==== (-91: Short)
    T ~ s.clamp(333, -44)    ==== (333: Short)
    T ~ s.in(-915, -4)       ==== true
    T ~ s.in(-44, 333)       ==== false
    T ~ s.in(-99, -91)       ==== false
    T ~ s.in(-4, -915)       ==== false
    T ~ s.toUInt             ==== 65448
    T ~ s.toUInt             ==== typed[Int]
    T ~ s.toULong            ==== 65448L
    T ~ s.toULong            ==== typed[Long]
    T ~ s.hexString          ==== "FFA8"
    T ~ s.hiHexString        ==== "FFA8"
    T ~ s.loHexString        ==== "ffa8"
    T ~ (1: Short).hexString ==== "0001"
    T ~ s.sq                 ==== 7744.0
    T ~ s.sq                 ==== typed[Double]
    T ~ s.sign               ==== -1
    T ~ (-s).toShort.sign    ==== 1
    T ~ (s/100).toShort.sign ==== 0
    T ~ s.sign               ==== typed[Int]

    val c = 'n'
    T ~ c.clamp('a', 'w') ==== 'n'
    T ~ c.clamp('A', 'W') ==== 'W'
    T ~ c.clamp('q', 'z') ==== 'q'
    T ~ c.clamp('z', 'A') ==== 'z'
    T ~ c.in('a', 'w')    ==== true
    T ~ c.in('A', 'W')    ==== false
    T ~ c.in('q', 'z')    ==== false
    T ~ c.in('w', 'a')    ==== false
    T ~ c.hexString       ==== "006E"
    T ~ c.hiHexString     ==== "006E"
    T ~ c.loHexString     ==== "006e"

    val i = -8888
    T ~ i.clamp(-9999, -7777) ==== -8888
    T ~ i.clamp(-9999, -9876) ==== -9876
    T ~ i.clamp(-7777, 12345) ==== -7777
    T ~ i.clamp(-7777, -9999) ==== -7777
    T ~ i.in(-9999, -7777)    ==== true
    T ~ i.in(-9999, -9876)    ==== false
    T ~ i.in(-7777, 12345)    ==== false
    T ~ i.in(-7777, -9999)    ==== false
    T ~ i.toULong             ==== 4294958408L
    T ~ i.toULong             ==== typed[Long]
    T ~ i.hexString           ==== "FFFFDD48"
    T ~ i.hiHexString         ==== "FFFFDD48"
    T ~ i.loHexString         ==== "ffffdd48"
    T ~ 11.hexString          ==== "0000000B"
    T ~ 1067030938.bitsF      ==== 1.2f
    T ~ i.sq                  ==== 78996544.0
    T ~ i.sq                  ==== typed[Double]
    T ~ i.sign                ==== -1
    T ~ (-i).sign             ==== 1
    T ~ (i/10000).sign        ==== 0
    T ~ i.sign                ==== typed[Int]


    val l = 42L
    T ~ l.clamp(-100L, 100L)       ==== 42L
    T ~ l.clamp(-100L, -10L)       ==== -10L
    T ~ l.clamp(72L, 43210L)       ==== 72L
    T ~ l.clamp(100L, -100L)       ==== 100L
    T ~ l.in(-100L, 100L)          ==== true
    T ~ l.in(-100L, -10L)          ==== false
    T ~ l.in(72L, 43210L)          ==== false
    T ~ l.in(100L, -100L)          ==== false
    T ~ l.hexString                ==== "000000000000002A"
    T ~ l.hiHexString              ==== "000000000000002A"
    T ~ l.loHexString              ==== "000000000000002a"
    T ~ 4608083138725491507L.bitsD ==== 1.2
    T ~ l.sq                       ==== 1764.0
    T ~ l.sq                       ==== typed[Double]
    T ~ l.sign                     ==== 1
    T ~ (-l).sign                  ==== -1
    T ~ (l/100).sign               ==== 0
    T ~ l.sign                     ==== typed[Long]

    val f = 1.2f
    val fnan = Float.NaN
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
    T ~ f.closeTo(f+1e-7f)  ==== true
    T ~ f.closeTo(f+1e-5f)  ==== false
    T ~ f.closeTo(1f,1f,1f) ==== true
    T ~ f.bitsI             ==== 1067030938
    T ~ f.f64               ==== 1.2f.toDouble
    T ~ f.f64               ==== typed[Double]

    val d = 1.2
    val dnan = Double.NaN
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
    T ~ d.closeTo(d+1e-13)    ==== true
    T ~ d.closeTo(d+1e-8)     ==== false
    T ~ d.closeTo(1.0,1,1)    ==== true
    T ~ d.bitsL               ==== 4608083138725491507L
    T ~ d.f32                 ==== 1.2f
    T ~ d.f32                 ==== typed[Float]

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
    T ~ fr.isExact               ==== true
    T ~ badf.isExact             ==== false
    T ~ fr.inexact               ==== false
    T ~ badf.inexact             ==== true
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
    T ~ badf.abs.inexact ==== true
    T ~ ovrf.abs.inexact ==== true
    T ~ (-fr).numer      ==== -fr.numer
    T ~ (-fr).denom      ==== fr.denom
    T ~ (-f2)            ==== (18 over 35)
    T ~ (-badf).numer    ==== -badf.numer
    T ~ (-badf).inexact  ==== true
    T ~ (-ovrf).inexact  ==== true
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
    T ~ (fr + ovrf).inexact ==== true
    T ~ (ovrf + fr).inexact ==== true
    T ~ (ovrf + 2).inexact  ==== true
    T ~ (2 + ovrf).inexact  ==== true
    val bigplus = (2000000000 over 1) + (2000000001 over 2)
    T ~ bigplus.inexact  ==== true
    T ~ bigplus.numer    ==== Int.MaxValue
    val largeplus = (2000000000 over 3) + (2000000001 over 4)
    T ~ (largeplus.numer < Int.MaxValue) ==== true
    T ~ largeplus.denom                  ==== 1
    T ~ largeplus.inexact                ==== true
    val impreciseplus = (1293815714 over 318571605) + (781517561 over 2018675117)
    T ~ impreciseplus.inexact        ==== true
    T ~ impreciseplus.denom          ==== 299463314
    T ~ (impreciseplus + fr).inexact ==== true
    T ~ (fr + impreciseplus).inexact ==== true
    val bittyplus = (3 over 2000000000) + (2 over 2000000001)
    T ~ (bittyplus.denom < Int.MaxValue) ==== true
    T ~ bittyplus.numer                  ==== 1
    T ~ bittyplus.inexact                ==== true
    val zeroplus = (1 over 2000000000) + (-1 over 2000000001)
    T ~ zeroplus ==== (0 over 1).overflowed
    val bigplusinrange = (2000000000 over 1) + Int.MinValue
    T ~ bigplusinrange         ==== (-147483648 over 1)
    T ~ bigplusinrange.inexact ==== false
    T ~ (fr - 2)            ==== (14 over 3)
    T ~ (2 - fr)            ==== (-14 over 3)
    T ~ ((9 over 20) - f2)  ==== (27 over 28)
    T ~ (badf - f2)         ==== badf
    T ~ (fr - ovrf).inexact ==== true
    T ~ (ovrf - fr).inexact ==== true
    T ~ (ovrf - 2).inexact  ==== true
    T ~ (2 - ovrf).inexact  ==== true
    val bigminus = (2000000000 over 1) - (-2000000001 over 2)
    T ~ bigminus.inexact  ==== true
    T ~ bigminus.numer    ==== Int.MaxValue
    val largeminus = (-2000000000 over 3) - (2000000001 over 4)
    T ~ (largeminus.numer > -Int.MaxValue) ==== true
    T ~ largeminus.denom                   ==== 1
    T ~ largeminus.inexact                 ==== true
    T ~ (largeplus + largeminus)           ==== 0x80000001L
    val impreciseminus = (1293815714 over 318571605) - (781517561 over 2018675117)
    T ~ impreciseminus.inexact        ==== true
    T ~ impreciseminus.denom          ==== 299463314
    T ~ (impreciseminus - fr).inexact ==== true
    T ~ (fr - impreciseminus).inexact ==== true
    val bittyminus = (3 over 2000000000) - (-2 over 2000000001)
    T ~ (bittyminus.denom < Int.MaxValue) ==== true
    T ~ bittyminus.numer                  ==== 1
    T ~ bittyminus.inexact                ==== true
    val zerominus = (1 over 2000000000) - (1 over 2000000001)
    T ~ zerominus ==== (0 over 1).overflowed
    val bigminusinrange = (-2000000000 over 1) - Int.MinValue
    T ~ bigminusinrange         ==== (147483648 over 1)
    T ~ bigminusinrange.inexact ==== false
    T ~ (fr * 2)            ==== (40 over 3)
    T ~ (f2 * 5)            ==== (-18 over 7)
    T ~ (5 * fr)            ==== (fr * 5)
    T ~ (fr * f2)           ==== (-24 over 7)
    T ~ (badf * fr)         ==== badf
    T ~ (fr * ovrf).inexact ==== true
    T ~ (ovrf * fr).inexact ==== true
    T ~ (ovrf * 2).inexact  ==== true
    T ~ (2 * ovrf).inexact  ==== true
    val bigmuli = fr * 1000000000
    T ~ bigmuli ==== (Int.MaxValue over 1).overflowed
    val bigmulf = fr * (2000000001 over 2)
    T ~ bigmulf ==== (Int.MaxValue over 1).overflowed
    val largemul = fr * (400000000 over 3)
    T ~ (largemul.numer < Int.MaxValue) ==== true
    T ~ largemul.denom                  ==== 1
    T ~ largemul.inexact                ==== true
    val imprecisemul = (1293815714 over 318571605) * (781517561 over 2018675117)
    T ~ imprecisemul.inexact        ==== true
    T ~ imprecisemul.denom          ==== 871166007
    T ~ (imprecisemul * fr).inexact ==== true
    T ~ (fr * imprecisemul).inexact ==== true
    val bittymul = f2 * (2 over 1917918581)
    T ~ (bittymul.denom < Int.MaxValue) ==== true
    T ~ bittymul.numer                  ==== -1
    T ~ bittymul.inexact                ==== true
    val zeromul = (1 over 10100101) * (-1 over 10101001)
    T ~ zeromul ==== (0 over 1).overflowed
    T ~ (fr / 2)            ==== (10 over 3)
    T ~ (f2 / 2)            ==== (-9 over 35)
    T ~ (2 / fr)            ==== (3 over 10)
    T ~ (fr / f2)           ==== (-350 over 27)
    T ~ (badf / -f2)        ==== badf
    T ~ (fr / ovrf).inexact ==== true
    T ~ (ovrf / fr).inexact ==== true
    T ~ (ovrf / 2).inexact  ==== true
    T ~ (2 / ovrf).inexact  ==== true
    val tinydivi = f2 / -2000000000
    T ~ tinydivi ==== (0 over 1).overflowed
    val bigdivf = (1000000 over 3) / (3 over 2000000)
    T ~ bigdivf ==== (Int.MaxValue over 1).overflowed
    val largediv = fr / (3 over 200000000)
    T ~ (largediv.numer < Int.MaxValue) ==== true
    T ~ largediv.denom                  ==== 1
    T ~ largediv.inexact                ==== true
    val imprecisediv = (1293815714 over 318571605) / (2018675117 over 781517561)
    T ~ imprecisediv ==== imprecisemul
    val bittydiv = f2 / (1917918581 over 2)
    T ~ bittydiv ==== bittymul
    val zerodiv = (1 over 10100101) / (-10101001 over 1)
    T ~ zerodiv ==== zeromul
    val f3 = 91271528 over 19857151
    val f4 = 91956397 over 20006152
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


  @Test
  def packedBitTest(): Unit =
    T ~ Bit(1) ==== 1
    T ~ Bit(0) ==== Bit.flip(1)
    T ~ Bit(1) ==== Bit(true)
    T ~ Bit(0) ==== Bit(false)
    T ~ Bit.hex(      0) ==== Hex(0)
    T ~ Bit.hex(      1) ==== Hex(1)
    T ~ Bit.hex(    1,0) ==== Hex(2)
    T ~ Bit.hex(    0,1) ==== Hex(1)
    T ~ Bit.hex(  1,1,0) ==== Hex(6)
    T ~ Bit.hex(  0,0,1) ==== Hex(1)
    T ~ Bit.hex(0,1,1,0) ==== Hex(6)
    T ~ Bit.hex(1,0,0,1) ==== Hex(9)
    T ~ Bit.B(              0) ==== (0x00: Byte)
    T ~ Bit.B(              1) ==== (0x01: Byte)
    T ~ Bit.B(            1,0) ==== (0x02: Byte)
    T ~ Bit.B(            0,1) ==== (0x01: Byte)
    T ~ Bit.B(          1,1,0) ==== (0x06: Byte)
    T ~ Bit.B(          0,0,1) ==== (0x01: Byte)
    T ~ Bit.B(        0,1,1,0) ==== (0x06: Byte)
    T ~ Bit.B(        1,0,0,1) ==== (0x09: Byte)
    T ~ Bit.B(      0,0,1,1,0) ==== (0x06: Byte)
    T ~ Bit.B(      1,1,0,0,1) ==== (0x19: Byte)
    T ~ Bit.B(    1,0,0,1,1,0) ==== (0x26: Byte)
    T ~ Bit.B(    0,1,1,0,0,1) ==== (0x19: Byte)
    T ~ Bit.B(  0,1,0,0,1,1,0) ==== (0x26: Byte)
    T ~ Bit.B(  1,0,1,1,0,0,1) ==== (0x59: Byte)
    T ~ Bit.B(0,0,1,0,0,1,1,0) ==== (0x26: Byte)
    T ~ Bit.B(1,1,0,1,1,0,0,1) ==== (0xD9.toByte)
    T ~ Bit.S(                              0) ==== (0x0000: Short)
    T ~ Bit.S(                              1) ==== (0x0001: Short)
    T ~ Bit.S(                            1,0) ==== (0x0002: Short)
    T ~ Bit.S(                            0,1) ==== (0x0001: Short)
    T ~ Bit.S(                          1,1,0) ==== (0x0006: Short)
    T ~ Bit.S(                          0,0,1) ==== (0x0001: Short)
    T ~ Bit.S(                        0,1,1,0) ==== (0x0006: Short)
    T ~ Bit.S(                        1,0,0,1) ==== (0x0009: Short)
    T ~ Bit.S(                      0,0,1,1,0) ==== (0x0006: Short)
    T ~ Bit.S(                      1,1,0,0,1) ==== (0x0019: Short)
    T ~ Bit.S(                    1,0,0,1,1,0) ==== (0x0026: Short)
    T ~ Bit.S(                    0,1,1,0,0,1) ==== (0x0019: Short)
    T ~ Bit.S(                  0,1,0,0,1,1,0) ==== (0x0026: Short)
    T ~ Bit.S(                  1,0,1,1,0,0,1) ==== (0x0059: Short)
    T ~ Bit.S(                0,0,1,0,0,1,1,0) ==== (0x0026: Short)
    T ~ Bit.S(                1,1,0,1,1,0,0,1) ==== (0x00D9: Short)
    T ~ Bit.S(              1,0,0,1,0,0,1,1,0) ==== (0x0126: Short)
    T ~ Bit.S(              0,1,1,0,1,1,0,0,1) ==== (0x00D9: Short)
    T ~ Bit.S(            1,1,0,0,1,0,0,1,1,0) ==== (0x0326: Short)
    T ~ Bit.S(            0,0,1,1,0,1,1,0,0,1) ==== (0x00D9: Short)
    T ~ Bit.S(          0,1,1,0,0,1,0,0,1,1,0) ==== (0x0326: Short)
    T ~ Bit.S(          1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9: Short)
    T ~ Bit.S(        1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x0B26: Short)
    T ~ Bit.S(        0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9: Short)
    T ~ Bit.S(      1,1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x1B26: Short)
    T ~ Bit.S(      0,0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9: Short)
    T ~ Bit.S(    1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x3B26: Short)
    T ~ Bit.S(    0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9: Short)
    T ~ Bit.S(  1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x7B26: Short)
    T ~ Bit.S(  0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9: Short)
    T ~ Bit.S(0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x7B26: Short)
    T ~ Bit.S(1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x84D9.toShort)
    T ~ Bit.C(                              0) ==== (0x0000.toChar)
    T ~ Bit.C(                              1) ==== (0x0001.toChar)
    T ~ Bit.C(                            1,0) ==== (0x0002.toChar)
    T ~ Bit.C(                            0,1) ==== (0x0001.toChar)
    T ~ Bit.C(                          1,1,0) ==== (0x0006.toChar)
    T ~ Bit.C(                          0,0,1) ==== (0x0001.toChar)
    T ~ Bit.C(                        0,1,1,0) ==== (0x0006.toChar)
    T ~ Bit.C(                        1,0,0,1) ==== (0x0009.toChar)
    T ~ Bit.C(                      0,0,1,1,0) ==== (0x0006.toChar)
    T ~ Bit.C(                      1,1,0,0,1) ==== (0x0019.toChar)
    T ~ Bit.C(                    1,0,0,1,1,0) ==== (0x0026.toChar)
    T ~ Bit.C(                    0,1,1,0,0,1) ==== (0x0019.toChar)
    T ~ Bit.C(                  0,1,0,0,1,1,0) ==== (0x0026.toChar)
    T ~ Bit.C(                  1,0,1,1,0,0,1) ==== (0x0059.toChar)
    T ~ Bit.C(                0,0,1,0,0,1,1,0) ==== (0x0026.toChar)
    T ~ Bit.C(                1,1,0,1,1,0,0,1) ==== (0x00D9.toChar)
    T ~ Bit.C(              1,0,0,1,0,0,1,1,0) ==== (0x0126.toChar)
    T ~ Bit.C(              0,1,1,0,1,1,0,0,1) ==== (0x00D9.toChar)
    T ~ Bit.C(            1,1,0,0,1,0,0,1,1,0) ==== (0x0326.toChar)
    T ~ Bit.C(            0,0,1,1,0,1,1,0,0,1) ==== (0x00D9.toChar)
    T ~ Bit.C(          0,1,1,0,0,1,0,0,1,1,0) ==== (0x0326.toChar)
    T ~ Bit.C(          1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9.toChar)
    T ~ Bit.C(        1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x0B26.toChar)
    T ~ Bit.C(        0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9.toChar)
    T ~ Bit.C(      1,1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x1B26.toChar)
    T ~ Bit.C(      0,0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9.toChar)
    T ~ Bit.C(    1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x3B26.toChar)
    T ~ Bit.C(    0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9.toChar)
    T ~ Bit.C(  1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x7B26.toChar)
    T ~ Bit.C(  0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x04D9.toChar)
    T ~ Bit.C(0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== (0x7B26.toChar)
    T ~ Bit.C(1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== (0x84D9.toChar)
    T ~ Bit.I(                                                              0) ==== 0x00000000
    T ~ Bit.I(                                                              1) ==== 0x00000001
    T ~ Bit.I(                                                            1,0) ==== 0x00000002
    T ~ Bit.I(                                                            0,1) ==== 0x00000001
    T ~ Bit.I(                                                          1,1,0) ==== 0x00000006
    T ~ Bit.I(                                                          0,0,1) ==== 0x00000001
    T ~ Bit.I(                                                        0,1,1,0) ==== 0x00000006
    T ~ Bit.I(                                                        1,0,0,1) ==== 0x00000009
    T ~ Bit.I(                                                      0,0,1,1,0) ==== 0x00000006
    T ~ Bit.I(                                                      1,1,0,0,1) ==== 0x00000019
    T ~ Bit.I(                                                    1,0,0,1,1,0) ==== 0x00000026
    T ~ Bit.I(                                                    0,1,1,0,0,1) ==== 0x00000019
    T ~ Bit.I(                                                  0,1,0,0,1,1,0) ==== 0x00000026
    T ~ Bit.I(                                                  1,0,1,1,0,0,1) ==== 0x00000059
    T ~ Bit.I(                                                0,0,1,0,0,1,1,0) ==== 0x00000026
    T ~ Bit.I(                                                1,1,0,1,1,0,0,1) ==== 0x000000D9
    T ~ Bit.I(                                              1,0,0,1,0,0,1,1,0) ==== 0x00000126
    T ~ Bit.I(                                              0,1,1,0,1,1,0,0,1) ==== 0x000000D9
    T ~ Bit.I(                                            1,1,0,0,1,0,0,1,1,0) ==== 0x00000326
    T ~ Bit.I(                                            0,0,1,1,0,1,1,0,0,1) ==== 0x000000D9
    T ~ Bit.I(                                          0,1,1,0,0,1,0,0,1,1,0) ==== 0x00000326
    T ~ Bit.I(                                          1,0,0,1,1,0,1,1,0,0,1) ==== 0x000004D9
    T ~ Bit.I(                                        1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00000B26
    T ~ Bit.I(                                        0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x000004D9
    T ~ Bit.I(                                      1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00001B26
    T ~ Bit.I(                                      0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x000004D9
    T ~ Bit.I(                                    1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00003B26
    T ~ Bit.I(                                    0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x000004D9
    T ~ Bit.I(                                  1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00007B26
    T ~ Bit.I(                                  0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x000004D9
    T ~ Bit.I(                                0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00007B26
    T ~ Bit.I(                                1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x000084D9
    T ~ Bit.I(                              1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00017B26
    T ~ Bit.I(                              0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x000084D9
    T ~ Bit.I(                            1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00037B26
    T ~ Bit.I(                            0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x000084D9
    T ~ Bit.I(                          0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00037B26
    T ~ Bit.I(                          1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x000484D9
    T ~ Bit.I(                        0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00037B26
    T ~ Bit.I(                        1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x000C84D9
    T ~ Bit.I(                      0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00037B26
    T ~ Bit.I(                      1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x001C84D9
    T ~ Bit.I(                    1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00237B26
    T ~ Bit.I(                    0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x001C84D9
    T ~ Bit.I(                  1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00637B26
    T ~ Bit.I(                  0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x001C84D9
    T ~ Bit.I(                1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00E37B26
    T ~ Bit.I(                0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x001C84D9
    T ~ Bit.I(              0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x00E37B26
    T ~ Bit.I(              1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x011C84D9
    T ~ Bit.I(            1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x02E37B26
    T ~ Bit.I(            0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x011C84D9
    T ~ Bit.I(          0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x02E37B26
    T ~ Bit.I(          1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x051C84D9
    T ~ Bit.I(        1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0AE37B26
    T ~ Bit.I(        0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x051C84D9
    T ~ Bit.I(      0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0AE37B26
    T ~ Bit.I(      1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x151C84D9
    T ~ Bit.I(    0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0AE37B26
    T ~ Bit.I(    1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x351C84D9
    T ~ Bit.I(  0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0AE37B26
    T ~ Bit.I(  1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x751C84D9
    T ~ Bit.I(0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0AE37B26
    T ~ Bit.I(1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0xF51C84D9
    T ~ Bit.L(                                                                                                                              0) ==== 0x0000000000000000L
    T ~ Bit.L(                                                                                                                              1) ==== 0x0000000000000001L
    T ~ Bit.L(                                                                                                                            1,0) ==== 0x0000000000000002L
    T ~ Bit.L(                                                                                                                            0,1) ==== 0x0000000000000001L
    T ~ Bit.L(                                                                                                                          1,1,0) ==== 0x0000000000000006L
    T ~ Bit.L(                                                                                                                          0,0,1) ==== 0x0000000000000001L
    T ~ Bit.L(                                                                                                                        0,1,1,0) ==== 0x0000000000000006L
    T ~ Bit.L(                                                                                                                        1,0,0,1) ==== 0x0000000000000009L
    T ~ Bit.L(                                                                                                                      0,0,1,1,0) ==== 0x0000000000000006L
    T ~ Bit.L(                                                                                                                      1,1,0,0,1) ==== 0x0000000000000019L
    T ~ Bit.L(                                                                                                                    1,0,0,1,1,0) ==== 0x0000000000000026L
    T ~ Bit.L(                                                                                                                    0,1,1,0,0,1) ==== 0x0000000000000019L
    T ~ Bit.L(                                                                                                                  0,1,0,0,1,1,0) ==== 0x0000000000000026L
    T ~ Bit.L(                                                                                                                  1,0,1,1,0,0,1) ==== 0x0000000000000059L
    T ~ Bit.L(                                                                                                                0,0,1,0,0,1,1,0) ==== 0x0000000000000026L
    T ~ Bit.L(                                                                                                                1,1,0,1,1,0,0,1) ==== 0x00000000000000D9L
    T ~ Bit.L(                                                                                                              1,0,0,1,0,0,1,1,0) ==== 0x0000000000000126L
    T ~ Bit.L(                                                                                                              0,1,1,0,1,1,0,0,1) ==== 0x00000000000000D9L
    T ~ Bit.L(                                                                                                            1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000000326L
    T ~ Bit.L(                                                                                                            0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000000D9L
    T ~ Bit.L(                                                                                                          0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000000326L
    T ~ Bit.L(                                                                                                          1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000004D9L
    T ~ Bit.L(                                                                                                        1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000000B26L
    T ~ Bit.L(                                                                                                        0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000004D9L
    T ~ Bit.L(                                                                                                      1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000001B26L
    T ~ Bit.L(                                                                                                      0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000004D9L
    T ~ Bit.L(                                                                                                    1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000003B26L
    T ~ Bit.L(                                                                                                    0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000004D9L
    T ~ Bit.L(                                                                                                  1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000007B26L
    T ~ Bit.L(                                                                                                  0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000004D9L
    T ~ Bit.L(                                                                                                0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000007B26L
    T ~ Bit.L(                                                                                                1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000084D9L
    T ~ Bit.L(                                                                                              1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000017B26L
    T ~ Bit.L(                                                                                              0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000084D9L
    T ~ Bit.L(                                                                                            1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000037B26L
    T ~ Bit.L(                                                                                            0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000084D9L
    T ~ Bit.L(                                                                                          0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000037B26L
    T ~ Bit.L(                                                                                          1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000484D9L
    T ~ Bit.L(                                                                                        0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000037B26L
    T ~ Bit.L(                                                                                        1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000000C84D9L
    T ~ Bit.L(                                                                                      0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000037B26L
    T ~ Bit.L(                                                                                      1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000001C84D9L
    T ~ Bit.L(                                                                                    1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000237B26L
    T ~ Bit.L(                                                                                    0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000001C84D9L
    T ~ Bit.L(                                                                                  1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000637B26L
    T ~ Bit.L(                                                                                  0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000001C84D9L
    T ~ Bit.L(                                                                                1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000E37B26L
    T ~ Bit.L(                                                                                0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000001C84D9L
    T ~ Bit.L(                                                                              0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000000E37B26L
    T ~ Bit.L(                                                                              1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000011C84D9L
    T ~ Bit.L(                                                                            1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000002E37B26L
    T ~ Bit.L(                                                                            0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000011C84D9L
    T ~ Bit.L(                                                                          0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x0000000002E37B26L
    T ~ Bit.L(                                                                          1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000051C84D9L
    T ~ Bit.L(                                                                        1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000000AE37B26L
    T ~ Bit.L(                                                                        0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000051C84D9L
    T ~ Bit.L(                                                                      0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000000AE37B26L
    T ~ Bit.L(                                                                      1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000151C84D9L
    T ~ Bit.L(                                                                    0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000000AE37B26L
    T ~ Bit.L(                                                                    1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000351C84D9L
    T ~ Bit.L(                                                                  0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000000AE37B26L
    T ~ Bit.L(                                                                  1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000751C84D9L
    T ~ Bit.L(                                                                0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000000AE37B26L
    T ~ Bit.L(                                                                1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000F51C84D9L
    T ~ Bit.L(                                                                0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000000AE37B26L
    T ~ Bit.L(                                                                1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000F51C84D9L
    T ~ Bit.L(                                                              1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000010AE37B26L
    T ~ Bit.L(                                                              0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000000F51C84D9L
    T ~ Bit.L(                                                            0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000010AE37B26L
    T ~ Bit.L(                                                            1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000002F51C84D9L
    T ~ Bit.L(                                                          0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000010AE37B26L
    T ~ Bit.L(                                                          1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000006F51C84D9L
    T ~ Bit.L(                                                        1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000090AE37B26L
    T ~ Bit.L(                                                        0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000006F51C84D9L
    T ~ Bit.L(                                                      1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000190AE37B26L
    T ~ Bit.L(                                                      0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000006F51C84D9L
    T ~ Bit.L(                                                    0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000190AE37B26L
    T ~ Bit.L(                                                    1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000026F51C84D9L
    T ~ Bit.L(                                                  1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000590AE37B26L
    T ~ Bit.L(                                                  0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000026F51C84D9L
    T ~ Bit.L(                                                1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000D90AE37B26L
    T ~ Bit.L(                                                0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000026F51C84D9L
    T ~ Bit.L(                                              0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000D90AE37B26L
    T ~ Bit.L(                                              1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000126F51C84D9L
    T ~ Bit.L(                                            0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000000D90AE37B26L
    T ~ Bit.L(                                            1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000326F51C84D9L
    T ~ Bit.L(                                          1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000004D90AE37B26L
    T ~ Bit.L(                                          0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000326F51C84D9L
    T ~ Bit.L(                                        0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000004D90AE37B26L
    T ~ Bit.L(                                        1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00000B26F51C84D9L
    T ~ Bit.L(                                      0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000004D90AE37B26L
    T ~ Bit.L(                                      1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00001B26F51C84D9L
    T ~ Bit.L(                                    0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000004D90AE37B26L
    T ~ Bit.L(                                    1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00003B26F51C84D9L
    T ~ Bit.L(                                  0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000004D90AE37B26L
    T ~ Bit.L(                                  1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00007B26F51C84D9L
    T ~ Bit.L(                                1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000084D90AE37B26L
    T ~ Bit.L(                                0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00007B26F51C84D9L
    T ~ Bit.L(                              0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000084D90AE37B26L
    T ~ Bit.L(                              1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00017B26F51C84D9L
    T ~ Bit.L(                            0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000084D90AE37B26L
    T ~ Bit.L(                            1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00037B26F51C84D9L
    T ~ Bit.L(                          1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000484D90AE37B26L
    T ~ Bit.L(                          0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00037B26F51C84D9L
    T ~ Bit.L(                        1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x000C84D90AE37B26L
    T ~ Bit.L(                        0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00037B26F51C84D9L
    T ~ Bit.L(                      1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x001C84D90AE37B26L
    T ~ Bit.L(                      0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00037B26F51C84D9L
    T ~ Bit.L(                    0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x001C84D90AE37B26L
    T ~ Bit.L(                    1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00237B26F51C84D9L
    T ~ Bit.L(                  0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x001C84D90AE37B26L
    T ~ Bit.L(                  1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00637B26F51C84D9L
    T ~ Bit.L(                0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x001C84D90AE37B26L
    T ~ Bit.L(                1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00E37B26F51C84D9L
    T ~ Bit.L(              1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x011C84D90AE37B26L
    T ~ Bit.L(              0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x00E37B26F51C84D9L
    T ~ Bit.L(            0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x011C84D90AE37B26L
    T ~ Bit.L(            1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x02E37B26F51C84D9L
    T ~ Bit.L(          1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x051C84D90AE37B26L
    T ~ Bit.L(          0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x02E37B26F51C84D9L
    T ~ Bit.L(        0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x051C84D90AE37B26L
    T ~ Bit.L(        1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x0AE37B26F51C84D9L
    T ~ Bit.L(      1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x151C84D90AE37B26L
    T ~ Bit.L(      0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x0AE37B26F51C84D9L
    T ~ Bit.L(    1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x351C84D90AE37B26L
    T ~ Bit.L(    0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x0AE37B26F51C84D9L
    T ~ Bit.L(  1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0x751C84D90AE37B26L
    T ~ Bit.L(  0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x0AE37B26F51C84D9L
    T ~ Bit.L(1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1,0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0) ==== 0xF51C84D90AE37B26L
    T ~ Bit.L(0,0,0,0,1,0,1,0,1,1,1,0,0,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,1,1,0,1,1,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,1,1,0,0,1) ==== 0x0AE37B26F51C84D9L

    T ~ Bit.flagI(19)     ==== 0x00080000
    T ~ Bit.maskI(19)     ==== 0xFFF7FFFF
    T ~ Bit.flagI(19, 26) ==== 0x03F80000
    T ~ Bit.maskI(19, 26) ==== 0xFC07FFFF

    T ~ Bit.flagL(42)     ==== 0x0000040000000000L
    T ~ Bit.maskL(42)     ==== 0xFFFFFBFFFFFFFFFFL
    T ~ Bit.flagL(42, 53) ==== 0x001FFC0000000000L
    T ~ Bit.maskL(42, 53) ==== 0xFFE003FFFFFFFFFFL


  @Test
  def packedHexTest(): Unit =
    T ~ Hex.zero ==== 0
    T ~ Hex(0)   ==== 0
    T ~ Hex(1)   ==== 1
    T ~ Hex(2)   ==== 2
    T ~ Hex(3)   ==== 3
    T ~ Hex(4)   ==== 4
    T ~ Hex(5)   ==== 5
    T ~ Hex(6)   ==== 6
    T ~ Hex(7)   ==== 7
    T ~ Hex(8)   ==== 8
    T ~ Hex(9)   ==== 9
    T ~ Hex(10)  ==== 10
    T ~ Hex(11)  ==== 11
    T ~ Hex(12)  ==== 12
    T ~ Hex(13)  ==== 13
    T ~ Hex(14)  ==== 14
    T ~ Hex(15)  ==== 15
    T ~ Hex.flip(0)   ==== ((~0 ) & 0xF)
    T ~ Hex.flip(1)   ==== ((~1 ) & 0xF)
    T ~ Hex.flip(2)   ==== ((~2 ) & 0xF)
    T ~ Hex.flip(3)   ==== ((~3 ) & 0xF)
    T ~ Hex.flip(4)   ==== ((~4 ) & 0xF)
    T ~ Hex.flip(5)   ==== ((~5 ) & 0xF)
    T ~ Hex.flip(6)   ==== ((~6 ) & 0xF)
    T ~ Hex.flip(7)   ==== ((~7 ) & 0xF)
    T ~ Hex.flip(8)   ==== ((~8 ) & 0xF)
    T ~ Hex.flip(9)   ==== ((~9 ) & 0xF)
    T ~ Hex.flip(10)  ==== ((~10) & 0xF)
    T ~ Hex.flip(11)  ==== ((~11) & 0xF)
    T ~ Hex.flip(12)  ==== ((~12) & 0xF)
    T ~ Hex.flip(13)  ==== ((~13) & 0xF)
    T ~ Hex.flip(14)  ==== ((~14) & 0xF)
    T ~ Hex.flip(15)  ==== ((~15) & 0xF)
    T ~ Hex.literal(   0) ==== 0
    T ~ Hex.literal(   1) ==== 1
    T ~ Hex.literal(  10) ==== 2
    T ~ Hex.literal(  11) ==== 3
    T ~ Hex.literal( 100) ==== 4
    T ~ Hex.literal( 101) ==== 5
    T ~ Hex.literal( 110) ==== 6
    T ~ Hex.literal( 111) ==== 7
    T ~ Hex.literal(1000) ==== 8
    T ~ Hex.literal(1001) ==== 9
    T ~ Hex.literal(1010) ==== 10
    T ~ Hex.literal(1011) ==== 11
    T ~ Hex.literal(1100) ==== 12
    T ~ Hex.literal(1101) ==== 13
    T ~ Hex.literal(1110) ==== 14
    T ~ Hex.literal(1111) ==== 15
    T ~ Hex.from('0') ==== 0
    T ~ Hex.from('1') ==== 1
    T ~ Hex.from('2') ==== 2
    T ~ Hex.from('3') ==== 3
    T ~ Hex.from('4') ==== 4
    T ~ Hex.from('5') ==== 5
    T ~ Hex.from('6') ==== 6
    T ~ Hex.from('7') ==== 7
    T ~ Hex.from('8') ==== 8
    T ~ Hex.from('9') ==== 9
    T ~ Hex.from('A') ==== 10
    T ~ Hex.from('B') ==== 11
    T ~ Hex.from('C') ==== 12
    T ~ Hex.from('D') ==== 13
    T ~ Hex.from('E') ==== 14
    T ~ Hex.from('F') ==== 15
    T ~ Hex.from('a') ==== 10
    T ~ Hex.from('b') ==== 11
    T ~ Hex.from('c') ==== 12
    T ~ Hex.from('d') ==== 13
    T ~ Hex.from('e') ==== 14
    T ~ Hex.from('f') ==== 15

    T ~ Hex.B(              3 ) ==== 0x03.toByte
    T ~ Hex.B(             '3') ==== 0x03.toByte
    T ~ Hex.B(        13 ,  3 ) ==== 0xD3.toByte
    T ~ Hex.B(        'D', '3') ==== 0xD3.toByte
    T ~ Hex.literalB(1101,  11) ==== 0xD3.toByte

    T ~ Hex.S(                        3 ) ==== 0x0003.toShort
    T ~ Hex.S(                       '3') ==== 0x0003.toShort
    T ~ Hex.S(                  13 ,  3 ) ==== 0x00D3.toShort
    T ~ Hex.S(                  'D', '3') ==== 0x00D3.toShort
    T ~ Hex.S(             11 , 13 ,  3 ) ==== 0x0BD3.toShort
    T ~ Hex.S(             'B', 'D', '3') ==== 0x0BD3.toShort
    T ~ Hex.S(         6 , 11 , 13 ,  3 ) ==== 0x6BD3.toShort
    T ~ Hex.S(        '6', 'B', 'D', '3') ==== 0x6BD3.toShort
    T ~ Hex.literalS( 110,1011,1101,  11) ==== 0x6BD3.toShort

    T ~ Hex.C(                        3 ) ==== 0x0003.toChar
    T ~ Hex.C(                       '3') ==== 0x0003.toChar
    T ~ Hex.C(                  13 ,  3 ) ==== 0x00D3.toChar
    T ~ Hex.C(                  'D', '3') ==== 0x00D3.toChar
    T ~ Hex.C(             11 , 13 ,  3 ) ==== 0x0BD3.toChar
    T ~ Hex.C(             'B', 'D', '3') ==== 0x0BD3.toChar
    T ~ Hex.C(         6 , 11 , 13 ,  3 ) ==== 0x6BD3.toChar
    T ~ Hex.C(        '6', 'B', 'D', '3') ==== 0x6BD3.toChar
    T ~ Hex.literalC( 110,1011,1101,  11) ==== 0x6BD3.toChar

    T ~ Hex.I(                                            3 ) ==== 0x00000003
    T ~ Hex.I(                                           '3') ==== 0x00000003
    T ~ Hex.I(                                      13 ,  3 ) ==== 0x000000D3
    T ~ Hex.I(                                      'D', '3') ==== 0x000000D3
    T ~ Hex.I(                                 11 , 13 ,  3 ) ==== 0x00000BD3
    T ~ Hex.I(                                 'B', 'D', '3') ==== 0x00000BD3
    T ~ Hex.I(                             6 , 11 , 13 ,  3 ) ==== 0x00006BD3
    T ~ Hex.I(                            '6', 'B', 'D', '3') ==== 0x00006BD3
    T ~ Hex.I(                        7 ,  6 , 11 , 13 ,  3 ) ==== 0x00076BD3
    T ~ Hex.I(                       '7', '6', 'B', 'D', '3') ==== 0x00076BD3
    T ~ Hex.I(                   5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x00576BD3
    T ~ Hex.I(                  '5', '7', '6', 'B', 'D', '3') ==== 0x00576BD3
    T ~ Hex.I(             15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x0F576BD3
    T ~ Hex.I(             'F', '5', '7', '6', 'B', 'D', '3') ==== 0x0F576BD3
    T ~ Hex.I(         4 , 15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x4F576BD3
    T ~ Hex.I(        '4', 'F', '5', '7', '6', 'B', 'D', '3') ==== 0x4F576BD3
    T ~ Hex.literalI( 100,1111, 101, 111, 110,1011,1101,  11) ==== 0x4F576BD3

    T ~ Hex.L(                                                                                    3 ) ==== 0x0000000000000003L
    T ~ Hex.L(                                                                                   '3') ==== 0x0000000000000003L
    T ~ Hex.L(                                                                              13 ,  3 ) ==== 0x00000000000000D3L
    T ~ Hex.L(                                                                              'D', '3') ==== 0x00000000000000D3L
    T ~ Hex.L(                                                                         11 , 13 ,  3 ) ==== 0x0000000000000BD3L
    T ~ Hex.L(                                                                         'B', 'D', '3') ==== 0x0000000000000BD3L
    T ~ Hex.L(                                                                     6 , 11 , 13 ,  3 ) ==== 0x0000000000006BD3L
    T ~ Hex.L(                                                                    '6', 'B', 'D', '3') ==== 0x0000000000006BD3L
    T ~ Hex.L(                                                                7 ,  6 , 11 , 13 ,  3 ) ==== 0x0000000000076BD3L
    T ~ Hex.L(                                                               '7', '6', 'B', 'D', '3') ==== 0x0000000000076BD3L
    T ~ Hex.L(                                                           5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x0000000000576BD3L
    T ~ Hex.L(                                                          '5', '7', '6', 'B', 'D', '3') ==== 0x0000000000576BD3L
    T ~ Hex.L(                                                     15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x000000000F576BD3L
    T ~ Hex.L(                                                     'F', '5', '7', '6', 'B', 'D', '3') ==== 0x000000000F576BD3L
    T ~ Hex.L(                                                 4 , 15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x000000004F576BD3L
    T ~ Hex.L(                                                '4', 'F', '5', '7', '6', 'B', 'D', '3') ==== 0x000000004F576BD3L
    T ~ Hex.L(                                       8 , 12 ,  4 , 15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x0000008C4F576BD3L
    T ~ Hex.L(                                      '8', 'C', '4', 'F', '5', '7', '6', 'B', 'D', '3') ==== 0x0000008C4F576BD3L
    T ~ Hex.L(                                  2 ,  8 , 12 ,  4 , 15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x0000028C4F576BD3L
    T ~ Hex.L(                                 '2', '8', 'C', '4', 'F', '5', '7', '6', 'B', 'D', '3') ==== 0x0000028C4F576BD3L
    T ~ Hex.L(                            10 ,  2 ,  8 , 12 ,  4 , 15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x0000A28C4F576BD3L
    T ~ Hex.L(                            'A', '2', '8', 'C', '4', 'F', '5', '7', '6', 'B', 'D', '3') ==== 0x0000A28C4F576BD3L
    T ~ Hex.L(                        1 , 10 ,  2 ,  8 , 12 ,  4 , 15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x0001A28C4F576BD3L
    T ~ Hex.L(                       '1', 'A', '2', '8', 'C', '4', 'F', '5', '7', '6', 'B', 'D', '3') ==== 0x0001A28C4F576BD3L
    T ~ Hex.L(                   9 ,  1 , 10 ,  2 ,  8 , 12 ,  4 , 15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x0091A28C4F576BD3L
    T ~ Hex.L(                  '9', '1', 'A', '2', '8', 'C', '4', 'F', '5', '7', '6', 'B', 'D', '3') ==== 0x0091A28C4F576BD3L
    T ~ Hex.L(              6 ,  9 ,  1 , 10 ,  2 ,  8 , 12 ,  4 , 15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0x0691A28C4F576BD3L
    T ~ Hex.L(             '6', '9', '1', 'A', '2', '8', 'C', '4', 'F', '5', '7', '6', 'B', 'D', '3') ==== 0x0691A28C4F576BD3L
    T ~ Hex.L(        14 ,  6 ,  9 ,  1 , 10 ,  2 ,  8 , 12 ,  4 , 15 ,  5 ,  7 ,  6 , 11 , 13 ,  3 ) ==== 0xE691A28C4F576BD3L
    T ~ Hex.L(        'E', '6', '9', '1', 'A', '2', '8', 'C', '4', 'F', '5', '7', '6', 'B', 'D', '3') ==== 0xE691A28C4F576BD3L
    T ~ Hex.literalL(1110, 110,1001,   1,1010,  10,1000,1100, 100,1111, 101, 111, 110,1011,1101,  11) ==== 0xE691A28C4F576BD3L


  @Test
  def packedPrimitiveTest(): Unit =
    T ~ Pack.S(0x17: Byte, 0xC4.toByte) ==== 0xC417.toShort
    T ~ Pack.C(0x17: Byte, 0xC4.toByte) ==== 0xC417.toChar
    T ~ Pack.I(0x17: Byte, 0xC4.toByte, 0x65: Byte, 0xAB.toByte) ==== 0xAB65C417
    T ~ Pack.I(0xC417.toShort, 0xAB65.toShort)                   ==== 0xAB65C417
    T ~ Pack.I(0xC417.toChar,  0xAB65.toChar)                    ==== 0xAB65C417
    T ~ Pack.L(0x17: Byte, 0xC4.toByte, 0x65: Byte, 0xAB.toByte, 0x07: Byte, 0x3C: Byte, 0x9F.toByte, 0xDE.toByte) ==== 0xDE9F3C07AB65C417L
    T ~ Pack.L(0xC417.toShort, 0xAB65.toShort, 0x3C07: Short, 0xDE9F.toShort)                                      ==== 0xDE9F3C07AB65C417L
    T ~ Pack.L(0xC417.toChar,  0xAB65.toChar,  0x3C07.toChar, 0xDE9F.toChar)                                       ==== 0xDE9F3C07AB65C417L
    T ~ Pack.L(0xAB65C417, 0xDE9F3C07)                                                                             ==== 0xDE9F3C07AB65C417L

    var bp: Byte = 0x17
    var bq: Byte = 0xE8.toByte
    T ~ bp ==== Bit.B(0, 0, 0, 1, 0, 1, 1, 1)
    T ~ bq ==== Bit.B(1, 1, 1, 0, 1, 0, 0, 0)
    T ~ bp.bit(0) ==== 1
    T ~ bq.bit(0) ==== 0
    T ~ bp.bit(1) ==== 1
    T ~ bq.bit(1) ==== 0
    T ~ bp.bit(2) ==== 1
    T ~ bq.bit(2) ==== 0
    T ~ bp.bit(3) ==== 0
    T ~ bq.bit(3) ==== 1
    T ~ bp.bit(4) ==== 1
    T ~ bq.bit(4) ==== 0
    T ~ bp.bit(5) ==== 0
    T ~ bq.bit(5) ==== 1
    T ~ bp.bit(6) ==== 0
    T ~ bq.bit(6) ==== 1
    T ~ bp.bit(7) ==== 0
    T ~ bq.bit(7) ==== 1
    T ~ bp.bits(2, 5) ==== Bit.B(1, 0, 1)
    T ~ bq.bits(3, 8) ==== Bit.B(1, 1, 1, 0, 1)
    T ~ bp.bitTo(0)(0) ==== (bp - 1).toByte
    T ~ bp.bitTo(0)(1) ==== bp
    T ~ bq.bitTo(0)(0) ==== bq
    T ~ bq.bitTo(0)(1) ==== (bq + 1).toByte
    T ~ bp.bitTo(1)(0) ==== (bp - 2).toByte
    T ~ bp.bitTo(1)(1) ==== bp
    T ~ bq.bitTo(1)(0) ==== bq
    T ~ bq.bitTo(1)(1) ==== (bq + 2).toByte
    T ~ bp.bitTo(2)(0) ==== (bp - 4).toByte
    T ~ bp.bitTo(2)(1) ==== bp
    T ~ bq.bitTo(2)(0) ==== bq
    T ~ bq.bitTo(2)(1) ==== (bq + 4).toByte
    T ~ bp.bitTo(3)(0) ==== bp
    T ~ bp.bitTo(3)(1) ==== (bp + 8).toByte
    T ~ bq.bitTo(3)(0) ==== (bq - 8).toByte
    T ~ bq.bitTo(3)(1) ==== bq
    T ~ bp.bitTo(4)(0) ==== (bp - 16).toByte
    T ~ bp.bitTo(4)(1) ==== bp
    T ~ bq.bitTo(4)(0) ==== bq
    T ~ bq.bitTo(4)(1) ==== (bq + 16).toByte
    T ~ bp.bitTo(5)(0) ==== bp
    T ~ bp.bitTo(5)(1) ==== (bp + 32).toByte
    T ~ bq.bitTo(5)(0) ==== (bq - 32).toByte
    T ~ bq.bitTo(5)(1) ==== bq
    T ~ bp.bitTo(6)(0) ==== bp
    T ~ bp.bitTo(6)(1) ==== (bp + 64).toByte
    T ~ bq.bitTo(6)(0) ==== (bq - 64).toByte
    T ~ bq.bitTo(6)(1) ==== bq
    T ~ bp.bitTo(7)(0) ==== bp
    T ~ bp.bitTo(7)(1) ==== (bp + 128).toByte
    T ~ bq.bitTo(7)(0) ==== (bq - 128).toByte
    T ~ bq.bitTo(7)(1) ==== bq
    T ~ bq.bitsTo(0, 2)(0xFF.toByte)          ==== 0xEB.toByte
    T ~ bp.bitsTo(2, 5)(Bit.B(0, 1, 0))       ==== 0x0B.toByte
    T ~ bq.bitsTo(3, 8)(Bit.B(0, 1, 0, 1, 0)) ==== 0x50.toByte
    T ~ bp.bitsTo(0, 8)(0x7E.toByte)          ==== 0x7E.toByte
    T ~ bp.reverseBits ==== Bit.B(1, 1, 1, 0, 1, 0, 0, 0)
    T ~ bq.bitString ==== "11101000"
    T ~ bp.hex(0) ==== Hex.from('7')
    T ~ bp.hex(1) ==== Hex.from('1')
    T ~ bq.hex(0) ==== Hex.from('8')
    T ~ bq.hex(1) ==== Hex.from('E')
    T ~ bp.hexTo(0)(0xC) ==== 0x1C.toByte
    T ~ bp.hexTo(1)(0x6) ==== 0x67.toByte
    T ~ bp.reverseHex ==== 0x71.toByte
    T ~ bq.hiHexString ==== "E8"
    T ~ bq.loHexString ==== "e8"
    T ~ bq.hexString   ==== "E8"

    var sp = 0x4917.toShort
    var sq = 0xB6E8.toShort
    T ~ sp ==== Bit.S(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1)
    T ~ sq ==== Bit.S(1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0)
    T ~ sp.bit( 0) ==== 1
    T ~ sq.bit( 0) ==== 0
    T ~ sp.bit( 1) ==== 1
    T ~ sq.bit( 1) ==== 0
    T ~ sp.bit( 2) ==== 1
    T ~ sq.bit( 2) ==== 0
    T ~ sp.bit( 3) ==== 0
    T ~ sq.bit( 3) ==== 1
    T ~ sp.bit( 4) ==== 1
    T ~ sq.bit( 4) ==== 0
    T ~ sp.bit( 5) ==== 0
    T ~ sq.bit( 5) ==== 1
    T ~ sp.bit( 6) ==== 0
    T ~ sq.bit( 6) ==== 1
    T ~ sp.bit( 7) ==== 0
    T ~ sq.bit( 7) ==== 1
    T ~ sp.bit( 8) ==== 1
    T ~ sq.bit( 8) ==== 0
    T ~ sp.bit( 9) ==== 0
    T ~ sq.bit( 9) ==== 1
    T ~ sp.bit(10) ==== 0
    T ~ sq.bit(10) ==== 1
    T ~ sp.bit(11) ==== 1
    T ~ sq.bit(11) ==== 0
    T ~ sp.bit(12) ==== 0
    T ~ sq.bit(12) ==== 1
    T ~ sp.bit(13) ==== 0
    T ~ sq.bit(13) ==== 1
    T ~ sp.bit(14) ==== 1
    T ~ sq.bit(14) ==== 0
    T ~ sp.bit(15) ==== 0
    T ~ sq.bit(15) ==== 1
    T ~ sp.bits(2, 5)   ==== Bit.S(1, 0, 1)
    T ~ sq.bits(3, 8)   ==== Bit.S(1, 1, 1, 0, 1)
    T ~ sp.bits(5, 15)  ==== Bit.S(1, 0, 0, 1, 0, 0, 1, 0, 0, 0)
    T ~ sq.bits(13, 16) ==== Bit.S(1, 0, 1)
    T ~ sp.bitTo( 0)(0) ==== (sp - 1).toShort
    T ~ sp.bitTo( 0)(1) ==== sp
    T ~ sq.bitTo( 0)(0) ==== sq
    T ~ sq.bitTo( 0)(1) ==== (sq + 1).toShort
    T ~ sp.bitTo( 1)(0) ==== (sp - 2).toShort
    T ~ sp.bitTo( 1)(1) ==== sp
    T ~ sq.bitTo( 1)(0) ==== sq
    T ~ sq.bitTo( 1)(1) ==== (sq + 2).toShort
    T ~ sp.bitTo( 2)(0) ==== (sp - 4).toShort
    T ~ sp.bitTo( 2)(1) ==== sp
    T ~ sq.bitTo( 2)(0) ==== sq
    T ~ sq.bitTo( 2)(1) ==== (sq + 4).toShort
    T ~ sp.bitTo( 3)(0) ==== sp
    T ~ sp.bitTo( 3)(1) ==== (sp + 8).toShort
    T ~ sq.bitTo( 3)(0) ==== (sq - 8).toShort
    T ~ sq.bitTo( 3)(1) ==== sq
    T ~ sp.bitTo( 4)(0) ==== (sp - 16).toShort
    T ~ sp.bitTo( 4)(1) ==== sp
    T ~ sq.bitTo( 4)(0) ==== sq
    T ~ sq.bitTo( 4)(1) ==== (sq + 16).toShort
    T ~ sp.bitTo( 5)(0) ==== sp
    T ~ sp.bitTo( 5)(1) ==== (sp + 32).toShort
    T ~ sq.bitTo( 5)(0) ==== (sq - 32).toShort
    T ~ sq.bitTo( 5)(1) ==== sq
    T ~ sp.bitTo( 6)(0) ==== sp
    T ~ sp.bitTo( 6)(1) ==== (sp + 64).toShort
    T ~ sq.bitTo( 6)(0) ==== (sq - 64).toShort
    T ~ sq.bitTo( 6)(1) ==== sq
    T ~ sp.bitTo( 7)(0) ==== sp
    T ~ sp.bitTo( 7)(1) ==== (sp + 128).toShort
    T ~ sq.bitTo( 7)(0) ==== (sq - 128).toShort
    T ~ sq.bitTo( 7)(1) ==== sq
    T ~ sp.bitTo( 8)(0) ==== (sp - 0x100).toShort
    T ~ sp.bitTo( 8)(1) ==== sp
    T ~ sq.bitTo( 8)(0) ==== sq
    T ~ sq.bitTo( 8)(1) ==== (sq + 0x100).toShort
    T ~ sp.bitTo( 9)(0) ==== sp
    T ~ sp.bitTo( 9)(1) ==== (sp + 0x200).toShort
    T ~ sq.bitTo( 9)(0) ==== (sq - 0x200).toShort
    T ~ sq.bitTo( 9)(1) ==== sq
    T ~ sp.bitTo(10)(0) ==== sp
    T ~ sp.bitTo(10)(1) ==== (sp + 0x400).toShort
    T ~ sq.bitTo(10)(0) ==== (sq - 0x400).toShort
    T ~ sq.bitTo(10)(1) ==== sq
    T ~ sp.bitTo(11)(0) ==== (sp - 0x800).toShort
    T ~ sp.bitTo(11)(1) ==== sp
    T ~ sq.bitTo(11)(0) ==== sq
    T ~ sq.bitTo(11)(1) ==== (sq + 0x800).toShort
    T ~ sp.bitTo(12)(0) ==== sp
    T ~ sp.bitTo(12)(1) ==== (sp + 0x1000).toShort
    T ~ sq.bitTo(12)(0) ==== (sq - 0x1000).toShort
    T ~ sq.bitTo(12)(1) ==== sq
    T ~ sp.bitTo(13)(0) ==== sp
    T ~ sp.bitTo(13)(1) ==== (sp + 0x2000).toShort
    T ~ sq.bitTo(13)(0) ==== (sq - 0x2000).toShort
    T ~ sq.bitTo(13)(1) ==== sq
    T ~ sp.bitTo(14)(0) ==== (sp - 0x4000).toShort
    T ~ sp.bitTo(14)(1) ==== sp
    T ~ sq.bitTo(14)(0) ==== sq
    T ~ sq.bitTo(14)(1) ==== (sq + 0x4000).toShort
    T ~ sp.bitTo(15)(0) ==== sp
    T ~ sp.bitTo(15)(1) ==== (sp + 0x8000).toShort
    T ~ sq.bitTo(15)(0) ==== (sq - 0x8000).toShort
    T ~ sq.bitTo(15)(1) ==== sq
    T ~ sq.bitsTo( 0,  2)(0xFFFF.toShort)                      ==== 0xB6EB.toShort
    T ~ sp.bitsTo( 2,  5)(Bit.S(0, 1, 0))                      ==== 0x490B.toShort
    T ~ sq.bitsTo( 3,  8)(Bit.S(0, 1, 0, 1, 0))                ==== 0xB650.toShort
    T ~ sq.bitsTo( 5, 15)(Bit.S(1, 1, 1, 0, 0, 1, 0, 0, 0, 0)) ==== Bit.S(1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0)
    T ~ sp.bitsTo(13, 16)(Bit.S(1, 1, 0))                      ==== 0xC917.toShort
    T ~ sp.bitsTo( 0, 16)(0xBA2D.toShort)                      ==== 0xBA2D.toShort
    T ~ sp.reverseBits ==== Bit.S(1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0)
    T ~ sq.bitString ==== "1011011011101000"
    T ~ sp.hex(0) ==== Hex.from('7')
    T ~ sp.hex(1) ==== Hex.from('1')
    T ~ sp.hex(2) ==== Hex.from('9')
    T ~ sp.hex(3) ==== Hex.from('4')
    T ~ sq.hex(0) ==== Hex.from('8')
    T ~ sq.hex(1) ==== Hex.from('E')
    T ~ sq.hex(2) ==== Hex.from('6')
    T ~ sq.hex(3) ==== Hex.from('B')
    T ~ sp.hexTo(0)(0xC) ==== 0x491C.toShort
    T ~ sp.hexTo(1)(0x6) ==== 0x4967.toShort
    T ~ sp.hexTo(2)(0x5) ==== 0x4517.toShort
    T ~ sp.hexTo(3)(0xA) ==== 0xA917.toShort
    T ~ sq.reverseHex ==== 0x8E6B.toShort
    T ~ sq.hiHexString ==== "B6E8"
    T ~ sq.loHexString ==== "b6e8"
    T ~ sq.hexString   ==== "B6E8"
    T ~ sp.byte(0) ==== 0x17.toByte
    T ~ sp.byte(1) ==== 0x49.toByte
    T ~ sq.byte(0) ==== 0xE8.toByte
    T ~ sq.byte(1) ==== 0xB6.toByte
    T ~ sp.byteTo(0)(0x3C: Byte)  ==== 0x493C.toShort
    T ~ sp.byteTo(1)(0xAA.toByte) ==== 0xAA17.toShort
    T ~ sp.reverseBytes ==== 0x1749.toShort

    var cp = 0x4917.toChar
    var cq = 0xB6E8.toChar
    T ~ cp ==== Bit.C(0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1)
    T ~ cq ==== Bit.C(1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0)
    T ~ cp.bit( 0) ==== 1
    T ~ cq.bit( 0) ==== 0
    T ~ cp.bit( 1) ==== 1
    T ~ cq.bit( 1) ==== 0
    T ~ cp.bit( 2) ==== 1
    T ~ cq.bit( 2) ==== 0
    T ~ cp.bit( 3) ==== 0
    T ~ cq.bit( 3) ==== 1
    T ~ cp.bit( 4) ==== 1
    T ~ cq.bit( 4) ==== 0
    T ~ cp.bit( 5) ==== 0
    T ~ cq.bit( 5) ==== 1
    T ~ cp.bit( 6) ==== 0
    T ~ cq.bit( 6) ==== 1
    T ~ cp.bit( 7) ==== 0
    T ~ cq.bit( 7) ==== 1
    T ~ cp.bit( 8) ==== 1
    T ~ cq.bit( 8) ==== 0
    T ~ cp.bit( 9) ==== 0
    T ~ cq.bit( 9) ==== 1
    T ~ cp.bit(10) ==== 0
    T ~ cq.bit(10) ==== 1
    T ~ cp.bit(11) ==== 1
    T ~ cq.bit(11) ==== 0
    T ~ cp.bit(12) ==== 0
    T ~ cq.bit(12) ==== 1
    T ~ cp.bit(13) ==== 0
    T ~ cq.bit(13) ==== 1
    T ~ cp.bit(14) ==== 1
    T ~ cq.bit(14) ==== 0
    T ~ cp.bit(15) ==== 0
    T ~ cq.bit(15) ==== 1
    T ~ cp.bits(2, 5)   ==== Bit.C(1, 0, 1)
    T ~ cq.bits(3, 8)   ==== Bit.C(1, 1, 1, 0, 1)
    T ~ cp.bits(5, 15)  ==== Bit.C(1, 0, 0, 1, 0, 0, 1, 0, 0, 0)
    T ~ cq.bits(13, 16) ==== Bit.C(1, 0, 1)
    T ~ cp.bitTo( 0)(0) ==== (cp - 1).toChar
    T ~ cp.bitTo( 0)(1) ==== cp
    T ~ cq.bitTo( 0)(0) ==== cq
    T ~ cq.bitTo( 0)(1) ==== (cq + 1).toChar
    T ~ cp.bitTo( 1)(0) ==== (cp - 2).toChar
    T ~ cp.bitTo( 1)(1) ==== cp
    T ~ cq.bitTo( 1)(0) ==== cq
    T ~ cq.bitTo( 1)(1) ==== (cq + 2).toChar
    T ~ cp.bitTo( 2)(0) ==== (cp - 4).toChar
    T ~ cp.bitTo( 2)(1) ==== cp
    T ~ cq.bitTo( 2)(0) ==== cq
    T ~ cq.bitTo( 2)(1) ==== (cq + 4).toChar
    T ~ cp.bitTo( 3)(0) ==== cp
    T ~ cp.bitTo( 3)(1) ==== (cp + 8).toChar
    T ~ cq.bitTo( 3)(0) ==== (cq - 8).toChar
    T ~ cq.bitTo( 3)(1) ==== cq
    T ~ cp.bitTo( 4)(0) ==== (cp - 16).toChar
    T ~ cp.bitTo( 4)(1) ==== cp
    T ~ cq.bitTo( 4)(0) ==== cq
    T ~ cq.bitTo( 4)(1) ==== (cq + 16).toChar
    T ~ cp.bitTo( 5)(0) ==== cp
    T ~ cp.bitTo( 5)(1) ==== (cp + 32).toChar
    T ~ cq.bitTo( 5)(0) ==== (cq - 32).toChar
    T ~ cq.bitTo( 5)(1) ==== cq
    T ~ cp.bitTo( 6)(0) ==== cp
    T ~ cp.bitTo( 6)(1) ==== (cp + 64).toChar
    T ~ cq.bitTo( 6)(0) ==== (cq - 64).toChar
    T ~ cq.bitTo( 6)(1) ==== cq
    T ~ cp.bitTo( 7)(0) ==== cp
    T ~ cp.bitTo( 7)(1) ==== (cp + 128).toChar
    T ~ cq.bitTo( 7)(0) ==== (cq - 128).toChar
    T ~ cq.bitTo( 7)(1) ==== cq
    T ~ cp.bitTo( 8)(0) ==== (cp - 0x100).toChar
    T ~ cp.bitTo( 8)(1) ==== cp
    T ~ cq.bitTo( 8)(0) ==== cq
    T ~ cq.bitTo( 8)(1) ==== (cq + 0x100).toChar
    T ~ cp.bitTo( 9)(0) ==== cp
    T ~ cp.bitTo( 9)(1) ==== (cp + 0x200).toChar
    T ~ cq.bitTo( 9)(0) ==== (cq - 0x200).toChar
    T ~ cq.bitTo( 9)(1) ==== cq
    T ~ cp.bitTo(10)(0) ==== cp
    T ~ cp.bitTo(10)(1) ==== (cp + 0x400).toChar
    T ~ cq.bitTo(10)(0) ==== (cq - 0x400).toChar
    T ~ cq.bitTo(10)(1) ==== cq
    T ~ cp.bitTo(11)(0) ==== (cp - 0x800).toChar
    T ~ cp.bitTo(11)(1) ==== cp
    T ~ cq.bitTo(11)(0) ==== cq
    T ~ cq.bitTo(11)(1) ==== (cq + 0x800).toChar
    T ~ cp.bitTo(12)(0) ==== cp
    T ~ cp.bitTo(12)(1) ==== (cp + 0x1000).toChar
    T ~ cq.bitTo(12)(0) ==== (cq - 0x1000).toChar
    T ~ cq.bitTo(12)(1) ==== cq
    T ~ cp.bitTo(13)(0) ==== cp
    T ~ cp.bitTo(13)(1) ==== (cp + 0x2000).toChar
    T ~ cq.bitTo(13)(0) ==== (cq - 0x2000).toChar
    T ~ cq.bitTo(13)(1) ==== cq
    T ~ cp.bitTo(14)(0) ==== (cp - 0x4000).toChar
    T ~ cp.bitTo(14)(1) ==== cp
    T ~ cq.bitTo(14)(0) ==== cq
    T ~ cq.bitTo(14)(1) ==== (cq + 0x4000).toChar
    T ~ cp.bitTo(15)(0) ==== cp
    T ~ cp.bitTo(15)(1) ==== (cp + 0x8000).toChar
    T ~ cq.bitTo(15)(0) ==== (cq - 0x8000).toChar
    T ~ cq.bitTo(15)(1) ==== cq
    T ~ cq.bitsTo( 0,  2)(0xFFFF.toChar)                      ==== 0xB6EB.toChar
    T ~ cp.bitsTo(2, 5)(Bit.C(0, 1, 0))                       ==== 0x490B.toChar
    T ~ cq.bitsTo(3, 8)(Bit.C(0, 1, 0, 1, 0))                 ==== 0xB650.toChar
    T ~ cq.bitsTo(5, 15)(Bit.C(1, 1, 1, 0, 0, 1, 0, 0, 0, 0)) ==== Bit.C(1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0)
    T ~ cp.bitsTo(13, 16)(Bit.C(1, 1, 0))                     ==== 0xC917.toChar
    T ~ cp.bitsTo( 0, 16)(0xBA2D.toChar)                      ==== 0xBA2D.toChar
    T ~ cp.reverseBits ==== Bit.C(1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0)
    T ~ cq.bitString ==== "1011011011101000"
    T ~ cp.hex(0) ==== Hex.from('7')
    T ~ cp.hex(1) ==== Hex.from('1')
    T ~ cp.hex(2) ==== Hex.from('9')
    T ~ cp.hex(3) ==== Hex.from('4')
    T ~ cq.hex(0) ==== Hex.from('8')
    T ~ cq.hex(1) ==== Hex.from('E')
    T ~ cq.hex(2) ==== Hex.from('6')
    T ~ cq.hex(3) ==== Hex.from('B')
    T ~ cp.hexTo(0)(0xC) ==== 0x491C.toChar
    T ~ cp.hexTo(1)(0x6) ==== 0x4967.toChar
    T ~ cp.hexTo(2)(0x5) ==== 0x4517.toChar
    T ~ cp.hexTo(3)(0xA) ==== 0xA917.toChar
    T ~ cq.reverseHex ==== 0x8E6B.toChar
    T ~ cq.hiHexString ==== "B6E8"
    T ~ cq.loHexString ==== "b6e8"
    T ~ cq.hexString   ==== "B6E8"
    T ~ cp.byte(0) ==== 0x17.toByte
    T ~ cp.byte(1) ==== 0x49.toByte
    T ~ cq.byte(0) ==== 0xE8.toByte
    T ~ cq.byte(1) ==== 0xB6.toByte
    T ~ cp.byteTo(0)(0x3C: Byte)  ==== 0x493C.toChar
    T ~ cp.byteTo(1)(0xAA.toByte) ==== 0xAA17.toChar
    T ~ cp.reverseBytes ==== 0x1749.toChar

    var ip = 0x2CF54917
    var iq = 0xD30AB6E8
    T ~ ip ==== Bit.I(0,0,1,0,1,1,0,0, 1,1,1,1,0,1,0,1, 0,1,0,0,1,0,0,1, 0,0,0,1,0,1,1,1)
    T ~ iq ==== Bit.I(1,1,0,1,0,0,1,1, 0,0,0,0,1,0,1,0, 1,0,1,1,0,1,1,0, 1,1,1,0,1,0,0,0)
    T ~ ip.bit( 0) ==== 1
    T ~ iq.bit( 0) ==== 0
    T ~ ip.bit( 1) ==== 1
    T ~ iq.bit( 1) ==== 0
    T ~ ip.bit( 2) ==== 1
    T ~ iq.bit( 2) ==== 0
    T ~ ip.bit( 3) ==== 0
    T ~ iq.bit( 3) ==== 1
    T ~ ip.bit( 4) ==== 1
    T ~ iq.bit( 4) ==== 0
    T ~ ip.bit( 5) ==== 0
    T ~ iq.bit( 5) ==== 1
    T ~ ip.bit( 6) ==== 0
    T ~ iq.bit( 6) ==== 1
    T ~ ip.bit( 7) ==== 0
    T ~ iq.bit( 7) ==== 1
    T ~ ip.bit( 8) ==== 1
    T ~ iq.bit( 8) ==== 0
    T ~ ip.bit( 9) ==== 0
    T ~ iq.bit( 9) ==== 1
    T ~ ip.bit(10) ==== 0
    T ~ iq.bit(10) ==== 1
    T ~ ip.bit(11) ==== 1
    T ~ iq.bit(11) ==== 0
    T ~ ip.bit(12) ==== 0
    T ~ iq.bit(12) ==== 1
    T ~ ip.bit(13) ==== 0
    T ~ iq.bit(13) ==== 1
    T ~ ip.bit(14) ==== 1
    T ~ iq.bit(14) ==== 0
    T ~ ip.bit(15) ==== 0
    T ~ iq.bit(15) ==== 1
    T ~ ip.bit(16) ==== 1
    T ~ iq.bit(16) ==== 0
    T ~ ip.bit(17) ==== 0
    T ~ iq.bit(17) ==== 1
    T ~ ip.bit(18) ==== 1
    T ~ iq.bit(18) ==== 0
    T ~ ip.bit(19) ==== 0
    T ~ iq.bit(19) ==== 1
    T ~ ip.bit(20) ==== 1
    T ~ iq.bit(20) ==== 0
    T ~ ip.bit(21) ==== 1
    T ~ iq.bit(21) ==== 0
    T ~ ip.bit(22) ==== 1
    T ~ iq.bit(22) ==== 0
    T ~ ip.bit(23) ==== 1
    T ~ iq.bit(23) ==== 0
    T ~ ip.bit(24) ==== 0
    T ~ iq.bit(24) ==== 1
    T ~ ip.bit(25) ==== 0
    T ~ iq.bit(25) ==== 1
    T ~ ip.bit(26) ==== 1
    T ~ iq.bit(26) ==== 0
    T ~ ip.bit(27) ==== 1
    T ~ iq.bit(27) ==== 0
    T ~ ip.bit(28) ==== 0
    T ~ iq.bit(28) ==== 1
    T ~ ip.bit(29) ==== 1
    T ~ iq.bit(29) ==== 0
    T ~ ip.bit(30) ==== 0
    T ~ iq.bit(30) ==== 1
    T ~ ip.bit(31) ==== 0
    T ~ iq.bit(31) ==== 1
    T ~ ip.bits( 2,  5) ==== Bit.I(1, 0, 1)
    T ~ iq.bits( 3,  8) ==== Bit.I(1, 1, 1, 0, 1)
    T ~ ip.bits( 5, 15) ==== Bit.I(1, 0, 0, 1, 0, 0, 1, 0, 0, 0)
    T ~ iq.bits(13, 16) ==== Bit.I(1, 0, 1)
    T ~ ip.bits( 5, 29) ==== Bit.I(0,1,1,0,0,1,1,1,1,0,1,0,1,0,1,0,0,1,0,0,1,0,0,0)
    T ~ iq.bits(28, 32) ==== Bit.I(1,1,0,1)
    T ~ ip.bitTo( 0)(0) ==== (ip - 1)
    T ~ ip.bitTo( 0)(1) ==== ip
    T ~ iq.bitTo( 0)(0) ==== iq
    T ~ iq.bitTo( 0)(1) ==== (iq + 1)
    T ~ ip.bitTo( 1)(0) ==== (ip - 2)
    T ~ ip.bitTo( 1)(1) ==== ip
    T ~ iq.bitTo( 1)(0) ==== iq
    T ~ iq.bitTo( 1)(1) ==== (iq + 2)
    T ~ ip.bitTo( 2)(0) ==== (ip - 4)
    T ~ ip.bitTo( 2)(1) ==== ip
    T ~ iq.bitTo( 2)(0) ==== iq
    T ~ iq.bitTo( 2)(1) ==== (iq + 4)
    T ~ ip.bitTo( 3)(0) ==== ip
    T ~ ip.bitTo( 3)(1) ==== (ip + 8)
    T ~ iq.bitTo( 3)(0) ==== (iq - 8)
    T ~ iq.bitTo( 3)(1) ==== iq
    T ~ ip.bitTo( 4)(0) ==== (ip - 16)
    T ~ ip.bitTo( 4)(1) ==== ip
    T ~ iq.bitTo( 4)(0) ==== iq
    T ~ iq.bitTo( 4)(1) ==== (iq + 16)
    T ~ ip.bitTo( 5)(0) ==== ip
    T ~ ip.bitTo( 5)(1) ==== (ip + 32)
    T ~ iq.bitTo( 5)(0) ==== (iq - 32)
    T ~ iq.bitTo( 5)(1) ==== iq
    T ~ ip.bitTo( 6)(0) ==== ip
    T ~ ip.bitTo( 6)(1) ==== (ip + 64)
    T ~ iq.bitTo( 6)(0) ==== (iq - 64)
    T ~ iq.bitTo( 6)(1) ==== iq
    T ~ ip.bitTo( 7)(0) ==== ip
    T ~ ip.bitTo( 7)(1) ==== (ip + 128)
    T ~ iq.bitTo( 7)(0) ==== (iq - 128)
    T ~ iq.bitTo( 7)(1) ==== iq
    T ~ ip.bitTo( 8)(0) ==== (ip - 0x100)
    T ~ ip.bitTo( 8)(1) ==== ip
    T ~ iq.bitTo( 8)(0) ==== iq
    T ~ iq.bitTo( 8)(1) ==== (iq + 0x100)
    T ~ ip.bitTo( 9)(0) ==== ip
    T ~ ip.bitTo( 9)(1) ==== (ip + 0x200)
    T ~ iq.bitTo( 9)(0) ==== (iq - 0x200)
    T ~ iq.bitTo( 9)(1) ==== iq
    T ~ ip.bitTo(10)(0) ==== ip
    T ~ ip.bitTo(10)(1) ==== (ip + 0x400)
    T ~ iq.bitTo(10)(0) ==== (iq - 0x400)
    T ~ iq.bitTo(10)(1) ==== iq
    T ~ ip.bitTo(11)(0) ==== (ip - 0x800)
    T ~ ip.bitTo(11)(1) ==== ip
    T ~ iq.bitTo(11)(0) ==== iq
    T ~ iq.bitTo(11)(1) ==== (iq + 0x800)
    T ~ ip.bitTo(12)(0) ==== ip
    T ~ ip.bitTo(12)(1) ==== (ip + 0x1000)
    T ~ iq.bitTo(12)(0) ==== (iq - 0x1000)
    T ~ iq.bitTo(12)(1) ==== iq
    T ~ ip.bitTo(13)(0) ==== ip
    T ~ ip.bitTo(13)(1) ==== (ip + 0x2000)
    T ~ iq.bitTo(13)(0) ==== (iq - 0x2000)
    T ~ iq.bitTo(13)(1) ==== iq
    T ~ ip.bitTo(14)(0) ==== (ip - 0x4000)
    T ~ ip.bitTo(14)(1) ==== ip
    T ~ iq.bitTo(14)(0) ==== iq
    T ~ iq.bitTo(14)(1) ==== (iq + 0x4000)
    T ~ ip.bitTo(15)(0) ==== ip
    T ~ ip.bitTo(15)(1) ==== (ip + 0x8000)
    T ~ iq.bitTo(15)(0) ==== (iq - 0x8000)
    T ~ iq.bitTo(15)(1) ==== iq
    T ~ ip.bitTo(16)(0) ==== (ip - 0x10000)
    T ~ ip.bitTo(16)(1) ==== ip
    T ~ iq.bitTo(16)(0) ==== iq
    T ~ iq.bitTo(16)(1) ==== (iq + 0x10000)
    T ~ ip.bitTo(17)(0) ==== ip
    T ~ ip.bitTo(17)(1) ==== (ip + 0x20000)
    T ~ iq.bitTo(17)(0) ==== (iq - 0x20000)
    T ~ iq.bitTo(17)(1) ==== iq
    T ~ ip.bitTo(18)(0) ==== (ip - 0x40000)
    T ~ ip.bitTo(18)(1) ==== ip
    T ~ iq.bitTo(18)(0) ==== iq
    T ~ iq.bitTo(18)(1) ==== (iq + 0x40000)
    T ~ ip.bitTo(19)(0) ==== ip
    T ~ ip.bitTo(19)(1) ==== (ip + 0x80000)
    T ~ iq.bitTo(19)(0) ==== (iq - 0x80000)
    T ~ iq.bitTo(19)(1) ==== iq
    T ~ ip.bitTo(20)(0) ==== (ip - 0x100000)
    T ~ ip.bitTo(20)(1) ==== ip
    T ~ iq.bitTo(20)(0) ==== iq
    T ~ iq.bitTo(20)(1) ==== (iq + 0x100000)
    T ~ ip.bitTo(21)(0) ==== (ip - 0x200000)
    T ~ ip.bitTo(21)(1) ==== ip
    T ~ iq.bitTo(21)(0) ==== iq
    T ~ iq.bitTo(21)(1) ==== (iq + 0x200000)
    T ~ ip.bitTo(22)(0) ==== (ip - 0x400000)
    T ~ ip.bitTo(22)(1) ==== ip
    T ~ iq.bitTo(22)(0) ==== iq
    T ~ iq.bitTo(22)(1) ==== (iq + 0x400000)
    T ~ ip.bitTo(23)(0) ==== (ip - 0x800000)
    T ~ ip.bitTo(23)(1) ==== ip
    T ~ iq.bitTo(23)(0) ==== iq
    T ~ iq.bitTo(23)(1) ==== (iq + 0x800000)
    T ~ ip.bitTo(24)(0) ==== ip
    T ~ ip.bitTo(24)(1) ==== (ip + 0x1000000)
    T ~ iq.bitTo(24)(0) ==== (iq - 0x1000000)
    T ~ iq.bitTo(24)(1) ==== iq
    T ~ ip.bitTo(25)(0) ==== ip
    T ~ ip.bitTo(25)(1) ==== (ip + 0x2000000)
    T ~ iq.bitTo(25)(0) ==== (iq - 0x2000000)
    T ~ iq.bitTo(25)(1) ==== iq
    T ~ ip.bitTo(26)(0) ==== (ip - 0x4000000)
    T ~ ip.bitTo(26)(1) ==== ip
    T ~ iq.bitTo(26)(0) ==== iq
    T ~ iq.bitTo(26)(1) ==== (iq + 0x4000000)
    T ~ ip.bitTo(27)(0) ==== (ip - 0x8000000)
    T ~ ip.bitTo(27)(1) ==== ip
    T ~ iq.bitTo(27)(0) ==== iq
    T ~ iq.bitTo(27)(1) ==== (iq + 0x8000000)
    T ~ ip.bitTo(28)(0) ==== ip
    T ~ ip.bitTo(28)(1) ==== (ip + 0x10000000)
    T ~ iq.bitTo(28)(0) ==== (iq - 0x10000000)
    T ~ iq.bitTo(28)(1) ==== iq
    T ~ ip.bitTo(29)(0) ==== (ip - 0x20000000)
    T ~ ip.bitTo(29)(1) ==== ip
    T ~ iq.bitTo(29)(0) ==== iq
    T ~ iq.bitTo(29)(1) ==== (iq + 0x20000000)
    T ~ ip.bitTo(30)(0) ==== ip
    T ~ ip.bitTo(30)(1) ==== (ip + 0x40000000)
    T ~ iq.bitTo(30)(0) ==== (iq - 0x40000000)
    T ~ iq.bitTo(30)(1) ==== iq
    T ~ ip.bitTo(31)(0) ==== ip
    T ~ ip.bitTo(31)(1) ==== (ip + 0x80000000)
    T ~ iq.bitTo(31)(0) ==== (iq - 0x80000000)
    T ~ iq.bitTo(31)(1) ==== iq
    T ~ iq.bitsTo( 0,  2)(0xFFFFFFFF)                          ==== 0xD30AB6EB
    T ~ ip.bitsTo( 2,  5)(Bit.I(0, 1, 0))                      ==== 0x2CF5490B
    T ~ iq.bitsTo( 3,  8)(Bit.I(0, 1, 0, 1, 0))                ==== 0xD30AB650
    T ~ iq.bitsTo( 5, 15)(Bit.I(1, 1, 1, 0, 0, 1, 0, 0, 0, 0)) ==== Pack.I(Bit.S(1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,0), 0xD30A.toShort)
    T ~ ip.bitsTo(13, 16)(Bit.I(1, 1, 0))                      ==== 0x2CF5C917
    T ~ ip.bitsTo( 5, 29)(0x595959)                            ==== Bit.I(0,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,1,1,0,1,1,1)
    T ~ iq.bitsTo(28, 32)(0xA)                                 ==== 0xA30AB6E8
    T ~ ip.bitsTo( 0, 32)(0x42317FB9)                          ==== 0x42317FB9
    T ~ ip.reverseBits ==== Bit.I(1,1,1,0,1,0,0,0, 1,0,0,1,0,0,1,0, 1,0,1,0,1,1,1,1, 0,0,1,1,0,1,0,0)
    T ~ iq.bitString ==== "11010011000010101011011011101000"
    T ~ ip.hex(0) ==== Hex.from('7')
    T ~ ip.hex(1) ==== Hex.from('1')
    T ~ ip.hex(2) ==== Hex.from('9')
    T ~ ip.hex(3) ==== Hex.from('4')
    T ~ ip.hex(4) ==== Hex.from('5')
    T ~ ip.hex(5) ==== Hex.from('F')
    T ~ ip.hex(6) ==== Hex.from('C')
    T ~ ip.hex(7) ==== Hex.from('2')
    T ~ iq.hex(0) ==== Hex.from('8')
    T ~ iq.hex(1) ==== Hex.from('E')
    T ~ iq.hex(2) ==== Hex.from('6')
    T ~ iq.hex(3) ==== Hex.from('B')
    T ~ iq.hex(4) ==== Hex.from('A')
    T ~ iq.hex(5) ==== Hex.from('0')
    T ~ iq.hex(6) ==== Hex.from('3')
    T ~ iq.hex(7) ==== Hex.from('D')
    T ~ ip.hexTo(0)(0xC) ==== 0x2CF5491C
    T ~ ip.hexTo(1)(0x6) ==== 0x2CF54967
    T ~ ip.hexTo(2)(0x5) ==== 0x2CF54517
    T ~ ip.hexTo(3)(0xA) ==== 0x2CF5A917
    T ~ ip.hexTo(4)(0xB) ==== 0x2CFB4917
    T ~ ip.hexTo(5)(0x4) ==== 0x2C454917
    T ~ ip.hexTo(6)(0x7) ==== 0x27F54917
    T ~ ip.hexTo(7)(0xE) ==== 0xECF54917
    T ~ iq.reverseHex ==== 0x8E6BA03D
    T ~ iq.hiHexString ==== "D30AB6E8"
    T ~ iq.loHexString ==== "d30ab6e8"
    T ~ iq.hexString   ==== "D30AB6E8"
    T ~ ip.byte(0) ==== 0x17.toByte
    T ~ ip.byte(1) ==== 0x49.toByte
    T ~ ip.byte(2) ==== 0xF5.toByte
    T ~ ip.byte(3) ==== 0x2C.toByte
    T ~ iq.byte(0) ==== 0xE8.toByte
    T ~ iq.byte(1) ==== 0xB6.toByte
    T ~ iq.byte(2) ==== 0x0A.toByte
    T ~ iq.byte(3) ==== 0xD3.toByte
    T ~ ip.byteTo(0)(0x3C: Byte)  ==== 0x2CF5493C
    T ~ ip.byteTo(1)(0xAA.toByte) ==== 0x2CF5AA17
    T ~ ip.byteTo(2)(0x64: Byte)  ==== 0x2C644917
    T ~ ip.byteTo(3)(0xE1.toByte) ==== 0xE1F54917
    T ~ ip.reverseBytes ==== 0x1749F52C
    T ~ ip.short(0) ==== 0x4917.toShort
    T ~ ip.short(1) ==== 0x2CF5.toShort
    T ~ ip.shortTo(0)(0x5E4D: Short) ==== 0x2CF55E4D
    T ~ ip.shortTo(1)(0x5E4D: Short) ==== 0x5E4D4917
    T ~ ip.reverseShorts ==== 0x49172CF5
    T ~ ip.char(0) ==== 0x4917.toChar
    T ~ ip.char(1) ==== 0x2CF5.toChar
    T ~ ip.charTo(0)(0x5E4D: Char) ==== 0x2CF55E4D
    T ~ ip.charTo(1)(0x5E4D: Char) ==== 0x5E4D4917
    T ~ ip.reverseChars ==== 0x49172CF5

    var lp = 0xD30AB6E82CF54917L
    var lq = 0x2CF54917D30AB6E8L
    T ~ lp ==== Bit.L(1,1,0,1,0,0,1,1, 0,0,0,0,1,0,1,0, 1,0,1,1,0,1,1,0, 1,1,1,0,1,0,0,0, 0,0,1,0,1,1,0,0, 1,1,1,1,0,1,0,1, 0,1,0,0,1,0,0,1, 0,0,0,1,0,1,1,1)
    T ~ lq ==== Bit.L(0,0,1,0,1,1,0,0, 1,1,1,1,0,1,0,1, 0,1,0,0,1,0,0,1, 0,0,0,1,0,1,1,1, 1,1,0,1,0,0,1,1, 0,0,0,0,1,0,1,0, 1,0,1,1,0,1,1,0, 1,1,1,0,1,0,0,0)
    T ~ lp.bit( 0) ==== 1
    T ~ lq.bit( 0) ==== 0
    T ~ lp.bit( 1) ==== 1
    T ~ lq.bit( 1) ==== 0
    T ~ lp.bit( 2) ==== 1
    T ~ lq.bit( 2) ==== 0
    T ~ lp.bit( 3) ==== 0
    T ~ lq.bit( 3) ==== 1
    T ~ lp.bit( 4) ==== 1
    T ~ lq.bit( 4) ==== 0
    T ~ lp.bit( 5) ==== 0
    T ~ lq.bit( 5) ==== 1
    T ~ lp.bit( 6) ==== 0
    T ~ lq.bit( 6) ==== 1
    T ~ lp.bit( 7) ==== 0
    T ~ lq.bit( 7) ==== 1
    T ~ lp.bit( 8) ==== 1
    T ~ lq.bit( 8) ==== 0
    T ~ lp.bit( 9) ==== 0
    T ~ lq.bit( 9) ==== 1
    T ~ lp.bit(10) ==== 0
    T ~ lq.bit(10) ==== 1
    T ~ lp.bit(11) ==== 1
    T ~ lq.bit(11) ==== 0
    T ~ lp.bit(12) ==== 0
    T ~ lq.bit(12) ==== 1
    T ~ lp.bit(13) ==== 0
    T ~ lq.bit(13) ==== 1
    T ~ lp.bit(14) ==== 1
    T ~ lq.bit(14) ==== 0
    T ~ lp.bit(15) ==== 0
    T ~ lq.bit(15) ==== 1
    T ~ lp.bit(16) ==== 1
    T ~ lq.bit(16) ==== 0
    T ~ lp.bit(17) ==== 0
    T ~ lq.bit(17) ==== 1
    T ~ lp.bit(18) ==== 1
    T ~ lq.bit(18) ==== 0
    T ~ lp.bit(19) ==== 0
    T ~ lq.bit(19) ==== 1
    T ~ lp.bit(20) ==== 1
    T ~ lq.bit(20) ==== 0
    T ~ lp.bit(21) ==== 1
    T ~ lq.bit(21) ==== 0
    T ~ lp.bit(22) ==== 1
    T ~ lq.bit(22) ==== 0
    T ~ lp.bit(23) ==== 1
    T ~ lq.bit(23) ==== 0
    T ~ lp.bit(24) ==== 0
    T ~ lq.bit(24) ==== 1
    T ~ lp.bit(25) ==== 0
    T ~ lq.bit(25) ==== 1
    T ~ lp.bit(26) ==== 1
    T ~ lq.bit(26) ==== 0
    T ~ lp.bit(27) ==== 1
    T ~ lq.bit(27) ==== 0
    T ~ lp.bit(28) ==== 0
    T ~ lq.bit(28) ==== 1
    T ~ lp.bit(29) ==== 1
    T ~ lq.bit(29) ==== 0
    T ~ lp.bit(30) ==== 0
    T ~ lq.bit(30) ==== 1
    T ~ lp.bit(31) ==== 0
    T ~ lq.bit(31) ==== 1
    T ~ lq.bit( 0+32) ==== 1
    T ~ lp.bit( 0+32) ==== 0
    T ~ lq.bit( 1+32) ==== 1
    T ~ lp.bit( 1+32) ==== 0
    T ~ lq.bit( 2+32) ==== 1
    T ~ lp.bit( 2+32) ==== 0
    T ~ lq.bit( 3+32) ==== 0
    T ~ lp.bit( 3+32) ==== 1
    T ~ lq.bit( 4+32) ==== 1
    T ~ lp.bit( 4+32) ==== 0
    T ~ lq.bit( 5+32) ==== 0
    T ~ lp.bit( 5+32) ==== 1
    T ~ lq.bit( 6+32) ==== 0
    T ~ lp.bit( 6+32) ==== 1
    T ~ lq.bit( 7+32) ==== 0
    T ~ lp.bit( 7+32) ==== 1
    T ~ lq.bit( 8+32) ==== 1
    T ~ lp.bit( 8+32) ==== 0
    T ~ lq.bit( 9+32) ==== 0
    T ~ lp.bit( 9+32) ==== 1
    T ~ lq.bit(10+32) ==== 0
    T ~ lp.bit(10+32) ==== 1
    T ~ lq.bit(11+32) ==== 1
    T ~ lp.bit(11+32) ==== 0
    T ~ lq.bit(12+32) ==== 0
    T ~ lp.bit(12+32) ==== 1
    T ~ lq.bit(13+32) ==== 0
    T ~ lp.bit(13+32) ==== 1
    T ~ lq.bit(14+32) ==== 1
    T ~ lp.bit(14+32) ==== 0
    T ~ lq.bit(15+32) ==== 0
    T ~ lp.bit(15+32) ==== 1
    T ~ lq.bit(16+32) ==== 1
    T ~ lp.bit(16+32) ==== 0
    T ~ lq.bit(17+32) ==== 0
    T ~ lp.bit(17+32) ==== 1
    T ~ lq.bit(18+32) ==== 1
    T ~ lp.bit(18+32) ==== 0
    T ~ lq.bit(19+32) ==== 0
    T ~ lp.bit(19+32) ==== 1
    T ~ lq.bit(20+32) ==== 1
    T ~ lp.bit(20+32) ==== 0
    T ~ lq.bit(21+32) ==== 1
    T ~ lp.bit(21+32) ==== 0
    T ~ lq.bit(22+32) ==== 1
    T ~ lp.bit(22+32) ==== 0
    T ~ lq.bit(23+32) ==== 1
    T ~ lp.bit(23+32) ==== 0
    T ~ lq.bit(24+32) ==== 0
    T ~ lp.bit(24+32) ==== 1
    T ~ lq.bit(25+32) ==== 0
    T ~ lp.bit(25+32) ==== 1
    T ~ lq.bit(26+32) ==== 1
    T ~ lp.bit(26+32) ==== 0
    T ~ lq.bit(27+32) ==== 1
    T ~ lp.bit(27+32) ==== 0
    T ~ lq.bit(28+32) ==== 0
    T ~ lp.bit(28+32) ==== 1
    T ~ lq.bit(29+32) ==== 1
    T ~ lp.bit(29+32) ==== 0
    T ~ lq.bit(30+32) ==== 0
    T ~ lp.bit(30+32) ==== 1
    T ~ lq.bit(31+32) ==== 0
    T ~ lp.bit(31+32) ==== 1
    T ~ lp.bits( 2,  5) ==== Bit.L(1, 0, 1)
    T ~ lq.bits( 3,  8) ==== Bit.L(1, 1, 1, 0, 1)
    T ~ lp.bits( 5, 15) ==== Bit.L(1, 0, 0, 1, 0, 0, 1, 0, 0, 0)
    T ~ lq.bits(13, 16) ==== Bit.L(1, 0, 1)
    T ~ lp.bits( 5, 29) ==== Bit.L(0,1,1,0,0,1,1,1,1,0,1,0,1,0,1,0,0,1,0,0,1,0,0,0)
    T ~ lq.bits(28, 32) ==== Bit.L(1,1,0,1)
    T ~ lp.bits(15, 55) ==== Bit.L(0,0,0,1,0,1,0,1,0,1,1,0,1,1,0,1,1,1,0,1,0,0,0,0,0,1,0,1,1,0,0,1,1,1,1,0,1,0,1,0)
    T ~ lp.bits(60, 64) ==== Bit.L(1,1,0,1)
    T ~ lp.bitTo( 0)(0) ==== (lp - 1)
    T ~ lp.bitTo( 0)(1) ==== lp
    T ~ lq.bitTo( 0)(0) ==== lq
    T ~ lq.bitTo( 0)(1) ==== (lq + 1)
    T ~ lp.bitTo( 1)(0) ==== (lp - 2)
    T ~ lp.bitTo( 1)(1) ==== lp
    T ~ lq.bitTo( 1)(0) ==== lq
    T ~ lq.bitTo( 1)(1) ==== (lq + 2)
    T ~ lp.bitTo( 2)(0) ==== (lp - 4)
    T ~ lp.bitTo( 2)(1) ==== lp
    T ~ lq.bitTo( 2)(0) ==== lq
    T ~ lq.bitTo( 2)(1) ==== (lq + 4)
    T ~ lp.bitTo( 3)(0) ==== lp
    T ~ lp.bitTo( 3)(1) ==== (lp + 8)
    T ~ lq.bitTo( 3)(0) ==== (lq - 8)
    T ~ lq.bitTo( 3)(1) ==== lq
    T ~ lp.bitTo( 4)(0) ==== (lp - 16)
    T ~ lp.bitTo( 4)(1) ==== lp
    T ~ lq.bitTo( 4)(0) ==== lq
    T ~ lq.bitTo( 4)(1) ==== (lq + 16)
    T ~ lp.bitTo( 5)(0) ==== lp
    T ~ lp.bitTo( 5)(1) ==== (lp + 32)
    T ~ lq.bitTo( 5)(0) ==== (lq - 32)
    T ~ lq.bitTo( 5)(1) ==== lq
    T ~ lp.bitTo( 6)(0) ==== lp
    T ~ lp.bitTo( 6)(1) ==== (lp + 64)
    T ~ lq.bitTo( 6)(0) ==== (lq - 64)
    T ~ lq.bitTo( 6)(1) ==== lq
    T ~ lp.bitTo( 7)(0) ==== lp
    T ~ lp.bitTo( 7)(1) ==== (lp + 128)
    T ~ lq.bitTo( 7)(0) ==== (lq - 128)
    T ~ lq.bitTo( 7)(1) ==== lq
    T ~ lp.bitTo( 8)(0) ==== (lp - 0x100)
    T ~ lp.bitTo( 8)(1) ==== lp
    T ~ lq.bitTo( 8)(0) ==== lq
    T ~ lq.bitTo( 8)(1) ==== (lq + 0x100)
    T ~ lp.bitTo( 9)(0) ==== lp
    T ~ lp.bitTo( 9)(1) ==== (lp + 0x200)
    T ~ lq.bitTo( 9)(0) ==== (lq - 0x200)
    T ~ lq.bitTo( 9)(1) ==== lq
    T ~ lp.bitTo(10)(0) ==== lp
    T ~ lp.bitTo(10)(1) ==== (lp + 0x400)
    T ~ lq.bitTo(10)(0) ==== (lq - 0x400)
    T ~ lq.bitTo(10)(1) ==== lq
    T ~ lp.bitTo(11)(0) ==== (lp - 0x800)
    T ~ lp.bitTo(11)(1) ==== lp
    T ~ lq.bitTo(11)(0) ==== lq
    T ~ lq.bitTo(11)(1) ==== (lq + 0x800)
    T ~ lp.bitTo(12)(0) ==== lp
    T ~ lp.bitTo(12)(1) ==== (lp + 0x1000)
    T ~ lq.bitTo(12)(0) ==== (lq - 0x1000)
    T ~ lq.bitTo(12)(1) ==== lq
    T ~ lp.bitTo(13)(0) ==== lp
    T ~ lp.bitTo(13)(1) ==== (lp + 0x2000)
    T ~ lq.bitTo(13)(0) ==== (lq - 0x2000)
    T ~ lq.bitTo(13)(1) ==== lq
    T ~ lp.bitTo(14)(0) ==== (lp - 0x4000)
    T ~ lp.bitTo(14)(1) ==== lp
    T ~ lq.bitTo(14)(0) ==== lq
    T ~ lq.bitTo(14)(1) ==== (lq + 0x4000)
    T ~ lp.bitTo(15)(0) ==== lp
    T ~ lp.bitTo(15)(1) ==== (lp + 0x8000)
    T ~ lq.bitTo(15)(0) ==== (lq - 0x8000)
    T ~ lq.bitTo(15)(1) ==== lq
    T ~ lp.bitTo(16)(0) ==== (lp - 0x10000)
    T ~ lp.bitTo(16)(1) ==== lp
    T ~ lq.bitTo(16)(0) ==== lq
    T ~ lq.bitTo(16)(1) ==== (lq + 0x10000)
    T ~ lp.bitTo(17)(0) ==== lp
    T ~ lp.bitTo(17)(1) ==== (lp + 0x20000)
    T ~ lq.bitTo(17)(0) ==== (lq - 0x20000)
    T ~ lq.bitTo(17)(1) ==== lq
    T ~ lp.bitTo(18)(0) ==== (lp - 0x40000)
    T ~ lp.bitTo(18)(1) ==== lp
    T ~ lq.bitTo(18)(0) ==== lq
    T ~ lq.bitTo(18)(1) ==== (lq + 0x40000)
    T ~ lp.bitTo(19)(0) ==== lp
    T ~ lp.bitTo(19)(1) ==== (lp + 0x80000)
    T ~ lq.bitTo(19)(0) ==== (lq - 0x80000)
    T ~ lq.bitTo(19)(1) ==== lq
    T ~ lp.bitTo(20)(0) ==== (lp - 0x100000)
    T ~ lp.bitTo(20)(1) ==== lp
    T ~ lq.bitTo(20)(0) ==== lq
    T ~ lq.bitTo(20)(1) ==== (lq + 0x100000)
    T ~ lp.bitTo(21)(0) ==== (lp - 0x200000)
    T ~ lp.bitTo(21)(1) ==== lp
    T ~ lq.bitTo(21)(0) ==== lq
    T ~ lq.bitTo(21)(1) ==== (lq + 0x200000)
    T ~ lp.bitTo(22)(0) ==== (lp - 0x400000)
    T ~ lp.bitTo(22)(1) ==== lp
    T ~ lq.bitTo(22)(0) ==== lq
    T ~ lq.bitTo(22)(1) ==== (lq + 0x400000)
    T ~ lp.bitTo(23)(0) ==== (lp - 0x800000)
    T ~ lp.bitTo(23)(1) ==== lp
    T ~ lq.bitTo(23)(0) ==== lq
    T ~ lq.bitTo(23)(1) ==== (lq + 0x800000)
    T ~ lp.bitTo(24)(0) ==== lp
    T ~ lp.bitTo(24)(1) ==== (lp + 0x1000000)
    T ~ lq.bitTo(24)(0) ==== (lq - 0x1000000)
    T ~ lq.bitTo(24)(1) ==== lq
    T ~ lp.bitTo(25)(0) ==== lp
    T ~ lp.bitTo(25)(1) ==== (lp + 0x2000000)
    T ~ lq.bitTo(25)(0) ==== (lq - 0x2000000)
    T ~ lq.bitTo(25)(1) ==== lq
    T ~ lp.bitTo(26)(0) ==== (lp - 0x4000000)
    T ~ lp.bitTo(26)(1) ==== lp
    T ~ lq.bitTo(26)(0) ==== lq
    T ~ lq.bitTo(26)(1) ==== (lq + 0x4000000)
    T ~ lp.bitTo(27)(0) ==== (lp - 0x8000000)
    T ~ lp.bitTo(27)(1) ==== lp
    T ~ lq.bitTo(27)(0) ==== lq
    T ~ lq.bitTo(27)(1) ==== (lq + 0x8000000)
    T ~ lp.bitTo(28)(0) ==== lp
    T ~ lp.bitTo(28)(1) ==== (lp + 0x10000000)
    T ~ lq.bitTo(28)(0) ==== (lq - 0x10000000)
    T ~ lq.bitTo(28)(1) ==== lq
    T ~ lp.bitTo(29)(0) ==== (lp - 0x20000000)
    T ~ lp.bitTo(29)(1) ==== lp
    T ~ lq.bitTo(29)(0) ==== lq
    T ~ lq.bitTo(29)(1) ==== (lq + 0x20000000)
    T ~ lp.bitTo(30)(0) ==== lp
    T ~ lp.bitTo(30)(1) ==== (lp + 0x40000000)
    T ~ lq.bitTo(30)(0) ==== (lq - 0x40000000)
    T ~ lq.bitTo(30)(1) ==== lq
    T ~ lp.bitTo(31)(0) ==== lp
    T ~ lp.bitTo(31)(1) ==== (lp + 0x80000000L)
    T ~ lq.bitTo(31)(0) ==== (lq - 0x80000000L)
    T ~ lq.bitTo(31)(1) ==== lq
    T ~ lq.bitTo( 0+32)(0) ==== (lq - 1*0x100000000L)
    T ~ lq.bitTo( 0+32)(1) ==== lq
    T ~ lp.bitTo( 0+32)(0) ==== lp
    T ~ lp.bitTo( 0+32)(1) ==== (lp + 1*0x100000000L)
    T ~ lq.bitTo( 1+32)(0) ==== (lq - 2*0x100000000L)
    T ~ lq.bitTo( 1+32)(1) ==== lq
    T ~ lp.bitTo( 1+32)(0) ==== lp
    T ~ lp.bitTo( 1+32)(1) ==== (lp + 2*0x100000000L)
    T ~ lq.bitTo( 2+32)(0) ==== (lq - 4*0x100000000L)
    T ~ lq.bitTo( 2+32)(1) ==== lq
    T ~ lp.bitTo( 2+32)(0) ==== lp
    T ~ lp.bitTo( 2+32)(1) ==== (lp + 4*0x100000000L)
    T ~ lq.bitTo( 3+32)(0) ==== lq
    T ~ lq.bitTo( 3+32)(1) ==== (lq + 8*0x100000000L)
    T ~ lp.bitTo( 3+32)(0) ==== (lp - 8*0x100000000L)
    T ~ lp.bitTo( 3+32)(1) ==== lp
    T ~ lq.bitTo( 4+32)(0) ==== (lq - 16*0x100000000L)
    T ~ lq.bitTo( 4+32)(1) ==== lq
    T ~ lp.bitTo( 4+32)(0) ==== lp
    T ~ lp.bitTo( 4+32)(1) ==== (lp + 16*0x100000000L)
    T ~ lq.bitTo( 5+32)(0) ==== lq
    T ~ lq.bitTo( 5+32)(1) ==== (lq + 32*0x100000000L)
    T ~ lp.bitTo( 5+32)(0) ==== (lp - 32*0x100000000L)
    T ~ lp.bitTo( 5+32)(1) ==== lp
    T ~ lq.bitTo( 6+32)(0) ==== lq
    T ~ lq.bitTo( 6+32)(1) ==== (lq + 64*0x100000000L)
    T ~ lp.bitTo( 6+32)(0) ==== (lp - 64*0x100000000L)
    T ~ lp.bitTo( 6+32)(1) ==== lp
    T ~ lq.bitTo( 7+32)(0) ==== lq
    T ~ lq.bitTo( 7+32)(1) ==== (lq + 128*0x100000000L)
    T ~ lp.bitTo( 7+32)(0) ==== (lp - 128*0x100000000L)
    T ~ lp.bitTo( 7+32)(1) ==== lp
    T ~ lq.bitTo( 8+32)(0) ==== (lq - 0x100*0x100000000L)
    T ~ lq.bitTo( 8+32)(1) ==== lq
    T ~ lp.bitTo( 8+32)(0) ==== lp
    T ~ lp.bitTo( 8+32)(1) ==== (lp + 0x100*0x100000000L)
    T ~ lq.bitTo( 9+32)(0) ==== lq
    T ~ lq.bitTo( 9+32)(1) ==== (lq + 0x200*0x100000000L)
    T ~ lp.bitTo( 9+32)(0) ==== (lp - 0x200*0x100000000L)
    T ~ lp.bitTo( 9+32)(1) ==== lp
    T ~ lq.bitTo(10+32)(0) ==== lq
    T ~ lq.bitTo(10+32)(1) ==== (lq + 0x400*0x100000000L)
    T ~ lp.bitTo(10+32)(0) ==== (lp - 0x400*0x100000000L)
    T ~ lp.bitTo(10+32)(1) ==== lp
    T ~ lq.bitTo(11+32)(0) ==== (lq - 0x800*0x100000000L)
    T ~ lq.bitTo(11+32)(1) ==== lq
    T ~ lp.bitTo(11+32)(0) ==== lp
    T ~ lp.bitTo(11+32)(1) ==== (lp + 0x800*0x100000000L)
    T ~ lq.bitTo(12+32)(0) ==== lq
    T ~ lq.bitTo(12+32)(1) ==== (lq + 0x1000*0x100000000L)
    T ~ lp.bitTo(12+32)(0) ==== (lp - 0x1000*0x100000000L)
    T ~ lp.bitTo(12+32)(1) ==== lp
    T ~ lq.bitTo(13+32)(0) ==== lq
    T ~ lq.bitTo(13+32)(1) ==== (lq + 0x2000*0x100000000L)
    T ~ lp.bitTo(13+32)(0) ==== (lp - 0x2000*0x100000000L)
    T ~ lp.bitTo(13+32)(1) ==== lp
    T ~ lq.bitTo(14+32)(0) ==== (lq - 0x4000*0x100000000L)
    T ~ lq.bitTo(14+32)(1) ==== lq
    T ~ lp.bitTo(14+32)(0) ==== lp
    T ~ lp.bitTo(14+32)(1) ==== (lp + 0x4000*0x100000000L)
    T ~ lq.bitTo(15+32)(0) ==== lq
    T ~ lq.bitTo(15+32)(1) ==== (lq + 0x8000*0x100000000L)
    T ~ lp.bitTo(15+32)(0) ==== (lp - 0x8000*0x100000000L)
    T ~ lp.bitTo(15+32)(1) ==== lp
    T ~ lq.bitTo(16+32)(0) ==== (lq - 0x10000*0x100000000L)
    T ~ lq.bitTo(16+32)(1) ==== lq
    T ~ lp.bitTo(16+32)(0) ==== lp
    T ~ lp.bitTo(16+32)(1) ==== (lp + 0x10000*0x100000000L)
    T ~ lq.bitTo(17+32)(0) ==== lq
    T ~ lq.bitTo(17+32)(1) ==== (lq + 0x20000*0x100000000L)
    T ~ lp.bitTo(17+32)(0) ==== (lp - 0x20000*0x100000000L)
    T ~ lp.bitTo(17+32)(1) ==== lp
    T ~ lq.bitTo(18+32)(0) ==== (lq - 0x40000*0x100000000L)
    T ~ lq.bitTo(18+32)(1) ==== lq
    T ~ lp.bitTo(18+32)(0) ==== lp
    T ~ lp.bitTo(18+32)(1) ==== (lp + 0x40000*0x100000000L)
    T ~ lq.bitTo(19+32)(0) ==== lq
    T ~ lq.bitTo(19+32)(1) ==== (lq + 0x80000*0x100000000L)
    T ~ lp.bitTo(19+32)(0) ==== (lp - 0x80000*0x100000000L)
    T ~ lp.bitTo(19+32)(1) ==== lp
    T ~ lq.bitTo(20+32)(0) ==== (lq - 0x100000*0x100000000L)
    T ~ lq.bitTo(20+32)(1) ==== lq
    T ~ lp.bitTo(20+32)(0) ==== lp
    T ~ lp.bitTo(20+32)(1) ==== (lp + 0x100000*0x100000000L)
    T ~ lq.bitTo(21+32)(0) ==== (lq - 0x200000*0x100000000L)
    T ~ lq.bitTo(21+32)(1) ==== lq
    T ~ lp.bitTo(21+32)(0) ==== lp
    T ~ lp.bitTo(21+32)(1) ==== (lp + 0x200000*0x100000000L)
    T ~ lq.bitTo(22+32)(0) ==== (lq - 0x400000*0x100000000L)
    T ~ lq.bitTo(22+32)(1) ==== lq
    T ~ lp.bitTo(22+32)(0) ==== lp
    T ~ lp.bitTo(22+32)(1) ==== (lp + 0x400000*0x100000000L)
    T ~ lq.bitTo(23+32)(0) ==== (lq - 0x800000*0x100000000L)
    T ~ lq.bitTo(23+32)(1) ==== lq
    T ~ lp.bitTo(23+32)(0) ==== lp
    T ~ lp.bitTo(23+32)(1) ==== (lp + 0x800000*0x100000000L)
    T ~ lq.bitTo(24+32)(0) ==== lq
    T ~ lq.bitTo(24+32)(1) ==== (lq + 0x1000000*0x100000000L)
    T ~ lp.bitTo(24+32)(0) ==== (lp - 0x1000000*0x100000000L)
    T ~ lp.bitTo(24+32)(1) ==== lp
    T ~ lq.bitTo(25+32)(0) ==== lq
    T ~ lq.bitTo(25+32)(1) ==== (lq + 0x2000000*0x100000000L)
    T ~ lp.bitTo(25+32)(0) ==== (lp - 0x2000000*0x100000000L)
    T ~ lp.bitTo(25+32)(1) ==== lp
    T ~ lq.bitTo(26+32)(0) ==== (lq - 0x4000000*0x100000000L)
    T ~ lq.bitTo(26+32)(1) ==== lq
    T ~ lp.bitTo(26+32)(0) ==== lp
    T ~ lp.bitTo(26+32)(1) ==== (lp + 0x4000000*0x100000000L)
    T ~ lq.bitTo(27+32)(0) ==== (lq - 0x8000000*0x100000000L)
    T ~ lq.bitTo(27+32)(1) ==== lq
    T ~ lp.bitTo(27+32)(0) ==== lp
    T ~ lp.bitTo(27+32)(1) ==== (lp + 0x8000000*0x100000000L)
    T ~ lq.bitTo(28+32)(0) ==== lq
    T ~ lq.bitTo(28+32)(1) ==== (lq + 0x10000000*0x100000000L)
    T ~ lp.bitTo(28+32)(0) ==== (lp - 0x10000000*0x100000000L)
    T ~ lp.bitTo(28+32)(1) ==== lp
    T ~ lq.bitTo(29+32)(0) ==== (lq - 0x20000000*0x100000000L)
    T ~ lq.bitTo(29+32)(1) ==== lq
    T ~ lp.bitTo(29+32)(0) ==== lp
    T ~ lp.bitTo(29+32)(1) ==== (lp + 0x20000000*0x100000000L)
    T ~ lq.bitTo(30+32)(0) ==== lq
    T ~ lq.bitTo(30+32)(1) ==== (lq + 0x40000000*0x100000000L)
    T ~ lp.bitTo(30+32)(0) ==== (lp - 0x40000000*0x100000000L)
    T ~ lp.bitTo(30+32)(1) ==== lp
    T ~ lq.bitTo(31+32)(0) ==== lq
    T ~ lq.bitTo(31+32)(1) ==== (lq + 0x80000000L*0x100000000L)
    T ~ lp.bitTo(31+32)(0) ==== (lp - 0x80000000L*0x100000000L)
    T ~ lp.bitTo(31+32)(1) ==== lp
    T ~ lq.bitsTo( 0,  2)(0xFFFFFFFFFFFFFFFFL)        ==== 0x2CF54917D30AB6EBL
    T ~ lp.bitsTo( 2,  5)(Bit.L(0,1,0))               ==== 0xD30AB6E82CF5490BL
    T ~ lq.bitsTo( 3,  8)(Bit.L(0,1,0,1,0))           ==== 0x2CF54917D30AB650L
    T ~ lq.bitsTo( 5, 15)(Bit.L(1,1,1,0,0,1,0,0,0,0)) ==== Pack.L(Pack.I(Bit.S(1,1,1,1,0,0,1,0,0,0,0,0,1,0,0,0), 0xD30A.toShort), 0x2CF54917)
    T ~ lp.bitsTo(13, 16)(Bit.L(1,1,0))               ==== 0xD30AB6E82CF5C917L
    T ~ lp.bitsTo( 5, 29)(0x595959)                   ==== Bit.L(1,1,0,1,0,0,1,1,0,0,0,0,1,0,1,0,1,0,1,1,0,1,1,0,1,1,1,0,1,0,0,0,0,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,1,1,0,1,1,1)
    T ~ lq.bitsTo(28, 32)(0xA)                        ==== 0x2CF54917A30AB6E8L
    T ~ lq.bitsTo(15, 55)(0x5959595959L)              ==== Bit.L(0,0,1,0,1,1,0,0,1, 0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,1, 0,1,1,0,1,1,0,1,1,1,0,1,0,0,0)
    T ~ lq.bitsTo(60, 64)(0xC)                        ==== 0xCCF54917D30AB6E8L
    T ~ lp.bitsTo( 0, 64)(0x4EA185F81BFF3195L)        ==== 0x4EA185F81BFF3195L
    T ~ lp.reverseBits ==== Bit.L(1,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,0,0,0,0,0,1,0,1,1,1,0,1,1,0,1,1,0,1,0,1,0,1,0,0,0,0,1,1,0,0,1,0,1,1)
    T ~ lq.bitString ==== "0010110011110101010010010001011111010011000010101011011011101000"
    T ~ lp.hex( 0) ==== Hex.from('7')
    T ~ lp.hex( 1) ==== Hex.from('1')
    T ~ lp.hex( 2) ==== Hex.from('9')
    T ~ lp.hex( 3) ==== Hex.from('4')
    T ~ lp.hex( 4) ==== Hex.from('5')
    T ~ lp.hex( 5) ==== Hex.from('F')
    T ~ lp.hex( 6) ==== Hex.from('C')
    T ~ lp.hex( 7) ==== Hex.from('2')
    T ~ lp.hex( 8) ==== Hex.from('8')
    T ~ lp.hex( 9) ==== Hex.from('E')
    T ~ lp.hex(10) ==== Hex.from('6')
    T ~ lp.hex(11) ==== Hex.from('B')
    T ~ lp.hex(12) ==== Hex.from('A')
    T ~ lp.hex(13) ==== Hex.from('0')
    T ~ lp.hex(14) ==== Hex.from('3')
    T ~ lp.hex(15) ==== Hex.from('D')
    T ~ lq.hex( 0) ==== Hex.from('8')
    T ~ lq.hex( 1) ==== Hex.from('E')
    T ~ lq.hex( 2) ==== Hex.from('6')
    T ~ lq.hex( 3) ==== Hex.from('B')
    T ~ lq.hex( 4) ==== Hex.from('A')
    T ~ lq.hex( 5) ==== Hex.from('0')
    T ~ lq.hex( 6) ==== Hex.from('3')
    T ~ lq.hex( 7) ==== Hex.from('D')
    T ~ lq.hex( 8) ==== Hex.from('7')
    T ~ lq.hex( 9) ==== Hex.from('1')
    T ~ lq.hex(10) ==== Hex.from('9')
    T ~ lq.hex(11) ==== Hex.from('4')
    T ~ lq.hex(12) ==== Hex.from('5')
    T ~ lq.hex(13) ==== Hex.from('F')
    T ~ lq.hex(14) ==== Hex.from('C')
    T ~ lq.hex(15) ==== Hex.from('2')
    T ~ lp.hexTo( 0)(0xC) ==== 0xD30AB6E82CF5491CL
    T ~ lp.hexTo( 1)(0x6) ==== 0xD30AB6E82CF54967L
    T ~ lp.hexTo( 2)(0x5) ==== 0xD30AB6E82CF54517L
    T ~ lp.hexTo( 3)(0xA) ==== 0xD30AB6E82CF5A917L
    T ~ lp.hexTo( 4)(0xB) ==== 0xD30AB6E82CFB4917L
    T ~ lp.hexTo( 5)(0x4) ==== 0xD30AB6E82C454917L
    T ~ lp.hexTo( 6)(0x7) ==== 0xD30AB6E827F54917L
    T ~ lp.hexTo( 7)(0xE) ==== 0xD30AB6E8ECF54917L
    T ~ lp.hexTo( 8)(0x7) ==== 0xD30AB6E72CF54917L
    T ~ lp.hexTo( 9)(0x4) ==== 0xD30AB6482CF54917L
    T ~ lp.hexTo(10)(0xF) ==== 0xD30ABFE82CF54917L
    T ~ lp.hexTo(11)(0x2) ==== 0xD30A26E82CF54917L
    T ~ lp.hexTo(12)(0x9) ==== 0xD309B6E82CF54917L
    T ~ lp.hexTo(13)(0xB) ==== 0xD3BAB6E82CF54917L
    T ~ lp.hexTo(14)(0x6) ==== 0xD60AB6E82CF54917L
    T ~ lp.hexTo(15)(0xA) ==== 0xA30AB6E82CF54917L
    T ~ lp.reverseHex ==== 0x71945FC28E6BA03DL
    T ~ lq.hiHexString ==== "2CF54917D30AB6E8"
    T ~ lq.loHexString ==== "2cf54917d30ab6e8"
    T ~ lq.hexString   ==== "2CF54917D30AB6E8"
    T ~ lp.byte(0) ==== 0x17.toByte
    T ~ lp.byte(1) ==== 0x49.toByte
    T ~ lp.byte(2) ==== 0xF5.toByte
    T ~ lp.byte(3) ==== 0x2C.toByte
    T ~ lp.byte(4) ==== 0xE8.toByte
    T ~ lp.byte(5) ==== 0xB6.toByte
    T ~ lp.byte(6) ==== 0x0A.toByte
    T ~ lp.byte(7) ==== 0xD3.toByte
    T ~ lp.byteTo(0)(0x3C: Byte)  ==== 0xD30AB6E82CF5493CL
    T ~ lp.byteTo(1)(0xAA.toByte) ==== 0xD30AB6E82CF5AA17L
    T ~ lp.byteTo(2)(0x64: Byte)  ==== 0xD30AB6E82C644917L
    T ~ lp.byteTo(3)(0xE1.toByte) ==== 0xD30AB6E8E1F54917L
    T ~ lp.byteTo(4)(0x75: Byte)  ==== 0xD30AB6752CF54917L
    T ~ lp.byteTo(5)(0xEE.toByte) ==== 0xD30AEEE82CF54917L
    T ~ lp.byteTo(6)(0x64: Byte)  ==== 0xD364B6E82CF54917L
    T ~ lp.byteTo(7)(0xA2.toByte) ==== 0xA20AB6E82CF54917L
    T ~ lp.reverseBytes ==== 0x1749F52CE8B60AD3L
    T ~ lp.short(0) ==== 0x4917.toShort
    T ~ lp.short(1) ==== 0x2CF5.toShort
    T ~ lp.short(2) ==== 0xB6E8.toShort
    T ~ lp.short(3) ==== 0xD30A.toShort
    T ~ lp.shortTo(0)(0x5E4D: Short) ==== 0xD30AB6E82CF55E4DL
    T ~ lp.shortTo(1)(0x5E4D: Short) ==== 0xD30AB6E85E4D4917L
    T ~ lp.shortTo(2)(0x5E4D: Short) ==== 0xD30A5E4D2CF54917L
    T ~ lp.shortTo(3)(0x5E4D: Short) ==== 0x5E4DB6E82CF54917L
    T ~ lp.reverseShorts ==== 0x49172CF5B6E8D30AL
    T ~ lp.char(0) ==== 0x4917.toChar
    T ~ lp.char(1) ==== 0x2CF5.toChar
    T ~ lp.char(2) ==== 0xB6E8.toChar
    T ~ lp.char(3) ==== 0xD30A.toChar
    T ~ lp.charTo(0)(0x5E4D: Char) ==== 0xD30AB6E82CF55E4DL
    T ~ lp.charTo(1)(0x5E4D: Char) ==== 0xD30AB6E85E4D4917L
    T ~ lp.charTo(2)(0x5E4D: Char) ==== 0xD30A5E4D2CF54917L
    T ~ lp.charTo(3)(0x5E4D: Char) ==== 0x5E4DB6E82CF54917L
    T ~ lp.reverseChars ==== 0x49172CF5B6E8D30AL
    T ~ lp.int(0) ==== 0x2CF54917
    T ~ lp.int(1) ==== 0xD30AB6E8
    T ~ lp.intTo(0)(0x87EAD223) ==== 0xD30AB6E887EAD223L
    T ~ lp.intTo(1)(0xBEEFDEAD) ==== 0xBEEFDEAD2CF54917L
    T ~ lp.reverseInts ==== 0x2CF54917D30AB6E8L

  @Test
  def temporalTest(): Unit =
    T ~ NanoTime(5L)      ==== 5L
    T ~ NanoTimeDelta(5L) ==== 5L
}
object MathsTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
