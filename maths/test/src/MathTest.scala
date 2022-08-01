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

    T(name) ~ 1500 .roll(rng) ==== 1 + r2 % 1500
    T(name) ~ 1500L.roll(rng) ==== 1 + r2 % 1500L

    {
      given Prng = rng
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
      val s = rng.validString(20.roll(rng))
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
      val bb = ByteBuffer.wrap(new Array[Byte](bytes))
      addToBuffer(bb)
      bb
    final def inArray: Array[Byte] =
      val ab = new Array[Byte](bytes)
      addToArray(ab, 0)
      ab
  }
  case class HZ(z: Boolean) extends H {
    def bytes = 1
    def addToBuffer(bb: ByteBuffer): Unit = bb put (if z then 1: Byte else 0: Byte)
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = if z then 1: Byte else 0: Byte
      index + 1
  }
  case class HB(b: Byte) extends H {
    def bytes = 1
    def addToBuffer(bb: ByteBuffer): Unit = bb put b
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = b
      index + 1
  }
  case class HS(s: Short) extends H {
    def bytes = 2
    def addToBuffer(bb: ByteBuffer): Unit = bb putShort s
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = (s & 0xFF).toByte
      ab(index + 1) = ((s & 0xFF00) >> 8).toByte
      index + 2
  }
  case class HC(c: Char) extends H {
    def bytes = 2
    def addToBuffer(bb: ByteBuffer): Unit = bb putChar c
    def addToArray(ab: Array[Byte], index: Int): Int =
      ab(index) = (c & 0xFF).toByte
      ab(index + 1) = ((c & 0xFF00) >> 8).toByte
      index + 2
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
  }
  class Hp(proxy: H) extends H {
    def bytes = proxy.bytes
    def addToBuffer(bb: ByteBuffer): Unit = proxy.addToBuffer(bb)
    def addToArray(ab: Array[Byte], index: Int): Int = proxy.addToArray(ab, index)
  }
  case class HF(f: Float) extends Hp(HI(java.lang.Float.floatToRawIntBits(f))) {}
  case class HD(d: Double) extends Hp(HL(java.lang.Double.doubleToRawLongBits(d))) {}
  case class HA(ab: Array[Byte], i0: Int, iN: Int) extends H {
    private[this] val j0 = math.max(i0, 0)
    private[this] val jN = math.min(iN, ab.length)
    val bytes = math.max(0, jN - j0)
    def addToBuffer(bb: ByteBuffer): Unit = bb.put(ab, j0, bytes)
    def addToArray(ab: Array[Byte], index: Int): Int =
      System.arraycopy(this.ab, j0, ab, index, bytes)
      index + bytes
  }
  case class Hbb(bb: ByteBuffer) extends H {
    val bytes = bb.position
    def addToBuffer(bb: ByteBuffer): Unit =
      val b = ByteBuffer.wrap(this.bb.array, 0, bytes)
      this.bb put b
    def addToArray(ab: Array[Byte], index: Int): Int =
      var i = index
      val b = ByteBuffer.wrap(this.bb.array, 0, bytes)
      while b.remaining > 0 do
        ab(i) = b.get
        i += 1
      i
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
  }
  case class Hiter(i: scala.collection.Iterable[H]) extends H {
    def bytes = i.foldLeft(0)(_ + _.bytes)
    def addToBuffer(bb: ByteBuffer): Unit = for x <- i do x.addToBuffer(bb)
    def addToArray(ab: Array[Byte], index: Int): Int =
      var j = index
      for x <- i do j = x.addToArray(ab, j)
      j
  }

  def randomHs(n: Int, lim: Int = 128)(rng: Prng): Hiter =
    val a = collection.mutable.ArrayBuffer.empty[H]
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
        case 8 => val k = lim roll rng; val i = rng % k; val j = 1 + rng % (k-i); HA(rng.arrayB(k), i, j)
        case 9 => Hbb(ByteBuffer wrap rng.arrayB(lim roll rng))
        case _ => Hstr(rng.validString((lim/2) roll rng))
      }
      a += h
    }
    Hiter(a)
}
object MathsTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
