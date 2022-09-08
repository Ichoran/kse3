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
    T ~ b.clamp(-12, -4) ==== (-8: Byte)
    T ~ b.clamp(13, 100) ==== (13: Byte)
    T ~ b.clamp(-99, -9) ==== (-9: Byte)
    T ~ b.clamp(55, -99) ==== (55: Byte)
    T ~ b.in(-12, 4)     ==== true
    T ~ b.in(13, 100)    ==== false
    T ~ b.in(-99, -9)    ==== false
    T ~ b.in(44, -12)    ==== false
    T ~ b.toUInt         ==== 248
    T ~ b.toUInt         ==== typed[Int]
    T ~ b.toULong        ==== 248L
    T ~ b.toULong        ==== typed[Long]

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
    T ~ f.rad2deg           =~~= 68.7549354157f
    T ~ f.rad2rev           =~~= 0.1909859317f
    T ~ f.deg2rad           =~~= 0.020943951f
    T ~ f.rev2rad           =~~= 7.5398223686f

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
    T ~ v.len.f32                  =~~= 3.32415402772f
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
    T ~ v.dist(-1.5f, 0.7f).f32    =~~= 3.61247837364f
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
    T ~ (1 + pm)                ==== (pm + 1)
    T ~ (pm + qm).value         ==== 5.4f
    T ~ (pm + qm).error         =~~= 0.538516480713f
    T ~ (pm - 1).value          ==== 2.5f
    T ~ (pm - 1).error          ==== 0.2f
    T ~ (1 - pm)                ==== -(pm - 1)
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

/*
  @Test
  def packedTest: Unit =
    T ~ Bit(false)  ==== 0
    T ~ Bit(true)   ==== 1
    T ~ Bit.flip(1) ==== 0
    T ~ Bit.flip(0) ==== 1

    T ~ Bits.B(                     0) ==== 0x00.toByte
    T ~ Bits.B(                  1, 0) ==== 0x02.toByte
    T ~ Bits.B(               1, 1, 0) ==== 0x06.toByte
    T ~ Bits.B(            1, 1, 1, 0) ==== 0x0E.toByte
    T ~ Bits.B(         0, 1, 1, 1, 0) ==== 0x0E.toByte
    T ~ Bits.B(      1, 0, 1, 1, 1, 0) ==== 0x2E.toByte
    T ~ Bits.B(   1, 1, 0, 1, 1, 1, 0) ==== 0x6E.toByte
    T ~ Bits.B(0, 1, 1, 0, 1, 1, 1, 0) ==== 0x6E.toByte
    T ~ Bits.S(                                             0) ==== 0x0000.toShort
    T ~ Bits.S(                                          1, 0) ==== 0x0002.toShort
    T ~ Bits.S(                                       1, 1, 0) ==== 0x0006.toShort
    T ~ Bits.S(                                    1, 1, 1, 0) ==== 0x000E.toShort
    T ~ Bits.S(                                 0, 1, 1, 1, 0) ==== 0x000E.toShort
    T ~ Bits.S(                              1, 0, 1, 1, 1, 0) ==== 0x002E.toShort
    T ~ Bits.S(                           1, 1, 0, 1, 1, 1, 0) ==== 0x006E.toShort
    T ~ Bits.S(                        0, 1, 1, 0, 1, 1, 1, 0) ==== 0x006E.toShort
    T ~ Bits.S(                     0, 0, 1, 1, 0, 1, 1, 1, 0) ==== 0x006E.toShort
    T ~ Bits.S(                  0, 0, 0, 1, 1, 0, 1, 1, 1, 0) ==== 0x006E.toShort
    T ~ Bits.S(               1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0) ==== 0x046E.toShort
    T ~ Bits.S(            1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0) ==== 0x0C6E.toShort
    T ~ Bits.S(         1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0) ==== 0x1C6E.toShort
    T ~ Bits.S(      0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0) ==== 0x1C6E.toShort
    T ~ Bits.S(   1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0) ==== 0x5C6E.toShort
    T ~ Bits.S(1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0) ==== 0xDC6E.toShort

    T ~ Hex.flip(15)  ==== 0
    T ~ Hex.flip(5)   ==== 10
    T ~ Hex.zero      ==== 0
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

    T ~ Hex.B(     'B') ==== 0x0B.toByte
    T ~ Hex.B('a', 'b') ==== 0xAB.toByte
    T ~ Hex.B(      11) ==== 0x0B.toByte
    T ~ Hex.B( 10,  11) ==== 0xAB.toByte
    T ~ Hex.S(               'B') ==== 0x000B.toShort
    T ~ Hex.S(          'a', 'b') ==== 0x00AB.toShort
    T ~ Hex.S(     '6', 'A', 'b') ==== 0x06AB.toShort
    T ~ Hex.S('f', '6', 'A', 'b') ==== 0xF6AB.toShort
    T ~ Hex.S(                11) ==== 0x000B.toShort
    T ~ Hex.S(           10,  11) ==== 0x00AB.toShort
    T ~ Hex.S(       6,  10,  11) ==== 0x06AB.toShort
    T ~ Hex.S( 15,   6,  10,  11) ==== 0xF6AB.toShort

    T ~ Bytes.S(      0x74) ==== 0x0074.toShort
    T ~ Bytes.S(0x1E, 0x74) ==== 0x1E74.toShort


    val b: Byte = -39
    val p: Byte = (~b).toByte
    T ~ b                       ==== Bits.B(1, 1, 0, 1, 1, 0, 0, 1)
    T ~ p                       ==== Bits.B(0, 0, 1, 0, 0, 1, 1, 0)
    T ~ (b.bit0 + 10*p.bit0)    ==== 1
    T ~ (b.bit1 + 10*p.bit1)    ==== 10
    T ~ (b.bit2 + 10*p.bit2)    ==== 10
    T ~ (b.bit3 + 10*p.bit3)    ==== 1
    T ~ (b.bit4 + 10*p.bit4)    ==== 1
    T ~ (b.bit5 + 10*p.bit5)    ==== 10
    T ~ (b.bit6 + 10*p.bit6)    ==== 1
    T ~ (b.bit7 + 10*p.bit7)    ==== 1
    T ~ (b.bit(0) + 2*p.bit(0)) ==== 1
    T ~ (b.bit(1) + 2*p.bit(1)) ==== 2
    T ~ (b.bit(2) + 2*p.bit(2)) ==== 2
    T ~ (b.bit(3) + 2*p.bit(3)) ==== 1
    T ~ (b.bit(4) + 2*p.bit(4)) ==== 1
    T ~ (b.bit(5) + 2*p.bit(5)) ==== 2
    T ~ (b.bit(6) + 2*p.bit(6)) ==== 1
    T ~ (b.bit(7) + 2*p.bit(7)) ==== 1
    T ~ b.bits(3, 4)            ==== Bits.B(1, 0, 1, 1)
    T ~ b.bitString             ==== "11011001"
    T ~ b.reverseBits           ==== Bits.B(1, 0, 0, 1, 1, 0, 1, 1)
    T ~ b.hex0                  ==== 9
    T ~ b.hex1                  ==== Hex.from('D')
    T ~ b.reverseHex            ==== 0x9D.toByte
    T ~ b.bit0To(0)             ==== 0xD8.toByte
    T ~ b.bit0To(1)             ==== 0xD9.toByte
    T ~ b.bit1To(0)             ==== 0xD9.toByte
    T ~ b.bit1To(1)             ==== 0xDB.toByte
    T ~ b.bit2To(0)             ==== 0xD9.toByte
    T ~ b.bit2To(1)             ==== 0xDD.toByte
    T ~ b.bit3To(0)             ==== 0xD1.toByte
    T ~ b.bit3To(1)             ==== 0xD9.toByte
    T ~ b.bit4To(0)             ==== 0xC9.toByte
    T ~ b.bit4To(1)             ==== 0xD9.toByte
    T ~ b.bit5To(0)             ==== 0xD9.toByte
    T ~ b.bit5To(1)             ==== 0xF9.toByte
    T ~ b.bit6To(0)             ==== 0x99.toByte
    T ~ b.bit6To(1)             ==== 0xD9.toByte
    T ~ b.bit7To(0)             ==== 0x59.toByte
    T ~ b.bit7To(1)             ==== 0xD9.toByte 
    T ~ b.bitTo(0)(0)           ==== 0xD8.toByte
    T ~ b.bitTo(0)(1)           ==== 0xD9.toByte
    T ~ b.bitTo(1)(0)           ==== 0xD9.toByte
    T ~ b.bitTo(1)(1)           ==== 0xDB.toByte
    T ~ b.bitTo(2)(0)           ==== 0xD9.toByte
    T ~ b.bitTo(2)(1)           ==== 0xDD.toByte
    T ~ b.bitTo(3)(0)           ==== 0xD1.toByte
    T ~ b.bitTo(3)(1)           ==== 0xD9.toByte
    T ~ b.bitTo(4)(0)           ==== 0xC9.toByte
    T ~ b.bitTo(4)(1)           ==== 0xD9.toByte
    T ~ b.bitTo(5)(0)           ==== 0xD9.toByte
    T ~ b.bitTo(5)(1)           ==== 0xF9.toByte
    T ~ b.bitTo(6)(0)           ==== 0x99.toByte
    T ~ b.bitTo(6)(1)           ==== 0xD9.toByte
    T ~ b.bitTo(7)(0)           ==== 0x59.toByte
    T ~ b.bitTo(7)(1)           ==== 0xD9.toByte
    T ~ b.bitsTo(3,4)(4)        ==== Bits.B(1, 0, 1, 0, 0, 0, 0, 1)
    T ~ b.hexString             ==== "D9"
    T ~ b.loHexString           ==== "d9"
    T ~ b.hiHexString           ==== "D9"
    T ~ p.hexString             ==== "26"

    val s: Short = 0x6ED9
    val z = (s ^ 0xFFFF).toShort
    T ~ s ==== Bits.S(0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1)
    T ~ z ==== Bits.S(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0)
    T ~ (s.bit0  + 10*z.bit0  + 2*s.bit( 0) + 20*z.bit( 0))    ==== 3
    T ~ (s.bit1  + 10*z.bit1  + 2*s.bit( 1) + 20*z.bit( 1))    ==== 30
    T ~ (s.bit2  + 10*z.bit2  + 2*s.bit( 2) + 20*z.bit( 2))    ==== 30
    T ~ (s.bit3  + 10*z.bit3  + 2*s.bit( 3) + 20*z.bit( 3))    ==== 3
    T ~ (s.bit4  + 10*z.bit4  + 2*s.bit( 4) + 20*z.bit( 4))    ==== 3
    T ~ (s.bit5  + 10*z.bit5  + 2*s.bit( 5) + 20*z.bit( 5))    ==== 30
    T ~ (s.bit6  + 10*z.bit6  + 2*s.bit( 6) + 20*z.bit( 6))    ==== 3
    T ~ (s.bit7  + 10*z.bit7  + 2*s.bit( 7) + 20*z.bit( 7))    ==== 3
    T ~ (s.bit8  + 10*z.bit8  + 2*s.bit( 8) + 20*z.bit( 8))    ==== 30
    T ~ (s.bit9  + 10*z.bit9  + 2*s.bit( 9) + 20*z.bit( 9))    ==== 3
    T ~ (s.bit10 + 10*z.bit10 + 2*s.bit(10) + 20*z.bit(10))    ==== 3
    T ~ (s.bit11 + 10*z.bit11 + 2*s.bit(11) + 20*z.bit(11))    ==== 3
    T ~ (s.bit12 + 10*z.bit12 + 2*s.bit(12) + 20*z.bit(12))    ==== 30
    T ~ (s.bit13 + 10*z.bit13 + 2*s.bit(13) + 20*z.bit(13))    ==== 3
    T ~ (s.bit14 + 10*z.bit14 + 2*s.bit(14) + 20*z.bit(14))    ==== 3
    T ~ (s.bit15 + 10*z.bit15 + 2*s.bit(15) + 20*z.bit(15))    ==== 30
*/
}
object MathsTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
