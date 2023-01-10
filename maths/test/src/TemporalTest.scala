// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022 Rex Kerr and Calico Life Sciences LLC.

package kse.maths.test

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.time._
import java.nio.file.attribute.FileTime
import java.util.concurrent.TimeUnit

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable

import sourcecode.{Line, given}


class TemporalTest() {
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

  def testCreation(): Unit =
    T ~ 1.day                ==== Duration.ofSeconds(86400)
    T ~ 2.days               ==== Duration.ofSeconds(172800)
    T ~ 3.h                  ==== Duration.ofSeconds(10800)
    T ~ 4.m                  ==== Duration.ofSeconds(240)
    T ~ 5.s                  ==== Duration.ofSeconds(5)
    T ~ 6.ms                 ==== Duration.ofSeconds(0, 6000000)
    T ~ 7.us                 ==== Duration.ofSeconds(0, 7000)
    T ~ 8.ns                 ==== Duration.ofSeconds(0, 8)
    T ~ 1.0.day              ==== DoubleDuration(86400)          --: typed[DoubleDuration]
    T ~ 2.0.days             ==== DoubleDuration(86400*2)        --: typed[DoubleDuration]
    T ~ 3.0.h                ==== DoubleDuration(10.8e3)         --: typed[DoubleDuration]
    T ~ 4.0.m                ==== DoubleDuration(240.0)          --: typed[DoubleDuration]
    T ~ 5.0.s                ==== DoubleDuration(5)              --: typed[DoubleDuration]
    T ~ 6.0.ms               ==== DoubleDuration(0.006)          --: typed[DoubleDuration]
    T ~ 7.0.us               ==== DoubleDuration(0.000007)       --: typed[DoubleDuration]
    T ~ 8.0.ns               ==== DoubleDuration(8e-9)           --: typed[DoubleDuration]
    T ~ 1.day_nano           ==== NanoDuration(86400000000000L)  --: typed[NanoDuration]
    T ~ (2: Short).days_nano ==== NanoDuration(172800000000000L) --: typed[NanoDuration]
    T ~ (3: Short).h_nano    ==== NanoDuration(10800000000000L)  --: typed[NanoDuration]
    T ~ (4: Short).m_nano    ==== NanoDuration(240000000000L)    --: typed[NanoDuration]
    T ~ 5.s_nano             ==== NanoDuration(5000000000L)      --: typed[NanoDuration]
    T ~ 6.ms_nano            ==== NanoDuration(6000000L)         --: typed[NanoDuration]
    T ~ 7.us_nano            ==== NanoDuration(7000L)            --: typed[NanoDuration]
    T ~ 8.ns_nano            ==== NanoDuration(8L)               --: typed[NanoDuration]

  def testDuration(): Unit =
    val dmin = DurationCompanion.MIN
    val dmax = DurationCompanion.MAX
    val twoB = 2000000000
    val sfrac = 10985802 + (1 over 3)
    val bfrac = 10985802 + (3 over 7)
    val zfrac = 0 over 7
    val ifrac = 7 over 0
    val nfrac = -7 over 0
    val d = Duration.ofSeconds(839571998156L, 875114185)
    val d2 = Duration.ofSeconds(839572005792L, 499501501)
    T ~ -(2.days)          ==== (-2).days
    T ~ -dmin              ==== DurationCompanion.MAX
    T ~ (5.h + 1.days)     ==== 29.h
    T ~ (twoB.days + 1.ns) ==== Duration.ofSeconds(172800000000000L, 1)
    T ~ (dmax + 1.ns)      ==== dmax
    T ~ (dmin + (-1).h)    ==== dmin
    T ~ (5.h +! 1.days)    ==== 29.h
    T ~ (dmax +! 1.ns)     ==== thrown[ArithmeticException]
    T ~ (dmin +! (-1).h)   ==== thrown[ArithmeticException]
    T ~ (5.h - 1.day)      ==== -19.h
    T ~ (twoB.days - 1.ns) ==== Duration.ofSeconds(172799999999999L, 999999999)
    T ~ (dmin - 1.ns)      ==== dmin
    T ~ (dmax - (-1).ns)   ==== dmax
    T ~ (5.h -! 1.day)     ==== -19.h
    T ~ (dmin -! 1.ns)     ==== thrown[ArithmeticException]
    T ~ (dmax -! (-1.ns))  ==== thrown[ArithmeticException]
    T ~ (10.us * 10000000) ==== 100.s
    T ~ (99999.h * twoB)   ==== Duration.ofSeconds(199998000000000L * 3600)
    T ~ (-999.h*twoB*twoB) ==== dmin
    T ~ ( 999.h*twoB*twoB) ==== dmax
    T ~ (d * 1234567890)   ==== dmax
    T ~ (d * 10985802)     ==== Duration.ofSeconds( 9223371736495794943L, 163801370)
    T ~ (d * -10985802)    ==== Duration.ofSeconds(-9223371736495794944L, 836198630)
    T ~ (-d * 10985802)    ==== Duration.ofSeconds(-9223371736495794944L, 836198630)
    T ~ (-d * -10985802)   ==== Duration.ofSeconds( 9223371736495794943L, 163801370)
    T ~ (d * 10985803)     ==== dmax
    T ~ (d * -10985803)    ==== dmin
    T ~ (-d * 10985803)    ==== dmin
    T ~ (-d * -10985803)   ==== dmax
    T ~ (10.us *! 1000000) ==== 10.s
    T ~ (d *! 10985802)    ==== Duration.ofSeconds( 9223371736495794943L, 163801370)
    T ~ (d *! -10985802)   ==== Duration.ofSeconds(-9223371736495794944L, 836198630)
    T ~ (-d *! 10985802)   ==== Duration.ofSeconds(-9223371736495794944L, 836198630)
    T ~ (-d *! -10985802)  ==== Duration.ofSeconds( 9223371736495794943L, 163801370)
    T ~ (d *! 10985803)    ==== thrown[ArithmeticException]
    T ~ (d *! -10985803)   ==== thrown[ArithmeticException]
    T ~ (-d *! 10985803)   ==== thrown[ArithmeticException]
    T ~ (-d *! -10985803)  ==== thrown[ArithmeticException]
    T ~ (d * sfrac)        ==== Duration.ofSeconds( 9223372016353127662L, 122172765)
    T ~ (d * -sfrac)       ==== Duration.ofSeconds(-9223372016353127663L, 877827235)
    T ~ (-d * sfrac)       ==== Duration.ofSeconds(-9223372016353127663L, 877827235)
    T ~ (-d * -sfrac)      ==== Duration.ofSeconds( 9223372016353127662L, 122172765)
    T ~ (d * bfrac)        ==== dmax
    T ~ (d * -bfrac)       ==== dmin
    T ~ (-d * bfrac)       ==== dmin
    T ~ (-d * -bfrac)      ==== dmax
    T ~ (1.s * zfrac)      ==== Duration.ZERO
    T ~ (1.s * ifrac)      ==== dmax
    T ~ (1.s * nfrac)      ==== dmin
    T ~ ((-1).s * nfrac)   ==== dmax
    T ~ (0.s * ifrac)      ==== Duration.ZERO
    T ~ (50.us / 5)        ==== 10.us
    T ~ (50.us / 15)       ==== 3333.ns
    T ~ (dmax/Int.MaxValue)==== Duration.ofSeconds(4294967298L)
    T ~ (d / Int.MaxValue) ==== (390.s + 956177631.ns)
    T ~ (dmin / (-1))      ==== dmax
    T ~ (5.days / 0)       ==== dmax
    T ~ ((-2).h / 0)       ==== dmin
    T ~ (0.days / 0)       ==== Duration.ZERO
    T ~ (d /! Int.MaxValue)==== (390.s + 956177631.ns)
    T ~ (dmin /! (-1))     ==== thrown[ArithmeticException]
    T ~ (9.h /! 0)         ==== thrown[ArithmeticException]
    T ~ ((-9).h /! 0)      ==== thrown[ArithmeticException]
    T ~ (0.s /! 0)         ==== thrown[ArithmeticException]
    T ~ (d / (1/sfrac))    ==== (d * sfrac)
    T ~ (d / (1/bfrac))    ==== (d * bfrac)
    T ~ (1.s / zfrac)      ==== dmax
    T ~ (-1.s / zfrac)     ==== dmin
    T ~ (1.s / ifrac)      ==== Duration.ZERO
    T ~ (-1.s / nfrac)     ==== Duration.ZERO
    T ~ (d / (3 over 989)) ==== Duration.ofSeconds(276778902059049L, 829309655)
    T ~ (d / d2)           ==== 0 --: typed[Long]
    T ~ (d2 / d)           ==== 1
    T ~ (d / 3.ns)         ==== Long.MaxValue
    T ~ (d / (-3).ns)      ==== Long.MinValue
    T ~ (d % d2)           ==== d
    T ~ (d2 % d)           ==== (d2 - d)
    T ~ (5.days / 45.ns)   ==== 9600000000000L
    T ~ (13518.ns / 92.ns) ==== (13518 / 92)
    T ~ (13518.ns % 92.ns) ==== (13518 % 92).ns
    T ~ (d2 /! d)          ==== 1 --: typed[Long]
    T ~ (d /! 3.ns)        ==== thrown[ArithmeticException]
    T ~ (d.round.us/3.us)  ==== 279857332718958371L
    T ~ (d.round.us%3.us)  ==== 1.us
    T ~ (d < d2)           ==== true
    T ~ (d < d)            ==== false
    T ~ (d2 < d)           ==== false
    T ~ (d <= d2)          ==== true
    T ~ (d <= d)           ==== true
    T ~ (d2 <= d)          ==== false
    T ~ (d >= d2)          ==== false
    T ~ (d >= d)           ==== true
    T ~ (d2 >= d)          ==== true
    T ~ (d > d2)           ==== false
    T ~ (d > d)            ==== false
    T ~ (d2 > d)           ==== true
    T ~ (d min d2)         ==== d
    T ~ (d2 min d)         ==== d
    T ~ (d max d2)         ==== d2
    T ~ (d2 max d)         ==== d2
    T ~ (-d).safeAbs       ==== d
    T ~ dmin.safeAbs       ==== dmax
    T ~ d2.clamp(d, dmax)  ==== d2
    T ~ d.clamp(d2, dmax)  ==== d2
    T ~ dmax.clamp(d, d2)  ==== d2
    T ~ dmax.clamp(d2, d)  ==== d2
    T ~ d2.in(d, dmax)     ==== true
    T ~ d.in(d2, dmax)     ==== false
    T ~ dmax.in(d, d2)     ==== false
    T ~ d2.checkIn(d,dmax) ==== d2
    T ~ d.checkIn(d2,dmax) ==== thrown[ArithmeticException]
    T ~ dmax.checkIn(d,d2) ==== thrown[ArithmeticException]

    val dna = Duration.ofSeconds(9223372036L, 854775807)
    val dnb = Duration.ofSeconds(9223372036L, 854775808)
    val dnc = Duration.ofSeconds(9223372036L, 854775809)
    T ~ (2.s).nano         ==== 2000000000L --: typed[NanoDuration]
    T ~ (-5.days).nano     ==== NanoDuration(-432000000000000L)
    T ~ dmin.nano          ==== NanoDuration.MinValue
    T ~ d.nano             ==== NanoDuration.MaxValue
    T ~ dna.nano           ==== Long.MaxValue
    T ~ dnb.nano           ==== Long.MaxValue
    T ~ (-dna).nano        ==== -Long.MaxValue
    T ~ (-dnb).nano        ==== Long.MinValue
    T ~ (-dnc).nano        ==== Long.MinValue
    T ~ 15.us.checkedNano  ==== 15000
    T ~ dna.checkedNano    ==== dna.nano
    T ~ dnb.checkedNano    ==== thrown[ArithmeticException]
    T ~ (-dnb).checkedNano ==== (-dnb).nano
    T ~ (-dnc).checkedNano ==== thrown[ArithmeticException]
    T ~ (4.ns).D           ==== 4e-9 --: typed[DoubleDuration]
    T ~ (-5.days).D        ==== -432000.0
    T ~ d.D                ==== 8.395719981568751e11
    T ~ dmin.D             ==== -9.223372036854776e18
    T ~ d.into.ns          ==== Long.MaxValue --: typed[Long]
    T ~ 5.us.into.ns       ==== 5000
    T ~ dna.into.ns        ==== Long.MaxValue
    T ~ dnb.into.ns        ==== Long.MaxValue
    T ~ (-5).us.into.ns    ==== -5000
    T ~ (-dna).into.ns     ==== -Long.MaxValue
    T ~ (-dnb).into.ns     ==== Long.MinValue
    T ~ (-dnc).into.ns     ==== Long.MinValue
    T ~ 5.us.checked.ns    ==== 5000
    T ~ dna.checked.ns     ==== Long.MaxValue
    T ~ dnb.checked.ns     ==== thrown[ArithmeticException]
    T ~ (-5).us.checked.ns ==== -5000
    T ~ (-dna).checked.ns  ==== -Long.MaxValue
    T ~ (-dnb).checked.ns  ==== Long.MinValue
    T ~ (-dnc).checked.ns  ==== thrown[ArithmeticException]

    val dua = Duration.ofSeconds(9223372036854L, 775807000)
    val dub = Duration.ofSeconds(9223372036854L, 775808000)
    T ~ d.floor.us    ==== Duration.ofSeconds(839571998156L, 875114000)
    T ~ d.ceil.us     ==== Duration.ofSeconds(839571998156L, 875115000)
    T ~ d.round.us    ==== d.floor.us
    T ~ d.trunc.us    ==== d.floor.us
    T ~ d2.floor.us   ==== Duration.ofSeconds(839572005792L, 499501000)
    T ~ d2.ceil.us    ==== Duration.ofSeconds(839572005792L, 499502000)
    T ~ d2.round.us   ==== d2.ceil.us
    T ~ d2.trunc.us   ==== d2.floor.us
    T ~ (-d).floor.us ==== -(d.ceil.us)
    T ~ (-d).ceil.us  ==== -(d.floor.us)
    T ~ (-d).round.us ==== -(d.round.us)
    T ~ (-d).trunc.us ==== (-d).ceil.us
    T ~ dmax.round.us ==== dmax.floor.us
    T ~ dmax.ceil.us  ==== dmax.floor.us
    T ~ 5.ms.into.us             ==== 5000 --: typed[Long]
    T ~ dua.into.us              ==== Long.MaxValue
    T ~ (dua - 1.ns).into.us     ==== Long.MaxValue - 1
    T ~ dub.into.us              ==== Long.MaxValue
    T ~ (-5).ms.into.us          ==== -5000 --: typed[Long]
    T ~ (-dua).into.us           ==== -Long.MaxValue
    T ~ (-dua - 999.ns).into.us  ==== -Long.MaxValue
    T ~ (-dub).into.us           ==== Long.MinValue
    T ~ (-dub - 999.ns).into.us  ==== Long.MinValue
    T ~ (-dub - 1.us).into.us    ==== Long.MinValue
    T ~ 5.ms.checked.us            ==== 5000 --: typed[Long]
    T ~ dua.checked.us             ==== Long.MaxValue
    T ~ (dua - 1.ns).checked.us    ==== Long.MaxValue - 1
    T ~ dub.checked.us             ==== thrown[ArithmeticException]
    T ~ (-5).ms.checked.us         ==== -5000 --: typed[Long]
    T ~ (-dua).checked.us          ==== -Long.MaxValue
    T ~ (-dua - 999.ns).checked.us ==== -Long.MaxValue
    T ~ (-dub).checked.us          ==== Long.MinValue
    T ~ (-dub - 999.ns).checked.us ==== Long.MinValue
    T ~ (-dub - 1.us).checked.us   ==== thrown[ArithmeticException]
    T ~ 185198.ns.round.into.us        ==== 185 --: typed[Long]
    T ~ dua.round.into.us              ==== Long.MaxValue
    T ~ (dua - 1.ns).round.into.us     ==== Long.MaxValue
    T ~ (dua - 499.ns).round.into.us   ==== Long.MaxValue
    T ~ (dua - 500.ns).round.into.us   ==== Long.MaxValue - 1
    T ~ (dua + 501.ns).round.into.us   ==== Long.MaxValue
    T ~ -981581.ns.round.into.us       ==== -982
    T ~ (-dub).round.into.us           ==== Long.MinValue
    T ~ (-dua - 500.ns).round.into.us  ==== Long.MinValue + 1
    T ~ (-dua - 501.ns).round.into.us  ==== Long.MinValue
    T ~ (-dub - 500.ns).round.into.us  ==== Long.MinValue
    T ~ (-dub - 501.ns).round.into.us  ==== Long.MinValue
    T ~ 185198.ns.round.checked.us       ==== 185 --: typed[Long]
    T ~ dua.round.checked.us             ==== Long.MaxValue
    T ~ (dua - 1.ns).round.checked.us    ==== Long.MaxValue
    T ~ (dua - 499.ns).round.checked.us  ==== Long.MaxValue
    T ~ (dua - 500.ns).round.checked.us  ==== Long.MaxValue - 1
    T ~ (dua + 501.ns).round.checked.us  ==== thrown[ArithmeticException]
    T ~ -981581.ns.round.checked.us      ==== -982
    T ~ (-dub).round.checked.us          ==== Long.MinValue
    T ~ (-dua - 500.ns).round.checked.us ==== Long.MinValue + 1
    T ~ (-dua - 501.ns).round.checked.us ==== Long.MinValue
    T ~ (-dub - 500.ns).round.checked.us ==== Long.MinValue
    T ~ (-dub - 501.ns).round.checked.us ==== thrown[ArithmeticException]
    T ~ 185198.ns.ceil.into.us         ==== 186 --: typed[Long]
    T ~ dua.ceil.into.us               ==== Long.MaxValue
    T ~ (dua - 999.ns).ceil.into.us    ==== Long.MaxValue
    T ~ (dua - 1000.ns).ceil.into.us   ==== Long.MaxValue - 1
    T ~ (dua + 1.ns).ceil.into.us      ==== Long.MaxValue
    T ~ -981581.ns.ceil.into.us        ==== -981
    T ~ (-dub).ceil.into.us            ==== Long.MinValue
    T ~ (-dua - 999.ns).ceil.into.us   ==== Long.MinValue + 1
    T ~ (-dub - 999.ns).ceil.into.us   ==== Long.MinValue
    T ~ (-dub - 1000.ns).ceil.into.us  ==== Long.MinValue
    T ~ 185198.ns.ceil.checked.us        ==== 186 --: typed[Long]
    T ~ dua.ceil.checked.us              ==== Long.MaxValue
    T ~ (dua - 999.ns).ceil.checked.us   ==== Long.MaxValue
    T ~ (dua - 1000.ns).ceil.checked.us  ==== Long.MaxValue - 1
    T ~ (dua + 1.ns).ceil.checked.us     ==== thrown[ArithmeticException]
    T ~ -981581.ns.ceil.checked.us       ==== -981
    T ~ (-dub).ceil.checked.us           ==== Long.MinValue
    T ~ (-dua - 999.ns).ceil.checked.us  ==== Long.MinValue + 1
    T ~ (-dub - 999.ns).ceil.checked.us  ==== Long.MinValue
    T ~ (-dub - 1000.ns).ceil.checked.us ==== thrown[ArithmeticException]
    T ~ 185198.ns.floor.into.us      ==== 185 --: typed[Long]
    T ~ dua.floor.into.us            ==== Long.MaxValue
    T ~ (dua - 1.ns).floor.into.us   ==== Long.MaxValue - 1
    T ~ (dub + 1.ns).floor.into.us   ==== Long.MaxValue
    T ~ -981581.ns.floor.into.us     ==== -982
    T ~ (-dub).floor.into.us         ==== Long.MinValue
    T ~ (-dua).floor.into.us         ==== Long.MinValue + 1
    T ~ (-dua - 1.ns).floor.into.us  ==== Long.MinValue
    T ~ (-dub - 1.ns).floor.into.us  ==== Long.MinValue
    T ~ 185198.ns.floor.checked.us     ==== 185 --: typed[Long]
    T ~ dua.floor.checked.us           ==== Long.MaxValue
    T ~ (dua - 1.ns).floor.checked.us  ==== Long.MaxValue - 1
    T ~ (dub + 1.ns).floor.checked.us  ==== thrown[ArithmeticException]
    T ~ -981581.ns.floor.checked.us    ==== -982
    T ~ (-dub).floor.checked.us        ==== Long.MinValue
    T ~ (-dua).floor.checked.us        ==== Long.MinValue + 1
    T ~ (-dua - 1.ns).floor.checked.us ==== Long.MinValue
    T ~ (-dub - 1.ns).floor.checked.us ==== thrown[ArithmeticException]

    val dma = Duration.ofSeconds(9223372036854775L, 807000000)
    val dmb = Duration.ofSeconds(9223372036854775L, 808000000)
    T ~ d.floor.ms    ==== Duration.ofSeconds(839571998156L, 875000000)
    T ~ d.ceil.ms     ==== Duration.ofSeconds(839571998156L, 876000000)
    T ~ d.round.ms    ==== d.floor.ms
    T ~ d.trunc.ms    ==== d.floor.ms
    T ~ d2.floor.ms   ==== Duration.ofSeconds(839572005792L, 499000000)
    T ~ d2.ceil.ms    ==== Duration.ofSeconds(839572005792L, 500000000)
    T ~ d2.round.ms   ==== d2.ceil.ms
    T ~ d2.trunc.ms   ==== d2.floor.ms
    T ~ (-d).floor.ms ==== -(d.ceil.ms)
    T ~ (-d).ceil.ms  ==== -(d.floor.ms)
    T ~ (-d).round.ms ==== -(d.round.ms)
    T ~ (-d).trunc.ms ==== (-d).ceil.ms
    T ~ dmax.round.ms ==== dmax.floor.ms
    T ~ dmax.ceil.ms  ==== dmax.floor.ms
    T ~ 5.s.into.ms                 ==== 5000 --: typed[Long]
    T ~ dma.into.ms                 ==== Long.MaxValue
    T ~ (dma - 1.ns).into.ms        ==== Long.MaxValue - 1
    T ~ dmb.into.ms                 ==== Long.MaxValue
    T ~ (-5).s.into.ms              ==== -5000 --: typed[Long]
    T ~ (-dma).into.ms              ==== -Long.MaxValue
    T ~ (-dma - 999999.ns).into.ms  ==== -Long.MaxValue
    T ~ (-dmb).into.ms              ==== Long.MinValue
    T ~ (-dmb - 999999.ns).into.ms  ==== Long.MinValue
    T ~ (-dmb - 1.ms).into.ms       ==== Long.MinValue
    T ~ 5.s.checked.ms                ==== 5000 --: typed[Long]
    T ~ dma.checked.ms                ==== Long.MaxValue
    T ~ (dma - 1.ns).checked.ms       ==== Long.MaxValue - 1
    T ~ dmb.checked.ms                ==== thrown[ArithmeticException]
    T ~ (-5).s.checked.ms             ==== -5000 --: typed[Long]
    T ~ (-dma).checked.ms             ==== -Long.MaxValue
    T ~ (-dma - 999999.ns).checked.ms ==== -Long.MaxValue
    T ~ (-dmb).checked.ms             ==== Long.MinValue
    T ~ (-dmb - 999999.ns).checked.ms ==== Long.MinValue
    T ~ (-dmb - 1.ms).checked.ms      ==== thrown[ArithmeticException]
    T ~ 185198.us.round.into.ms           ==== 185 --: typed[Long]
    T ~ dma.round.into.ms                 ==== Long.MaxValue
    T ~ (dma - 1.ns).round.into.ms        ==== Long.MaxValue
    T ~ (dma - 499999.ns).round.into.ms   ==== Long.MaxValue
    T ~ (dma - 500000.ns).round.into.ms   ==== Long.MaxValue - 1
    T ~ (dma + 500001.ns).round.into.ms   ==== Long.MaxValue
    T ~ -981581.us.round.into.ms          ==== -982
    T ~ (-dmb).round.into.ms              ==== Long.MinValue
    T ~ (-dma - 500000.ns).round.into.ms  ==== Long.MinValue + 1
    T ~ (-dma - 500001.ns).round.into.ms  ==== Long.MinValue
    T ~ (-dmb - 500000.ns).round.into.ms  ==== Long.MinValue
    T ~ (-dmb - 500001.ns).round.into.ms  ==== Long.MinValue
    T ~ 185198.us.round.checked.ms          ==== 185 --: typed[Long]
    T ~ dma.round.checked.ms                ==== Long.MaxValue
    T ~ (dma - 1.ns).round.checked.ms       ==== Long.MaxValue
    T ~ (dma - 499999.ns).round.checked.ms  ==== Long.MaxValue
    T ~ (dma - 500000.ns).round.checked.ms  ==== Long.MaxValue - 1
    T ~ (dma + 500001.ns).round.checked.ms  ==== thrown[ArithmeticException]
    T ~ -981581.us.round.checked.ms         ==== -982
    T ~ (-dmb).round.checked.ms             ==== Long.MinValue
    T ~ (-dma - 500000.ns).round.checked.ms ==== Long.MinValue + 1
    T ~ (-dma - 500001.ns).round.checked.ms ==== Long.MinValue
    T ~ (-dmb - 500000.ns).round.checked.ms ==== Long.MinValue
    T ~ (-dmb - 500001.ns).round.checked.ms ==== thrown[ArithmeticException]
    T ~ 185198.us.ceil.into.ms            ==== 186 --: typed[Long]
    T ~ dma.ceil.into.ms                  ==== Long.MaxValue
    T ~ (dma - 999999.ns).ceil.into.ms    ==== Long.MaxValue
    T ~ (dma - 1000000.ns).ceil.into.ms   ==== Long.MaxValue - 1
    T ~ (dma + 1.ns).ceil.into.ms         ==== Long.MaxValue
    T ~ -981581.us.ceil.into.ms           ==== -981
    T ~ (-dmb).ceil.into.ms               ==== Long.MinValue
    T ~ (-dma - 999999.ns).ceil.into.ms   ==== Long.MinValue + 1
    T ~ (-dmb - 999999.ns).ceil.into.ms   ==== Long.MinValue
    T ~ (-dmb - 1000000.ns).ceil.into.ms  ==== Long.MinValue
    T ~ 185198.us.ceil.checked.ms           ==== 186 --: typed[Long]
    T ~ dma.ceil.checked.ms                 ==== Long.MaxValue
    T ~ (dma - 999999.ns).ceil.checked.ms   ==== Long.MaxValue
    T ~ (dma - 1000.us).ceil.checked.ms     ==== Long.MaxValue - 1
    T ~ (dma + 1.ns).ceil.checked.ms        ==== thrown[ArithmeticException]
    T ~ -981581.us.ceil.checked.ms          ==== -981
    T ~ (-dmb).ceil.checked.ms              ==== Long.MinValue
    T ~ (-dma - 999999.ns).ceil.checked.ms  ==== Long.MinValue + 1
    T ~ (-dmb - 999999.ns).ceil.checked.ms  ==== Long.MinValue
    T ~ (-dmb - 1000000.ns).ceil.checked.ms ==== thrown[ArithmeticException]
    T ~ 185198.us.floor.into.ms      ==== 185 --: typed[Long]
    T ~ dma.floor.into.ms            ==== Long.MaxValue
    T ~ (dma - 1.ns).floor.into.ms   ==== Long.MaxValue - 1
    T ~ (dmb + 1.ns).floor.into.ms   ==== Long.MaxValue
    T ~ -981581.us.floor.into.ms     ==== -982
    T ~ (-dmb).floor.into.ms         ==== Long.MinValue
    T ~ (-dma).floor.into.ms         ==== Long.MinValue + 1
    T ~ (-dma - 1.ns).floor.into.ms  ==== Long.MinValue
    T ~ (-dmb - 1.ns).floor.into.ms  ==== Long.MinValue
    T ~ 185198.us.floor.checked.ms     ==== 185 --: typed[Long]
    T ~ dma.floor.checked.ms           ==== Long.MaxValue
    T ~ (dma - 1.ns).floor.checked.ms  ==== Long.MaxValue - 1
    T ~ (dmb + 1.ns).floor.checked.ms  ==== thrown[ArithmeticException]
    T ~ -981581.us.floor.checked.ms    ==== -982
    T ~ (-dmb).floor.checked.ms        ==== Long.MinValue
    T ~ (-dma).floor.checked.ms        ==== Long.MinValue + 1
    T ~ (-dma - 1.ns).floor.checked.ms ==== Long.MinValue
    T ~ (-dmb - 1.ns).floor.checked.ms ==== thrown[ArithmeticException]

    val dsp = DurationCompanion.MaxSeconds
    val dsn = DurationCompanion.MinSeconds
    val dsx = DurationCompanion.MAX
    T ~ dsx.floor.s                   ==== dsp
    T ~ dsx.ceil.s                    ==== dsp
    T ~ dsx.round.s                   ==== dsp
    T ~ dsx.trunc.s                   ==== dsp
    T ~ dsp.floor.s                   ==== dsp
    T ~ dsp.ceil.s                    ==== dsp
    T ~ dsp.round.s                   ==== dsp
    T ~ dsp.trunc.s                   ==== dsp
    T ~ (5.s + 999999999.ns).floor.s  ==== 5.s
    T ~ (5.s + 999999999.ns).ceil.s   ==== 6.s
    T ~ (5.s + 999999999.ns).round.s  ==== 6.s
    T ~ (5.s + 999999999.ns).trunc.s  ==== 5.s
    T ~ (5.s + 500000001.ns).floor.s  ==== 5.s
    T ~ (5.s + 500000001.ns).ceil.s   ==== 6.s
    T ~ (5.s + 500000001.ns).round.s  ==== 6.s
    T ~ (5.s + 500000001.ns).trunc.s  ==== 5.s
    T ~ (5.s + 500000000.ns).floor.s  ==== 5.s
    T ~ (5.s + 500000000.ns).ceil.s   ==== 6.s
    T ~ (5.s + 500000000.ns).round.s  ==== 5.s
    T ~ (5.s + 500000000.ns).trunc.s  ==== 5.s
    T ~ (5.s + 1.ns).floor.s          ==== 5.s
    T ~ (5.s + 1.ns).ceil.s           ==== 6.s
    T ~ (5.s + 1.ns).round.s          ==== 5.s
    T ~ (5.s + 1.ns).trunc.s          ==== 5.s
    T ~ dsn.floor.s                   ==== dsn
    T ~ dsn.ceil.s                    ==== dsn
    T ~ dsn.round.s                   ==== dsn
    T ~ dsn.trunc.s                   ==== dsn
    T ~ (dsn + 1.ns).floor.s          ==== dsn
    T ~ (dsn + 1.ns).ceil.s           ==== -dsp
    T ~ (dsn + 1.ns).round.s          ==== dsn
    T ~ (dsn + 1.ns).trunc.s          ==== -dsp
    T ~ (-5.s - 999999999.ns).floor.s ==== -6.s
    T ~ (-5.s - 999999999.ns).ceil.s  ==== -5.s
    T ~ (-5.s - 999999999.ns).round.s ==== -6.s
    T ~ (-5.s - 999999999.ns).trunc.s ==== -5.s
    T ~ (-5.s - 500000001.ns).floor.s ==== -6.s
    T ~ (-5.s - 500000001.ns).ceil.s  ==== -5.s
    T ~ (-5.s - 500000001.ns).round.s ==== -6.s
    T ~ (-5.s - 500000001.ns).trunc.s ==== -5.s
    T ~ (-5.s - 500000000.ns).floor.s ==== -6.s
    T ~ (-5.s - 500000000.ns).ceil.s  ==== -5.s
    T ~ (-5.s - 500000000.ns).round.s ==== -5.s
    T ~ (-5.s - 500000000.ns).trunc.s ==== -5.s
    T ~ (-5.s - 1.ns).floor.s         ==== -6.s
    T ~ (-5.s - 1.ns).ceil.s          ==== -5.s
    T ~ (-5.s - 1.ns).round.s         ==== -5.s
    T ~ (-5.s - 1.ns).trunc.s         ==== -5.s
    T ~ 1751851951.ns.into.s                 ==== 1   --: typed[Long]
    T ~ 1751851951.ns.round.into.s           ==== 2   --: typed[Long]
    T ~ 1751851951.ns.floor.into.s           ==== 1   --: typed[Long]
    T ~ 1751851951.ns.ceil.into.s            ==== 2   --: typed[Long]
    T ~ 1751851951.ns.round.checked.s        ==== 2   --: typed[Long]
    T ~ 1751851951.ns.ceil.checked.s         ==== 2   --: typed[Long]
    T ~ -1751851951.ns.into.s                ==== -1  --: typed[Long]
    T ~ -1751851951.ns.round.into.s          ==== -2  --: typed[Long]
    T ~ -1751851951.ns.floor.into.s          ==== -2  --: typed[Long]
    T ~ -1751851951.ns.ceil.into.s           ==== -1  --: typed[Long]
    T ~ -1751851951.ns.round.checked.s       ==== -2  --: typed[Long]
    T ~ -1751851951.ns.ceil.checked.s        ==== -1  --: typed[Long]
    T ~ (dsp + 1.ns).into.s                  ==== Long.MaxValue
    T ~ (dsp + 1.ns).round.into.s            ==== Long.MaxValue
    T ~ (dsp + 1.ns).round.checked.s         ==== Long.MaxValue
    T ~ (dsp + 1.ns).floor.into.s            ==== Long.MaxValue
    T ~ (dsp + 1.ns).ceil.into.s             ==== Long.MaxValue
    T ~ (dsp + 1.ns).ceil.checked.s          ==== thrown[ArithmeticException]
    T ~ (dsp + 500000000.ns).into.s          ==== Long.MaxValue
    T ~ (dsp + 500000000.ns).round.into.s    ==== Long.MaxValue
    T ~ (dsp + 500000000.ns).round.checked.s ==== Long.MaxValue
    T ~ (dsp + 500000000.ns).floor.into.s    ==== Long.MaxValue
    T ~ (dsp + 500000000.ns).ceil.into.s     ==== Long.MaxValue
    T ~ (dsp + 500000000.ns).ceil.checked.s  ==== thrown[ArithmeticException]
    T ~ (dsp + 500000001.ns).into.s          ==== Long.MaxValue
    T ~ (dsp + 500000001.ns).round.into.s    ==== Long.MaxValue
    T ~ (dsp + 500000001.ns).round.checked.s ==== thrown[ArithmeticException]
    T ~ (dsp + 500000001.ns).floor.into.s    ==== Long.MaxValue
    T ~ (dsp + 500000001.ns).ceil.into.s     ==== Long.MaxValue
    T ~ (dsp + 500000001.ns).ceil.checked.s  ==== thrown[ArithmeticException]
    T ~ (dsn + 1.ns).into.s                  ==== Long.MinValue + 1
    T ~ (dsn + 1.ns).round.into.s            ==== Long.MinValue
    T ~ (dsn + 1.ns).round.checked.s         ==== Long.MinValue
    T ~ (dsn + 1.ns).floor.into.s            ==== Long.MinValue
    T ~ (dsn + 1.ns).ceil.into.s             ==== Long.MinValue + 1
    T ~ (dsn + 1.ns).ceil.checked.s          ==== Long.MinValue + 1
    T ~ (dsn + 499999999.ns).into.s          ==== Long.MinValue + 1
    T ~ (dsn + 499999999.ns).round.into.s    ==== Long.MinValue
    T ~ (dsn + 499999999.ns).round.checked.s ==== Long.MinValue
    T ~ (dsn + 499999999.ns).floor.into.s    ==== Long.MinValue
    T ~ (dsn + 499999999.ns).ceil.into.s     ==== Long.MinValue + 1
    T ~ (dsn + 499999999.ns).ceil.checked.s  ==== Long.MinValue + 1
    T ~ (dsn + 500000000.ns).into.s          ==== Long.MinValue + 1
    T ~ (dsn + 500000000.ns).round.into.s    ==== Long.MinValue + 1
    T ~ (dsn + 500000000.ns).round.checked.s ==== Long.MinValue + 1
    T ~ (dsn + 500000000.ns).floor.into.s    ==== Long.MinValue
    T ~ (dsn + 500000000.ns).ceil.into.s     ==== Long.MinValue + 1
    T ~ (dsn + 500000000.ns).ceil.checked.s  ==== Long.MinValue + 1
    T ~ (dsn + 999999999.ns).into.s          ==== Long.MinValue + 1
    T ~ (dsn + 999999999.ns).round.into.s    ==== Long.MinValue + 1
    T ~ (dsn + 999999999.ns).round.checked.s ==== Long.MinValue + 1
    T ~ (dsn + 999999999.ns).floor.into.s    ==== Long.MinValue
    T ~ (dsn + 999999999.ns).ceil.into.s     ==== Long.MinValue + 1
    T ~ (dsn + 999999999.ns).ceil.checked.s  ==== Long.MinValue + 1

    val minxd = 600.s
    val mincu = Duration.ofSeconds(600, 1)
    val minrd = Duration.ofSeconds(630, 0)
    val minru = Duration.ofSeconds(630, 1)
    val minfd = Duration.ofSeconds(659, 999999999)
    val minxu = 660.s
    T ~ minxd.round.m     ==== minxd
    T ~ minxd.ceil.m      ==== minxd
    T ~ minxd.floor.m     ==== minxd
    T ~ minxd.trunc.m     ==== minxd
    T ~ mincu.round.m     ==== minxd
    T ~ mincu.ceil.m      ==== minxu
    T ~ mincu.floor.m     ==== minxd
    T ~ mincu.trunc.m     ==== minxd
    T ~ minrd.round.m     ==== minxd
    T ~ minrd.ceil.m      ==== minxu
    T ~ minrd.floor.m     ==== minxd
    T ~ minrd.trunc.m     ==== minxd
    T ~ minru.round.m     ==== minxu
    T ~ minru.ceil.m      ==== minxu
    T ~ minru.floor.m     ==== minxd
    T ~ minru.trunc.m     ==== minxd
    T ~ minfd.round.m     ==== minxu
    T ~ minfd.ceil.m      ==== minxu
    T ~ minfd.floor.m     ==== minxd
    T ~ minfd.trunc.m     ==== minxd
    T ~ minxu.round.m     ==== minxu
    T ~ minxu.ceil.m      ==== minxu
    T ~ minxu.floor.m     ==== minxu
    T ~ minxu.trunc.m     ==== minxu
    T ~ (-minxd).round.m  ==== -minxd
    T ~ (-minxd).ceil.m   ==== -minxd
    T ~ (-minxd).floor.m  ==== -minxd
    T ~ (-minxd).trunc.m  ==== -minxd
    T ~ (-mincu).round.m  ==== -minxd
    T ~ (-mincu).ceil.m   ==== -minxd
    T ~ (-mincu).floor.m  ==== -minxu
    T ~ (-mincu).trunc.m  ==== -minxd
    T ~ (-minrd).round.m  ==== -minxd
    T ~ (-minrd).ceil.m   ==== -minxd
    T ~ (-minrd).floor.m  ==== -minxu
    T ~ (-minrd).trunc.m  ==== -minxd
    T ~ (-minru).round.m  ==== -minxu
    T ~ (-minru).ceil.m   ==== -minxd
    T ~ (-minru).floor.m  ==== -minxu
    T ~ (-minru).trunc.m  ==== -minxd
    T ~ (-minfd).round.m  ==== -minxu
    T ~ (-minfd).ceil.m   ==== -minxd
    T ~ (-minfd).floor.m  ==== -minxu
    T ~ (-minfd).trunc.m  ==== -minxd
    T ~ (-minxu).round.m  ==== -minxu
    T ~ (-minxu).ceil.m   ==== -minxu
    T ~ (-minxu).floor.m  ==== -minxu
    T ~ (-minxu).trunc.m  ==== -minxu
    T ~ dmin.round.m      ==== DurationCompanion.MinMinutes
    T ~ dmin.ceil.m       ==== DurationCompanion.MinMinutes
    T ~ dmin.floor.m      ==== DurationCompanion.MinMinutes
    T ~ dmin.trunc.m      ==== DurationCompanion.MinMinutes
    T ~ dmax.round.m      ==== DurationCompanion.MaxMinutes
    T ~ dmax.ceil.m       ==== DurationCompanion.MaxMinutes
    T ~ dmax.floor.m      ==== DurationCompanion.MaxMinutes
    T ~ dmax.trunc.m      ==== DurationCompanion.MaxMinutes
    T ~ minxd.into.m          ==== 10  --: typed[Long]
    T ~ minxd.floor.into.m    ==== 10  --: typed[Long]
    T ~ minxd.round.into.m    ==== 10  --: typed[Long]
    T ~ minxd.ceil.into.m     ==== 10  --: typed[Long]
    T ~ mincu.ceil.into.m     ==== 11
    T ~ minrd.round.into.m    ==== 10
    T ~ minru.round.into.m    ==== 11
    T ~ minfd.floor.into.m    ==== 10
    T ~ minxu.into.m          ==== 11
    T ~ minxu.floor.into.m    ==== 11
    T ~ minxu.round.into.m    ==== 11
    T ~ minxu.ceil.into.m     ==== 11
    T ~ (-minxd).into.m       ==== -10  --: typed[Long]
    T ~ (-minxd).floor.into.m ==== -10  --: typed[Long]
    T ~ (-minxd).round.into.m ==== -10  --: typed[Long]
    T ~ (-minxd).ceil.into.m  ==== -10  --: typed[Long]
    T ~ (-mincu).floor.into.m ==== -11
    T ~ (-minrd).round.into.m ==== -10
    T ~ (-minru).round.into.m ==== -11
    T ~ (-minfd).ceil.into.m  ==== -10
    T ~ (-minxu).into.m       ==== -11
    T ~ (-minxu).floor.into.m ==== -11
    T ~ (-minxu).round.into.m ==== -11
    T ~ (-minxu).ceil.into.m  ==== -11
    T ~ dmin.into.m           ==== Long.MinValue/60
    T ~ dmin.round.into.m     ==== Long.MinValue/60
    T ~ dmin.ceil.into.m      ==== Long.MinValue/60
    T ~ dmin.floor.into.m     ==== Long.MinValue/60 - 1
    T ~ dmax.into.m           ==== Long.MaxValue/60
    T ~ dmax.round.into.m     ==== Long.MaxValue/60
    T ~ dmax.ceil.into.m      ==== Long.MaxValue/60 + 1
    T ~ dmax.floor.into.m     ==== Long.MaxValue/60

    val hrxd = 36000.s
    val hrcu = Duration.ofSeconds(36000, 1)
    val hrrd = Duration.ofSeconds(37800, 0)
    val hrru = Duration.ofSeconds(37800, 1)
    val hrfd = Duration.ofSeconds(39599, 999999999)
    val hrxu = 39600.s
    T ~ hrxd.round.h     ==== hrxd
    T ~ hrxd.ceil.h      ==== hrxd
    T ~ hrxd.floor.h     ==== hrxd
    T ~ hrxd.trunc.h     ==== hrxd
    T ~ hrcu.round.h     ==== hrxd
    T ~ hrcu.ceil.h      ==== hrxu
    T ~ hrcu.floor.h     ==== hrxd
    T ~ hrcu.trunc.h     ==== hrxd
    T ~ hrrd.round.h     ==== hrxd
    T ~ hrrd.ceil.h      ==== hrxu
    T ~ hrrd.floor.h     ==== hrxd
    T ~ hrrd.trunc.h     ==== hrxd
    T ~ hrru.round.h     ==== hrxu
    T ~ hrru.ceil.h      ==== hrxu
    T ~ hrru.floor.h     ==== hrxd
    T ~ hrru.trunc.h     ==== hrxd
    T ~ hrfd.round.h     ==== hrxu
    T ~ hrfd.ceil.h      ==== hrxu
    T ~ hrfd.floor.h     ==== hrxd
    T ~ hrfd.trunc.h     ==== hrxd
    T ~ hrxu.round.h     ==== hrxu
    T ~ hrxu.ceil.h      ==== hrxu
    T ~ hrxu.floor.h     ==== hrxu
    T ~ hrxu.trunc.h     ==== hrxu
    T ~ (-hrxd).round.h  ==== -hrxd
    T ~ (-hrxd).ceil.h   ==== -hrxd
    T ~ (-hrxd).floor.h  ==== -hrxd
    T ~ (-hrxd).trunc.h  ==== -hrxd
    T ~ (-hrcu).round.h  ==== -hrxd
    T ~ (-hrcu).ceil.h   ==== -hrxd
    T ~ (-hrcu).floor.h  ==== -hrxu
    T ~ (-hrcu).trunc.h  ==== -hrxd
    T ~ (-hrrd).round.h  ==== -hrxd
    T ~ (-hrrd).ceil.h   ==== -hrxd
    T ~ (-hrrd).floor.h  ==== -hrxu
    T ~ (-hrrd).trunc.h  ==== -hrxd
    T ~ (-hrru).round.h  ==== -hrxu
    T ~ (-hrru).ceil.h   ==== -hrxd
    T ~ (-hrru).floor.h  ==== -hrxu
    T ~ (-hrru).trunc.h  ==== -hrxd
    T ~ (-hrfd).round.h  ==== -hrxu
    T ~ (-hrfd).ceil.h   ==== -hrxd
    T ~ (-hrfd).floor.h  ==== -hrxu
    T ~ (-hrfd).trunc.h  ==== -hrxd
    T ~ (-hrxu).round.h  ==== -hrxu
    T ~ (-hrxu).ceil.h   ==== -hrxu
    T ~ (-hrxu).floor.h  ==== -hrxu
    T ~ (-hrxu).trunc.h  ==== -hrxu
    T ~ dmin.round.h     ==== DurationCompanion.MinHours
    T ~ dmin.ceil.h      ==== DurationCompanion.MinHours
    T ~ dmin.floor.h     ==== DurationCompanion.MinHours
    T ~ dmin.trunc.h     ==== DurationCompanion.MinHours
    T ~ dmax.round.h     ==== DurationCompanion.MaxHours
    T ~ dmax.ceil.h      ==== DurationCompanion.MaxHours
    T ~ dmax.floor.h     ==== DurationCompanion.MaxHours
    T ~ dmax.trunc.h     ==== DurationCompanion.MaxHours
    T ~ hrxd.into.h          ==== 10  --: typed[Long]
    T ~ hrxd.floor.into.h    ==== 10  --: typed[Long]
    T ~ hrxd.round.into.h    ==== 10  --: typed[Long]
    T ~ hrxd.ceil.into.h     ==== 10  --: typed[Long]
    T ~ hrcu.ceil.into.h     ==== 11
    T ~ hrrd.round.into.h    ==== 10
    T ~ hrru.round.into.h    ==== 11
    T ~ hrfd.floor.into.h    ==== 10
    T ~ hrxu.into.h          ==== 11
    T ~ hrxu.floor.into.h    ==== 11
    T ~ hrxu.round.into.h    ==== 11
    T ~ hrxu.ceil.into.h     ==== 11
    T ~ (-hrxd).into.h       ==== -10  --: typed[Long]
    T ~ (-hrxd).floor.into.h ==== -10  --: typed[Long]
    T ~ (-hrxd).round.into.h ==== -10  --: typed[Long]
    T ~ (-hrxd).ceil.into.h  ==== -10  --: typed[Long]
    T ~ (-hrcu).floor.into.h ==== -11
    T ~ (-hrrd).round.into.h ==== -10
    T ~ (-hrru).round.into.h ==== -11
    T ~ (-hrfd).ceil.into.h  ==== -10
    T ~ (-hrxu).into.h       ==== -11
    T ~ (-hrxu).floor.into.h ==== -11
    T ~ (-hrxu).round.into.h ==== -11
    T ~ (-hrxu).ceil.into.h  ==== -11
    T ~ dmin.into.h          ==== Long.MinValue/3600
    T ~ dmin.round.into.h    ==== Long.MinValue/3600 - 1
    T ~ dmin.ceil.into.h     ==== Long.MinValue/3600
    T ~ dmin.floor.into.h    ==== Long.MinValue/3600 - 1
    T ~ dmax.into.h          ==== Long.MaxValue/3600
    T ~ dmax.round.into.h    ==== Long.MaxValue/3600 + 1
    T ~ dmax.ceil.into.h     ==== Long.MaxValue/3600 + 1
    T ~ dmax.floor.into.h    ==== Long.MaxValue/3600

    val dayxd = 864000.s
    val daycu = Duration.ofSeconds(864000, 1)
    val dayrd = Duration.ofSeconds(907200, 0)
    val dayru = Duration.ofSeconds(907200, 1)
    val dayfd = Duration.ofSeconds(950399, 999999999)
    val dayxu = 950400.s
    T ~ dayxd.round.d     ==== dayxd
    T ~ dayxd.ceil.d      ==== dayxd
    T ~ dayxd.floor.d     ==== dayxd
    T ~ dayxd.trunc.d     ==== dayxd
    T ~ daycu.round.d     ==== dayxd
    T ~ daycu.ceil.d      ==== dayxu
    T ~ daycu.floor.d     ==== dayxd
    T ~ daycu.trunc.d     ==== dayxd
    T ~ dayrd.round.d     ==== dayxd
    T ~ dayrd.ceil.d      ==== dayxu
    T ~ dayrd.floor.d     ==== dayxd
    T ~ dayrd.trunc.d     ==== dayxd
    T ~ dayru.round.d     ==== dayxu
    T ~ dayru.ceil.d      ==== dayxu
    T ~ dayru.floor.d     ==== dayxd
    T ~ dayru.trunc.d     ==== dayxd
    T ~ dayfd.round.d     ==== dayxu
    T ~ dayfd.ceil.d      ==== dayxu
    T ~ dayfd.floor.d     ==== dayxd
    T ~ dayfd.trunc.d     ==== dayxd
    T ~ dayxu.round.d     ==== dayxu
    T ~ dayxu.ceil.d      ==== dayxu
    T ~ dayxu.floor.d     ==== dayxu
    T ~ dayxu.trunc.d     ==== dayxu
    T ~ (-dayxd).round.d  ==== -dayxd
    T ~ (-dayxd).ceil.d   ==== -dayxd
    T ~ (-dayxd).floor.d  ==== -dayxd
    T ~ (-dayxd).trunc.d  ==== -dayxd
    T ~ (-daycu).round.d  ==== -dayxd
    T ~ (-daycu).ceil.d   ==== -dayxd
    T ~ (-daycu).floor.d  ==== -dayxu
    T ~ (-daycu).trunc.d  ==== -dayxd
    T ~ (-dayrd).round.d  ==== -dayxd
    T ~ (-dayrd).ceil.d   ==== -dayxd
    T ~ (-dayrd).floor.d  ==== -dayxu
    T ~ (-dayrd).trunc.d  ==== -dayxd
    T ~ (-dayru).round.d  ==== -dayxu
    T ~ (-dayru).ceil.d   ==== -dayxd
    T ~ (-dayru).floor.d  ==== -dayxu
    T ~ (-dayru).trunc.d  ==== -dayxd
    T ~ (-dayfd).round.d  ==== -dayxu
    T ~ (-dayfd).ceil.d   ==== -dayxd
    T ~ (-dayfd).floor.d  ==== -dayxu
    T ~ (-dayfd).trunc.d  ==== -dayxd
    T ~ (-dayxu).round.d  ==== -dayxu
    T ~ (-dayxu).ceil.d   ==== -dayxu
    T ~ (-dayxu).floor.d  ==== -dayxu
    T ~ (-dayxu).trunc.d  ==== -dayxu
    T ~ dmin.round.d      ==== DurationCompanion.MinDays
    T ~ dmin.ceil.d       ==== DurationCompanion.MinDays
    T ~ dmin.floor.d      ==== DurationCompanion.MinDays
    T ~ dmin.trunc.d      ==== DurationCompanion.MinDays
    T ~ dmax.round.d      ==== DurationCompanion.MaxDays
    T ~ dmax.ceil.d       ==== DurationCompanion.MaxDays
    T ~ dmax.floor.d      ==== DurationCompanion.MaxDays
    T ~ dmax.trunc.d      ==== DurationCompanion.MaxDays
    T ~ dayxd.into.d          ==== 10  --: typed[Long]
    T ~ dayxd.floor.into.d    ==== 10  --: typed[Long]
    T ~ dayxd.round.into.d    ==== 10  --: typed[Long]
    T ~ dayxd.ceil.into.d     ==== 10  --: typed[Long]
    T ~ daycu.ceil.into.d     ==== 11
    T ~ dayrd.round.into.d    ==== 10
    T ~ dayru.round.into.d    ==== 11
    T ~ dayfd.floor.into.d    ==== 10
    T ~ dayxu.into.d          ==== 11
    T ~ dayxu.floor.into.d    ==== 11
    T ~ dayxu.round.into.d    ==== 11
    T ~ dayxu.ceil.into.d     ==== 11
    T ~ (-dayxd).into.d       ==== -10  --: typed[Long]
    T ~ (-dayxd).floor.into.d ==== -10  --: typed[Long]
    T ~ (-dayxd).round.into.d ==== -10  --: typed[Long]
    T ~ (-dayxd).ceil.into.d  ==== -10  --: typed[Long]
    T ~ (-daycu).floor.into.d ==== -11
    T ~ (-dayrd).round.into.d ==== -10
    T ~ (-dayru).round.into.d ==== -11
    T ~ (-dayfd).ceil.into.d  ==== -10
    T ~ (-dayxu).into.d       ==== -11
    T ~ (-dayxu).floor.into.d ==== -11
    T ~ (-dayxu).round.into.d ==== -11
    T ~ (-dayxu).ceil.into.d  ==== -11
    T ~ dmin.into.d           ==== Long.MinValue/86400
    T ~ dmin.round.into.d     ==== Long.MinValue/86400 - 1
    T ~ dmin.ceil.into.d      ==== Long.MinValue/86400
    T ~ dmin.floor.into.d     ==== Long.MinValue/86400 - 1
    T ~ dmax.into.d           ==== Long.MaxValue/86400
    T ~ dmax.round.into.d     ==== Long.MaxValue/86400 + 1
    T ~ dmax.ceil.into.d      ==== Long.MaxValue/86400 + 1
    T ~ dmax.floor.into.d     ==== Long.MaxValue/86400
    T ~ dayru.floor.days      ==== dayru.floor.d
    T ~ dayru.trunc.days      ==== dayru.trunc.d
    T ~ dayru.round.days      ==== dayru.round.d
    T ~ dayru.ceil.days       ==== dayru.ceil.d
    T ~ dayru.into.days       ==== dayru.into.d
    T ~ dayru.round.into.days ==== dayru.round.into.d
    T ~ dayru.floor.into.days ==== dayru.floor.into.d
    T ~ dayru.ceil.into.days  ==== dayru.ceil.into.d

  def testNanoDuration(): Unit =
    val t = tic
    val t0 = System.nanoTime

    val nd  = NanoDuration(238597181528L)
    val nd2 = NanoDuration(239051615115L)
    val nd3 = NanoDuration(259123561422L)
    val nz  = NanoDuration(0L)
    val ndmax = NanoDuration.MaxValue
    val ndmin = NanoDuration.MinValue
    val ndf = 581 over 409
    val sfrac = 38656668 + (1 over 51)
    val bfrac = 38656668 + (1 over 49)
    T ~ NanoDuration(1985711895L)     ==== 1985711895L              --: typed[NanoDuration]
    T ~ NanoDuration.MinValue         ==== Long.MinValue            --: typed[NanoDuration]
    T ~ NanoDuration.MaxValue         ==== Long.MaxValue            --: typed[NanoDuration]
    T ~ nd.unwrap                     ==== 238597181528L            --: typed[Long]
    T ~ (nd + nd2)                    ==== (nd.unwrap + nd2.unwrap) --: typed[NanoDuration]
    T ~ (ndmax + nd)                  ==== ndmax
    T ~ (ndmin + ndmin)               ==== ndmin
    T ~ (nd +! nd2)                   ==== (nd + nd2)               --: typed[NanoDuration]
    T ~ (ndmax +! nd)                 ==== thrown[ArithmeticException]
    T ~ (nd - nd2)                    ==== (nd.unwrap - nd2.unwrap) --: typed[NanoDuration]
    T ~ (ndmin - nd)                  ==== ndmin
    T ~ (nd -! nd2)                   ==== (nd - nd2)               --: typed[NanoDuration]
    T ~ (ndmin -! nd)                 ==== thrown[ArithmeticException]
    T ~ -nd                           ==== -(nd.unwrap)             --: typed[NanoDuration]
    T ~ (nd * 2)                      ==== (nd + nd)                --: typed[NanoDuration]
    T ~ (nd * 9158718951L)            ==== ndmax
    T ~ (nd *! 2)                     ==== (nd * 2)                 --: typed[NanoDuration]
    T ~ (nd *! 9158718951L)           ==== thrown[ArithmeticException]
    T ~ (nd * ndf)                    ==== 338936338552L            --: typed[NanoDuration]
    T ~ (nd * sfrac)                  ==== 9223372036742004812L     --: typed[NanoDuration]
    T ~ (nd * bfrac)                  ==== Long.MaxValue
    T ~ ((-nd) * sfrac)               ==== -(nd * sfrac)
    T ~ (nd * (1 over 0))             ==== ndmax
    T ~ ((-nd) * (1 over 0))          ==== ndmin
    T ~ (0.ns_nano * (0 over 0))      ==== 0L 
    T ~ (nd *! ndf)                   ==== (nd * ndf)               --: typed[NanoDuration]
    T ~ (nd *! sfrac)                 ==== (nd * sfrac)
    T ~ (ndmin *! -1)                 ==== thrown[ArithmeticException]
    T ~ ((-nd) *! bfrac)              ==== thrown[ArithmeticException]
    T ~ (ndmin *! (-3 over 3))        ==== thrown[ArithmeticException]
    T ~ (0.ns_nano *! (0 over 0))     ==== thrown[ArithmeticException]
    T ~ (nz *! (Int.MinValue over 7)) ==== thrown[ArithmeticException]
    T ~ (nd / 409)                    ==== 583367192L               --: typed[NanoDuration]
    T ~ (nd / 411)                    ==== 580528422L
    T ~ ((-nd) / 0)                   ==== Long.MinValue
    T ~ (nz / 0)                      ==== 0L
    T ~ (nd / ndf.reciprocal)         ==== (nd * ndf)
    T ~ (nd / bfrac.reciprocal)       ==== Long.MaxValue
    T ~ (nd / (0 over 1))             ==== Long.MaxValue
    T ~ (nd /! 409)                   ==== (nd / 409)               --: typed[NanoDuration]
    T ~ ((-nd) /! 0)                  ==== thrown[ArithmeticException]
    T ~ (nd /! sfrac.reciprocal)      ==== (nd * sfrac)
    T ~ (nd /! bfrac.reciprocal)      ==== thrown[ArithmeticException]
    T ~ (0.ns_nano /! (0 over 0))     ==== thrown[ArithmeticException]
    T ~ (nd % nd2)                    ==== nd.unwrap                --: typed[NanoDuration]
    T ~ (nd2 % nd)                    ==== (nd2 - nd).unwrap
    T ~ (nd % nz)                     ==== nz
    T ~ (nd < nd2)  ==== true
    T ~ (nd < nd)   ==== false
    T ~ (nd2 < nd)  ==== false
    T ~ (nd <= nd2) ==== true
    T ~ (nd <= nd)  ==== true
    T ~ (nd2 <= nd) ==== false
    T ~ (nd >= nd2) ==== false
    T ~ (nd >= nd)  ==== true
    T ~ (nd2 >= nd) ==== true
    T ~ (nd > nd2)  ==== false
    T ~ (nd > nd)   ==== false
    T ~ (nd2 > nd)  ==== true
    T ~ nd.abs               ==== nd --: typed[NanoDuration]
    T ~ (-nd).abs            ==== nd
    T ~ ndmin.abs            ==== ndmax
    T ~ (nd max nd2)         ==== nd2
    T ~ (nd2 max nd)         ==== nd2
    T ~ (nd min nd2)         ==== nd
    T ~ (nd2 min nd)         ==== nd
    T ~ nd2.clamp(nd, nd3)   ==== nd2 --: typed[NanoDuration]
    T ~ nd.clamp(nd2, nd3)   ==== nd2
    T ~ nd3.clamp(nd, nd2)   ==== nd2
    T ~ nd2.clamp(nd3, nd)   ==== nd3
    T ~ nd2.in(nd, nd3)      ==== true
    T ~ nd.in(nd2, nd3)      ==== false
    T ~ nd3.in(nd, nd2)      ==== false
    T ~ nd2.checkIn(nd, nd3) ==== nd2 --: typed[NanoDuration]
    T ~ nd.checkIn(nd2, nd3) ==== thrown[ArithmeticException]
    T ~ nd3.checkIn(nd, nd2) ==== thrown[ArithmeticException]

    T ~ nd.D                 ==== 238.597181528 --: typed[DoubleDuration]
    T ~ ndmin.D              ==== -9.223372036854775808e9
    T ~ nd.duration          ==== Duration.ofSeconds(238, 597181528)

    T ~ nd.into.ns           ==== nd                             --: typed[Long]
    T ~ nd.into.us           ==== (nd.unwrap / 1000L)            --: typed[Long]
    T ~ nd.into.ms           ==== (nd.unwrap / 1000000L)         --: typed[Long]
    T ~ nd.into.s            ==== (nd.unwrap / 1000000000L)      --: typed[Long]
    T ~ nd.into.m            ==== (nd.unwrap / 60000000000L)     --: typed[Int]
    T ~ ndmax.into.h         ==== (Long.MaxValue/3600000000000L) --: typed[Int]
    T ~ ndmax.into.d         ==== (Long.MaxValue/86400000000000L)--: typed[Int]
    T ~ ndmax.into.days      ==== ndmax.into.d                   --: typed[Int]

    T ~ nd.trunc.us          ==== 238597181000L  --: typed[NanoDuration]
    T ~ nd.floor.us          ==== 238597181000L  --: typed[NanoDuration]
    T ~ nd.round.us          ==== 238597182000L  --: typed[NanoDuration]
    T ~ nd.ceil.us           ==== 238597182000L  --: typed[NanoDuration]
    T ~ nd2.trunc.us         ==== 239051615000L  --: typed[NanoDuration]
    T ~ nd2.floor.us         ==== 239051615000L  --: typed[NanoDuration]
    T ~ nd2.round.us         ==== 239051615000L  --: typed[NanoDuration]
    T ~ nd2.ceil.us          ==== 239051616000L  --: typed[NanoDuration]
    T ~ nd.into.floor.us     ==== 238597181L     --: typed[Long]
    T ~ nd.into.round.us     ==== 238597182L     --: typed[Long]
    T ~ nd.into.ceil.us      ==== 238597182L     --: typed[Long]
    T ~ nd2.into.floor.us    ==== 239051615L     --: typed[Long]
    T ~ nd2.into.round.us    ==== 239051615L     --: typed[Long]
    T ~ nd2.into.ceil.us     ==== 239051616L     --: typed[Long]
    T ~ nd.round.into.us     ==== nd.into.round.us
    T ~ (-nd).trunc.us       ==== (-nd).ceil.us
    T ~ (-nd).floor.us       ==== -(nd.ceil.us)
    T ~ (-nd).ceil.us        ==== -(nd.floor.us)
    T ~ (-nd).floor.into.us  ==== -(nd.ceil.into.us)
    T ~ (-nd).ceil.into.us   ==== -(nd.floor.into.us)

    T ~ nd.trunc.ms          ==== 238597000000L  --: typed[NanoDuration]
    T ~ nd.floor.ms          ==== 238597000000L  --: typed[NanoDuration]
    T ~ nd.round.ms          ==== 238597000000L  --: typed[NanoDuration]
    T ~ nd.ceil.ms           ==== 238598000000L  --: typed[NanoDuration]
    T ~ nd2.trunc.ms         ==== 239051000000L  --: typed[NanoDuration]
    T ~ nd2.floor.ms         ==== 239051000000L  --: typed[NanoDuration]
    T ~ nd2.round.ms         ==== 239052000000L  --: typed[NanoDuration]
    T ~ nd2.ceil.ms          ==== 239052000000L  --: typed[NanoDuration]
    T ~ nd.into.floor.ms     ==== 238597L        --: typed[Long]
    T ~ nd.into.round.ms     ==== 238597L        --: typed[Long]
    T ~ nd.into.ceil.ms      ==== 238598L        --: typed[Long]
    T ~ nd2.into.floor.ms    ==== 239051L        --: typed[Long]
    T ~ nd2.into.round.ms    ==== 239052L        --: typed[Long]
    T ~ nd2.into.ceil.ms     ==== 239052L        --: typed[Long]
    T ~ nd.round.into.ms     ==== nd.into.round.ms
    T ~ (-nd).trunc.ms       ==== (-nd).ceil.ms
    T ~ (-nd).floor.ms       ==== -(nd.ceil.ms)
    T ~ (-nd).ceil.ms        ==== -(nd.floor.ms)
    T ~ (-nd).floor.into.ms  ==== -(nd.ceil.into.ms)
    T ~ (-nd).ceil.into.ms   ==== -(nd.floor.into.ms)

    T ~ nd.trunc.s           ==== 238000000000L  --: typed[NanoDuration]
    T ~ nd.floor.s           ==== 238000000000L  --: typed[NanoDuration]
    T ~ nd.round.s           ==== 239000000000L  --: typed[NanoDuration]
    T ~ nd.ceil.s            ==== 239000000000L  --: typed[NanoDuration]
    T ~ nd2.trunc.s          ==== 239000000000L  --: typed[NanoDuration]
    T ~ nd2.floor.s          ==== 239000000000L  --: typed[NanoDuration]
    T ~ nd2.round.s          ==== 239000000000L  --: typed[NanoDuration]
    T ~ nd2.ceil.s           ==== 240000000000L  --: typed[NanoDuration]
    T ~ nd.into.floor.s      ==== 238L           --: typed[Long]
    T ~ nd.into.round.s      ==== 239L           --: typed[Long]
    T ~ nd.into.ceil.s       ==== 239L           --: typed[Long]
    T ~ nd2.into.floor.s     ==== 239L           --: typed[Long]
    T ~ nd2.into.round.s     ==== 239L           --: typed[Long]
    T ~ nd2.into.ceil.s      ==== 240L           --: typed[Long]
    T ~ nd.round.into.s      ==== nd.into.round.s
    T ~ (-nd).trunc.s        ==== (-nd).ceil.s
    T ~ (-nd).floor.s        ==== -(nd.ceil.s)
    T ~ (-nd).ceil.s         ==== -(nd.floor.s)
    T ~ (-nd).floor.into.s   ==== -(nd.ceil.into.s)
    T ~ (-nd).ceil.into.s    ==== -(nd.floor.into.s)

    T ~ nd.trunc.m           ==== 180000000000L  --: typed[NanoDuration]
    T ~ nd.floor.m           ==== 180000000000L  --: typed[NanoDuration]
    T ~ nd.round.m           ==== 240000000000L  --: typed[NanoDuration]
    T ~ nd.ceil.m            ==== 240000000000L  --: typed[NanoDuration]
    T ~ nd3.trunc.m          ==== 240000000000L  --: typed[NanoDuration]
    T ~ nd3.floor.m          ==== 240000000000L  --: typed[NanoDuration]
    T ~ nd3.round.m          ==== 240000000000L  --: typed[NanoDuration]
    T ~ nd3.ceil.m           ==== 300000000000L  --: typed[NanoDuration]
    T ~ nd.into.floor.m      ==== 3              --: typed[Int]
    T ~ nd.into.round.m      ==== 4              --: typed[Int]
    T ~ nd.into.ceil.m       ==== 4              --: typed[Int]
    T ~ nd3.into.floor.m     ==== 4              --: typed[Int]
    T ~ nd3.into.round.m     ==== 4              --: typed[Int]
    T ~ nd3.into.ceil.m      ==== 5              --: typed[Int]
    T ~ nd.round.into.m      ==== nd.into.round.m
    T ~ (-nd).trunc.m        ==== (-nd).ceil.m
    T ~ (-nd).floor.m        ==== -(nd.ceil.m)
    T ~ (-nd).ceil.m         ==== -(nd.floor.m)
    T ~ (-nd).floor.into.m   ==== -(nd.ceil.into.m)
    T ~ (-nd).ceil.into.m    ==== -(nd.floor.into.m)

    val ba = NanoDuration((86400 +  7*3600 + 14*60 + 42)*1000000000L + 819571235)
    val ha = NanoDuration((86400 + 17*3600 + 35*60 + 11)*1000000000L + 119171835)
    T ~ ba.trunc.h           ==== 111600000000000L  --: typed[NanoDuration]
    T ~ ba.floor.h           ==== 111600000000000L  --: typed[NanoDuration]
    T ~ ba.round.h           ==== 111600000000000L  --: typed[NanoDuration]
    T ~ ba.ceil.h            ==== 115200000000000L  --: typed[NanoDuration]
    T ~ ha.trunc.h           ==== 147600000000000L  --: typed[NanoDuration]
    T ~ ha.floor.h           ==== 147600000000000L  --: typed[NanoDuration]
    T ~ ha.round.h           ==== 151200000000000L  --: typed[NanoDuration]
    T ~ ha.ceil.h            ==== 151200000000000L  --: typed[NanoDuration]
    T ~ ba.into.floor.h      ==== 31                --: typed[Int]
    T ~ ba.into.round.h      ==== 31                --: typed[Int]
    T ~ ba.into.ceil.h       ==== 32                --: typed[Int]
    T ~ ha.into.floor.h      ==== 41                --: typed[Int]
    T ~ ha.into.round.h      ==== 42                --: typed[Int]
    T ~ ha.into.ceil.h       ==== 42                --: typed[Int]
    T ~ ba.round.into.h      ==== ba.into.round.h
    T ~ (-ba).trunc.h        ==== (-ba).ceil.h
    T ~ (-ba).floor.h        ==== -(ba.ceil.h)
    T ~ (-ba).ceil.h         ==== -(ba.floor.h)
    T ~ (-ba).floor.into.h   ==== -(ba.ceil.into.h)
    T ~ (-ba).ceil.into.h    ==== -(ba.floor.into.h)

    T ~ ba.trunc.d           ====  86400000000000L  --: typed[NanoDuration]
    T ~ ba.floor.d           ====  86400000000000L  --: typed[NanoDuration]
    T ~ ba.round.d           ====  86400000000000L  --: typed[NanoDuration]
    T ~ ba.ceil.d            ==== 172800000000000L  --: typed[NanoDuration]
    T ~ ha.trunc.d           ====  86400000000000L  --: typed[NanoDuration]
    T ~ ha.floor.d           ====  86400000000000L  --: typed[NanoDuration]
    T ~ ha.round.d           ==== 172800000000000L  --: typed[NanoDuration]
    T ~ ha.ceil.d            ==== 172800000000000L  --: typed[NanoDuration]
    T ~ ba.into.floor.d      ==== 1                 --: typed[Int]
    T ~ ba.into.round.d      ==== 1                 --: typed[Int]
    T ~ ba.into.ceil.d       ==== 2                 --: typed[Int]
    T ~ ha.into.floor.d      ==== 1                 --: typed[Int]
    T ~ ha.into.round.d      ==== 2                 --: typed[Int]
    T ~ ha.into.ceil.d       ==== 2                 --: typed[Int]
    T ~ ba.round.into.d      ==== ba.into.round.d
    T ~ (-ba).trunc.d        ==== (-ba).ceil.d
    T ~ (-ba).floor.d        ==== -(ba.ceil.d)
    T ~ (-ba).ceil.d         ==== -(ba.floor.d)
    T ~ (-ba).floor.into.d   ==== -(ba.ceil.into.d)
    T ~ (-ba).ceil.into.d    ==== -(ba.floor.into.d)
    T ~ ba.trunc.days        ==== ba.trunc.d        --: typed[NanoDuration]
    T ~ ba.floor.days        ==== ba.floor.d        --: typed[NanoDuration]
    T ~ ba.round.days        ==== ba.round.d        --: typed[NanoDuration]
    T ~ ba.ceil.days         ==== ba.ceil.d         --: typed[NanoDuration]
    T ~ ba.into.floor.days   ==== ba.into.floor.d   --: typed[Int]
    T ~ ba.into.round.days   ==== ba.into.round.d   --: typed[Int]
    T ~ ba.into.ceil.days    ==== ba.into.ceil.d    --: typed[Int]

    T ~ List(ba, nd, nd3, -ha, nd2).sorted ==== List(-ha, nd, nd2, nd3, ba)
    T ~ nd.pr ==== "238597181528 ns"

    val t1 = System.nanoTime
    val dt = t.toc
    val dt2 = NanoDuration.since(t.unwrap)
    T ~ ((t1 - t0) <= dt.unwrap) ==== true
    T ~ (dt >= 0.ns_nano)        ==== true
    T ~ (t1 - t0 > 0)            ==== (dt >= 0.ns_nano)
    T ~ (dt2 >= dt)              ==== true


  def testDoubleDuration(): Unit =
    def dd(d: Double): DoubleDuration = DoubleDuration(d)

    val da = dd(3.9)
    val db = dd(4.2)
    val dc = dd(-3.1)
    val dn = dd(Double.NaN)
    val dnnd = dd((Long.MinValue/1e9).prev)
    val dpnd = dd((Long.MaxValue/1e9).next)
    val dnd = dd(Long.MinValue.toDouble.prev)
    val dpd = dd(Long.MaxValue.toDouble.next)
    T ~ dd(51.1892315)      ==== 51.1892315  --: typed[DoubleDuration]
    T ~ (dd(4.2) + dd(3.9)) ==== (4.2 + 3.9) --: typed[DoubleDuration]
    T ~ (dd(4.2) - dd(3.9)) ==== (4.2 - 3.9) --: typed[DoubleDuration]
    T ~ -dd(4.2)            ==== -4.2        --: typed[DoubleDuration]
    T ~ (dd(4.2) * 3.9)     ==== 4.2 * 3.9   --: typed[DoubleDuration]
    T ~ (dd(4.2) / 3.9)     ==== 4.2 / 3.9   --: typed[DoubleDuration]
    T ~ (dd(4.2) / dd(3.9)) ==== 4.2 / 3.9   --: typed[Double]
    T ~ (dd(4.2) % dd(3.9)) ==== 4.2 % 3.9   --: typed[DoubleDuration]
    T ~ (da < db)  ==== true
    T ~ (da < da)  ==== false
    T ~ (db < da)  ==== false
    T ~ (da < dn)  ==== false
    T ~ (dn < da)  ==== false
    T ~ (da <= db) ==== true
    T ~ (da <= da) ==== true
    T ~ (db <= da) ==== false
    T ~ (da <= dn) ==== false
    T ~ (dn <= da) ==== false
    T ~ (da >= db) ==== false
    T ~ (da >= da) ==== true
    T ~ (db >= da) ==== true
    T ~ (da >= dn) ==== false
    T ~ (dn >= da) ==== false
    T ~ (da > db)  ==== false
    T ~ (da > da)  ==== false
    T ~ (db > da)  ==== true
    T ~ (da > dn)  ==== false
    T ~ (dn > da)  ==== false
    T ~ (-dd(-3.1)).abs    ==== 3.1          --: typed[DoubleDuration]
    T ~ (da max db)        ==== db           --: typed[DoubleDuration]
    T ~ (db max da)        ==== db
    T ~ (dn max da)        ==== dn
    T ~ (da max dn)        ==== dn
    T ~ (da min db)        ==== da           --: typed[DoubleDuration]
    T ~ (db min da)        ==== da
    T ~ (da min dn)        ==== dn
    T ~ (dn min da)        ==== dn
    T ~ da.clamp(dc, db)   ==== da           --: typed[DoubleDuration]
    T ~ dc.clamp(da, db)   ==== da
    T ~ db.clamp(dc, da)   ==== da
    T ~ dc.clamp(db, da)   ==== db
    T ~ dn.clamp(da, db)   ==== dn
    T ~ da.clamp(dn, db)   ==== dn
    T ~ da.clamp(dc, dn)   ==== dn
    T ~ da.in(dc, db)      ==== true
    T ~ dc.in(da, db)      ==== false
    T ~ db.in(dc, da)      ==== false
    T ~ dn.in(da, db)      ==== false
    T ~ da.in(dn, db)      ==== false
    T ~ da.in(dc, dn)      ==== false
    T ~ da.checkIn(dc, db) ==== da
    T ~ dc.checkIn(da, db) ==== thrown[ArithmeticException]
    T ~ db.checkIn(dc, da) ==== thrown[ArithmeticException]
    T ~ dn.checkIn(da, db) ==== thrown[ArithmeticException]
    T ~ da.checkIn(dn, db) ==== thrown[ArithmeticException]
    T ~ da.checkIn(dc, dn) ==== thrown[ArithmeticException]
    T ~ da.nano            ==== NanoDuration(3900000000L) --: typed[NanoDuration]
    T ~ dd(4e10).nano      ==== NanoDuration.MaxValue
    T ~ dnnd.nano          ==== NanoDuration.MinValue
    T ~ dn.nano            ==== 0L
    T ~ da.checkedNano       ==== da.nano                   --: typed[NanoDuration]
    T ~ dd(4e10).checkedNano ==== thrown[ArithmeticException]
    T ~ dnnd.checkedNano     ==== thrown[ArithmeticException]
    T ~ dn.checkedNano       ==== thrown[ArithmeticException]
    T ~ da.duration        ==== 3900.ms                   --: typed[Duration]
    T ~ dpd.duration       ==== DurationCompanion.MAX
    T ~ dnd.duration       ==== DurationCompanion.MIN
    T ~ dn.duration        ==== Duration.ZERO
    T ~ da.checkedDuration   ==== da.duration               --: typed[Duration]
    T ~ dpd.checkedDuration  ==== thrown[ArithmeticException]
    T ~ dnd.checkedDuration  ==== thrown[ArithmeticException]
    T ~ dn.checkedDuration   ==== thrown[ArithmeticException]

    val nsd = dd(3.481957197151)
    val nsu = dd(3.481957197515)
    val nzu = dd(-3.481957197151)
    val nzd = dd(-3.481957197515)
    T ~ nsd.trunc.ns        ==== 3.481957197       --: typed[DoubleDuration]
    T ~ nsd.floor.ns        ==== nsd.trunc.ns      --: typed[DoubleDuration]
    T ~ nsd.round.ns        ==== nsd.trunc.ns      --: typed[DoubleDuration]
    T ~ nsd.ceil.ns         ==== 3.481957198       --: typed[DoubleDuration]
    T ~ nsu.trunc.us        ==== nsd.floor.us      --: typed[DoubleDuration]
    T ~ nsu.floor.ns        ==== nsd.floor.ns      --: typed[DoubleDuration]
    T ~ nsu.round.ns        ==== nsd.ceil.ns       --: typed[DoubleDuration]
    T ~ nsu.ceil.ns         ==== nsd.ceil.ns       --: typed[DoubleDuration]
    T ~ nzu.trunc.ns        ==== -3.481957197      --: typed[DoubleDuration]
    T ~ nzu.floor.ns        ==== -3.481957198      --: typed[DoubleDuration]
    T ~ nzu.round.ns        ==== nzu.trunc.ns      --: typed[DoubleDuration]
    T ~ nzu.ceil.ns         ==== nzu.trunc.ns      --: typed[DoubleDuration]
    T ~ nzd.trunc.ns        ==== nzu.trunc.ns      --: typed[DoubleDuration]
    T ~ nzd.floor.ns        ==== nzu.floor.ns      --: typed[DoubleDuration]
    T ~ nzd.round.ns        ==== nzu.floor.ns      --: typed[DoubleDuration]
    T ~ nzd.ceil.ns         ==== nzu.trunc.ns      --: typed[DoubleDuration]
    T ~ nsd.into.ns         =~~=  3.481957197151e9
    T ~ nsd.floor.into.ns   ====  3.481957197e9    --: typed[Double]
    T ~ nsd.trunc.into.ns   ====  3.481957197e9    --: typed[Double]
    T ~ nsd.round.into.ns   ====  3.481957197e9    --: typed[Double]
    T ~ nsd.ceil.into.ns    ====  3.481957198e9    --: typed[Double]
    T ~ nsu.into.ns         =~~=  3.481957197515e9
    T ~ nsu.floor.into.ns   ====  3.481957197e9    --: typed[Double]
    T ~ nsu.trunc.into.ns   ====  3.481957197e9    --: typed[Double]
    T ~ nsu.round.into.ns   ====  3.481957198e9    --: typed[Double]
    T ~ nsu.ceil.into.ns    ====  3.481957198e9    --: typed[Double]
    T ~ nzu.into.ns         =~~= -3.481957197151e9
    T ~ nzu.floor.into.ns   ==== -3.481957198e9    --: typed[Double]
    T ~ nzu.round.into.ns   ==== -3.481957197e9    --: typed[Double]
    T ~ nzu.ceil.into.ns    ==== -3.481957197e9    --: typed[Double]
    T ~ nzu.trunc.into.ns   ==== -3.481957197e9    --: typed[Double]
    T ~ nzd.into.ns         =~~= -3.481957197515e9
    T ~ nzd.floor.into.ns   ==== -3.481957198e9    --: typed[Double]
    T ~ nzd.round.into.ns   ==== -3.481957198e9    --: typed[Double]
    T ~ nzd.ceil.into.ns    ==== -3.481957197e9    --: typed[Double]
    T ~ nzd.trunc.into.ns   ==== -3.481957197e9    --: typed[Double]
    T ~ nsd.into.trunc.ns   ==== nsd.trunc.into.ns
    T ~ nsd.into.floor.ns   ==== nsd.floor.into.ns
    T ~ nsd.into.round.ns   ==== nsd.round.into.ns
    T ~ nsd.into.ceil.ns    ==== nsd.ceil.into.ns
    T ~ nsd.long.ns         ====  3481957197L      --: typed[Long]
    T ~ nsd.floor.long.ns   ====  3481957197L      --: typed[Long]
    T ~ nsd.round.long.ns   ====  3481957197L      --: typed[Long]
    T ~ nsd.ceil.long.ns    ====  3481957198L      --: typed[Long]
    T ~ nsu.long.ns         ====  3481957197L      --: typed[Long]
    T ~ nsu.floor.long.ns   ====  3481957197L      --: typed[Long]
    T ~ nsu.round.long.ns   ====  3481957198L      --: typed[Long]
    T ~ nsu.ceil.long.ns    ====  3481957198L      --: typed[Long]
    T ~ nzu.long.ns         ==== -3481957197L      --: typed[Long]
    T ~ nzu.floor.long.ns   ==== -3481957198L      --: typed[Long]
    T ~ nzu.round.long.ns   ==== -3481957197L      --: typed[Long]
    T ~ nzu.ceil.long.ns    ==== -3481957197L      --: typed[Long]
    T ~ nzd.long.ns         ==== -3481957197L      --: typed[Long]
    T ~ nzd.floor.long.ns   ==== -3481957198L      --: typed[Long]
    T ~ nzd.round.long.ns   ==== -3481957198L      --: typed[Long]
    T ~ nzd.ceil.long.ns    ==== -3481957197L      --: typed[Long]
    T ~ nsd.long.floor.ns   ==== nsd.floor.long.ns
    T ~ nsd.long.round.ns   ==== nsd.round.long.ns
    T ~ nsd.long.ceil.ns    ==== nsd.ceil.long.ns
    T ~ nsd.checked.ns        ====  3481957197L      --: typed[Long]
    T ~ nsd.floor.checked.ns  ====  3481957197L      --: typed[Long]
    T ~ nsd.round.checked.ns  ====  3481957197L      --: typed[Long]
    T ~ nsd.ceil.checked.ns   ====  3481957198L      --: typed[Long]
    T ~ nsu.checked.ns        ====  3481957197L      --: typed[Long]
    T ~ nsu.floor.checked.ns  ====  3481957197L      --: typed[Long]
    T ~ nsu.round.checked.ns  ====  3481957198L      --: typed[Long]
    T ~ nsu.ceil.checked.ns   ====  3481957198L      --: typed[Long]
    T ~ nzu.checked.ns        ==== -3481957197L      --: typed[Long]
    T ~ nzu.floor.checked.ns  ==== -3481957198L      --: typed[Long]
    T ~ nzu.round.checked.ns  ==== -3481957197L      --: typed[Long]
    T ~ nzu.ceil.checked.ns   ==== -3481957197L      --: typed[Long]
    T ~ nzd.checked.ns        ==== -3481957197L      --: typed[Long]
    T ~ nzd.floor.checked.ns  ==== -3481957198L      --: typed[Long]
    T ~ nzd.round.checked.ns  ==== -3481957198L      --: typed[Long]
    T ~ nzd.ceil.checked.ns   ==== -3481957197L      --: typed[Long]
    T ~ nsd.checked.floor.ns  ==== nsd.floor.checked.ns
    T ~ nsd.checked.round.ns  ==== nsd.round.checked.ns
    T ~ nsd.checked.ceil.ns   ==== nsd.ceil.checked.ns
    T ~ dnnd.checked.ns       ==== thrown[ArithmeticException]
    T ~ dpnd.checked.ns       ==== thrown[ArithmeticException]
    T ~ dnnd.floor.checked.ns ==== thrown[ArithmeticException]
    T ~ dpnd.floor.checked.ns ==== thrown[ArithmeticException]
    T ~ dnnd.round.checked.ns ==== thrown[ArithmeticException]
    T ~ dpnd.round.checked.ns ==== thrown[ArithmeticException]
    T ~ dnnd.ceil.checked.ns  ==== thrown[ArithmeticException]
    T ~ dpnd.ceil.checked.ns  ==== thrown[ArithmeticException]

    val usd = dd(3.481197151)
    val usu = dd(3.481197515)
    val uzu = dd(-3.481197151)
    val uzd = dd(-3.481197515)
    val dnus = dd((Long.MinValue/1e6).prev)
    val dpus = dd((Long.MaxValue/1e6).next)
    T ~ usd.trunc.us        ==== 3.481197       --: typed[DoubleDuration]
    T ~ usd.floor.us        ==== usd.trunc.us   --: typed[DoubleDuration]
    T ~ usd.round.us        ==== usd.trunc.us   --: typed[DoubleDuration]
    T ~ usd.ceil.us         ==== 3.481198       --: typed[DoubleDuration]
    T ~ usu.trunc.us        ==== usd.floor.us   --: typed[DoubleDuration]
    T ~ usu.floor.us        ==== usd.floor.us   --: typed[DoubleDuration]
    T ~ usu.round.us        ==== usd.ceil.us    --: typed[DoubleDuration]
    T ~ usu.ceil.us         ==== usd.ceil.us    --: typed[DoubleDuration]
    T ~ uzu.trunc.us        ==== -3.481197      --: typed[DoubleDuration]
    T ~ uzu.floor.us        ==== -3.481198      --: typed[DoubleDuration]
    T ~ uzu.round.us        ==== uzu.trunc.us   --: typed[DoubleDuration]
    T ~ uzu.ceil.us         ==== uzu.trunc.us   --: typed[DoubleDuration]
    T ~ uzd.trunc.us        ==== uzu.trunc.us   --: typed[DoubleDuration]
    T ~ uzd.floor.us        ==== uzu.floor.us   --: typed[DoubleDuration]
    T ~ uzd.round.us        ==== uzu.floor.us   --: typed[DoubleDuration]
    T ~ uzd.ceil.us         ==== uzu.trunc.us   --: typed[DoubleDuration]
    T ~ usd.into.us         =~~=  3.481197151e6
    T ~ usd.floor.into.us   ====  3.481197e6    --: typed[Double]
    T ~ usd.trunc.into.us   ====  3.481197e6    --: typed[Double]
    T ~ usd.round.into.us   ====  3.481197e6    --: typed[Double]
    T ~ usd.ceil.into.us    ====  3.481198e6    --: typed[Double]
    T ~ usu.into.us         =~~=  3.481197515e6
    T ~ usu.floor.into.us   ====  3.481197e6    --: typed[Double]
    T ~ usu.trunc.into.us   ====  3.481197e6    --: typed[Double]
    T ~ usu.round.into.us   ====  3.481198e6    --: typed[Double]
    T ~ usu.ceil.into.us    ====  3.481198e6    --: typed[Double]
    T ~ uzu.into.us         =~~= -3.481197151e6
    T ~ uzu.floor.into.us   ==== -3.481198e6    --: typed[Double]
    T ~ uzu.round.into.us   ==== -3.481197e6    --: typed[Double]
    T ~ uzu.ceil.into.us    ==== -3.481197e6    --: typed[Double]
    T ~ uzu.trunc.into.us   ==== -3.481197e6    --: typed[Double]
    T ~ uzd.into.us         =~~= -3.481197515e6
    T ~ uzd.floor.into.us   ==== -3.481198e6    --: typed[Double]
    T ~ uzd.round.into.us   ==== -3.481198e6    --: typed[Double]
    T ~ uzd.ceil.into.us    ==== -3.481197e6    --: typed[Double]
    T ~ uzd.trunc.into.us   ==== -3.481197e6    --: typed[Double]
    T ~ usd.into.trunc.us   ==== usd.trunc.into.us
    T ~ usd.into.floor.us   ==== usd.floor.into.us
    T ~ usd.into.round.us   ==== usd.round.into.us
    T ~ usd.into.ceil.us    ==== usd.ceil.into.us
    T ~ usd.long.us         ====  3481197L      --: typed[Long]
    T ~ usd.floor.long.us   ====  3481197L      --: typed[Long]
    T ~ usd.round.long.us   ====  3481197L      --: typed[Long]
    T ~ usd.ceil.long.us    ====  3481198L      --: typed[Long]
    T ~ usu.long.us         ====  3481197L      --: typed[Long]
    T ~ usu.floor.long.us   ====  3481197L      --: typed[Long]
    T ~ usu.round.long.us   ====  3481198L      --: typed[Long]
    T ~ usu.ceil.long.us    ====  3481198L      --: typed[Long]
    T ~ uzu.long.us         ==== -3481197L      --: typed[Long]
    T ~ uzu.floor.long.us   ==== -3481198L      --: typed[Long]
    T ~ uzu.round.long.us   ==== -3481197L      --: typed[Long]
    T ~ uzu.ceil.long.us    ==== -3481197L      --: typed[Long]
    T ~ uzd.long.us         ==== -3481197L      --: typed[Long]
    T ~ uzd.floor.long.us   ==== -3481198L      --: typed[Long]
    T ~ uzd.round.long.us   ==== -3481198L      --: typed[Long]
    T ~ uzd.ceil.long.us    ==== -3481197L      --: typed[Long]
    T ~ usd.long.floor.us   ==== usd.floor.long.us
    T ~ usd.long.round.us   ==== usd.round.long.us
    T ~ usd.long.ceil.us    ==== usd.ceil.long.us
    T ~ usd.checked.us        ====  3481197L      --: typed[Long]
    T ~ usd.floor.checked.us  ====  3481197L      --: typed[Long]
    T ~ usd.round.checked.us  ====  3481197L      --: typed[Long]
    T ~ usd.ceil.checked.us   ====  3481198L      --: typed[Long]
    T ~ usu.checked.us        ====  3481197L      --: typed[Long]
    T ~ usu.floor.checked.us  ====  3481197L      --: typed[Long]
    T ~ usu.round.checked.us  ====  3481198L      --: typed[Long]
    T ~ usu.ceil.checked.us   ====  3481198L      --: typed[Long]
    T ~ uzu.checked.us        ==== -3481197L      --: typed[Long]
    T ~ uzu.floor.checked.us  ==== -3481198L      --: typed[Long]
    T ~ uzu.round.checked.us  ==== -3481197L      --: typed[Long]
    T ~ uzu.ceil.checked.us   ==== -3481197L      --: typed[Long]
    T ~ uzd.checked.us        ==== -3481197L      --: typed[Long]
    T ~ uzd.floor.checked.us  ==== -3481198L      --: typed[Long]
    T ~ uzd.round.checked.us  ==== -3481198L      --: typed[Long]
    T ~ uzd.ceil.checked.us   ==== -3481197L      --: typed[Long]
    T ~ usd.checked.floor.us  ==== usd.floor.checked.us
    T ~ usd.checked.round.us  ==== usd.round.checked.us
    T ~ usd.checked.ceil.us   ==== usd.ceil.checked.us
    T ~ dnus.checked.us       ==== thrown[ArithmeticException]
    T ~ dpus.checked.us       ==== thrown[ArithmeticException]
    T ~ dnus.floor.checked.us ==== thrown[ArithmeticException]
    T ~ dpus.floor.checked.us ==== thrown[ArithmeticException]
    T ~ dnus.round.checked.us ==== thrown[ArithmeticException]
    T ~ dpus.round.checked.us ==== thrown[ArithmeticException]
    T ~ dnus.ceil.checked.us  ==== thrown[ArithmeticException]
    T ~ dpus.ceil.checked.us  ==== thrown[ArithmeticException]

    val msd = dd(3.497151)
    val msu = dd(3.497515)
    val mzu = dd(-3.497151)
    val mzd = dd(-3.497515)
    val dnms = dd((Long.MinValue/1e3).prev)
    val dpms = dd((Long.MaxValue/1e3).next)
    T ~ msd.trunc.ms        ==== 3.497        --: typed[DoubleDuration]
    T ~ msd.floor.ms        ==== msd.trunc.ms --: typed[DoubleDuration]
    T ~ msd.round.ms        ==== msd.trunc.ms --: typed[DoubleDuration]
    T ~ msd.ceil.ms         ==== 3.498        --: typed[DoubleDuration]
    T ~ msu.trunc.ms        ==== msd.floor.ms --: typed[DoubleDuration]
    T ~ msu.floor.ms        ==== msd.floor.ms --: typed[DoubleDuration]
    T ~ msu.round.ms        ==== msd.ceil.ms  --: typed[DoubleDuration]
    T ~ msu.ceil.ms         ==== msd.ceil.ms  --: typed[DoubleDuration]
    T ~ mzu.trunc.ms        ==== -3.497       --: typed[DoubleDuration]
    T ~ mzu.floor.ms        ==== -3.498       --: typed[DoubleDuration]
    T ~ mzu.round.ms        ==== mzu.trunc.ms --: typed[DoubleDuration]
    T ~ mzu.ceil.ms         ==== mzu.trunc.ms --: typed[DoubleDuration]
    T ~ mzd.trunc.ms        ==== mzu.trunc.ms --: typed[DoubleDuration]
    T ~ mzd.floor.ms        ==== mzu.floor.ms --: typed[DoubleDuration]
    T ~ mzd.round.ms        ==== mzu.floor.ms --: typed[DoubleDuration]
    T ~ mzd.ceil.ms         ==== mzu.trunc.ms --: typed[DoubleDuration]
    T ~ msd.into.ms         =~~=  3.497151e3
    T ~ msd.floor.into.ms   ====  3.497e3     --: typed[Double]
    T ~ msd.trunc.into.ms   ====  3.497e3     --: typed[Double]
    T ~ msd.round.into.ms   ====  3.497e3     --: typed[Double]
    T ~ msd.ceil.into.ms    ====  3.498e3     --: typed[Double]
    T ~ msu.into.ms         =~~=  3.497515e3
    T ~ msu.floor.into.ms   ====  3.497e3     --: typed[Double]
    T ~ msu.trunc.into.ms   ====  3.497e3     --: typed[Double]
    T ~ msu.round.into.ms   ====  3.498e3     --: typed[Double]
    T ~ msu.ceil.into.ms    ====  3.498e3     --: typed[Double]
    T ~ mzu.into.ms         =~~= -3.497151e3
    T ~ mzu.floor.into.ms   ==== -3.498e3     --: typed[Double]
    T ~ mzu.round.into.ms   ==== -3.497e3     --: typed[Double]
    T ~ mzu.ceil.into.ms    ==== -3.497e3     --: typed[Double]
    T ~ mzu.trunc.into.ms   ==== -3.497e3     --: typed[Double]
    T ~ mzd.into.ms         =~~= -3.497515e3
    T ~ mzd.floor.into.ms   ==== -3.498e3     --: typed[Double]
    T ~ mzd.round.into.ms   ==== -3.498e3     --: typed[Double]
    T ~ mzd.ceil.into.ms    ==== -3.497e3     --: typed[Double]
    T ~ mzd.trunc.into.ms   ==== -3.497e3     --: typed[Double]
    T ~ msd.into.trunc.ms   ==== msd.trunc.into.ms
    T ~ msd.into.floor.ms   ==== msd.floor.into.ms
    T ~ msd.into.round.ms   ==== msd.round.into.ms
    T ~ msd.into.ceil.ms    ==== msd.ceil.into.ms
    T ~ msd.long.ms         ====  3497L       --: typed[Long]
    T ~ msd.floor.long.ms   ====  3497L       --: typed[Long]
    T ~ msd.round.long.ms   ====  3497L       --: typed[Long]
    T ~ msd.ceil.long.ms    ====  3498L       --: typed[Long]
    T ~ msu.long.ms         ====  3497L       --: typed[Long]
    T ~ msu.floor.long.ms   ====  3497L       --: typed[Long]
    T ~ msu.round.long.ms   ====  3498L       --: typed[Long]
    T ~ msu.ceil.long.ms    ====  3498L       --: typed[Long]
    T ~ mzu.long.ms         ==== -3497L       --: typed[Long]
    T ~ mzu.floor.long.ms   ==== -3498L       --: typed[Long]
    T ~ mzu.round.long.ms   ==== -3497L       --: typed[Long]
    T ~ mzu.ceil.long.ms    ==== -3497L       --: typed[Long]
    T ~ mzd.long.ms         ==== -3497L       --: typed[Long]
    T ~ mzd.floor.long.ms   ==== -3498L       --: typed[Long]
    T ~ mzd.round.long.ms   ==== -3498L       --: typed[Long]
    T ~ mzd.ceil.long.ms    ==== -3497L       --: typed[Long]
    T ~ msd.long.floor.ms   ==== msd.floor.long.ms
    T ~ msd.long.round.ms   ==== msd.round.long.ms
    T ~ msd.long.ceil.ms    ==== msd.ceil.long.ms
    T ~ msd.checked.ms        ====  3497L       --: typed[Long]
    T ~ msd.floor.checked.ms  ====  3497L       --: typed[Long]
    T ~ msd.round.checked.ms  ====  3497L       --: typed[Long]
    T ~ msd.ceil.checked.ms   ====  3498L       --: typed[Long]
    T ~ msu.checked.ms        ====  3497L       --: typed[Long]
    T ~ msu.floor.checked.ms  ====  3497L       --: typed[Long]
    T ~ msu.round.checked.ms  ====  3498L       --: typed[Long]
    T ~ msu.ceil.checked.ms   ====  3498L       --: typed[Long]
    T ~ mzu.checked.ms        ==== -3497L       --: typed[Long]
    T ~ mzu.floor.checked.ms  ==== -3498L       --: typed[Long]
    T ~ mzu.round.checked.ms  ==== -3497L       --: typed[Long]
    T ~ mzu.ceil.checked.ms   ==== -3497L       --: typed[Long]
    T ~ mzd.checked.ms        ==== -3497L       --: typed[Long]
    T ~ mzd.floor.checked.ms  ==== -3498L       --: typed[Long]
    T ~ mzd.round.checked.ms  ==== -3498L       --: typed[Long]
    T ~ mzd.ceil.checked.ms   ==== -3497L       --: typed[Long]
    T ~ msd.checked.floor.ms  ==== msd.floor.checked.ms
    T ~ msd.checked.round.ms  ==== msd.round.checked.ms
    T ~ msd.checked.ceil.ms   ==== msd.ceil.checked.ms
    T ~ dnms.checked.ms       ==== thrown[ArithmeticException]
    T ~ dpms.checked.ms       ==== thrown[ArithmeticException]
    T ~ dnms.floor.checked.ms ==== thrown[ArithmeticException]
    T ~ dpms.floor.checked.ms ==== thrown[ArithmeticException]
    T ~ dnms.round.checked.ms ==== thrown[ArithmeticException]
    T ~ dpms.round.checked.ms ==== thrown[ArithmeticException]
    T ~ dnms.ceil.checked.ms  ==== thrown[ArithmeticException]
    T ~ dpms.ceil.checked.ms  ==== thrown[ArithmeticException]

    val ssd = dd(3497.151)
    val ssu = dd(3497.515)
    val szu = dd(-3497.151)
    val szd = dd(-3497.515)
    val dnss = dd(Long.MinValue.toDouble.prev)
    val dpss = dd(Long.MaxValue.toDouble.next)
    T ~ ssd.trunc.s        ==== 3.497e3     --: typed[DoubleDuration]
    T ~ ssd.floor.s        ==== ssd.trunc.s --: typed[DoubleDuration]
    T ~ ssd.round.s        ==== ssd.trunc.s --: typed[DoubleDuration]
    T ~ ssd.ceil.s         ==== 3.498e3     --: typed[DoubleDuration]
    T ~ ssu.trunc.s        ==== ssd.floor.s --: typed[DoubleDuration]
    T ~ ssu.floor.s        ==== ssd.floor.s --: typed[DoubleDuration]
    T ~ ssu.round.s        ==== ssd.ceil.s  --: typed[DoubleDuration]
    T ~ ssu.ceil.s         ==== ssd.ceil.s  --: typed[DoubleDuration]
    T ~ szu.trunc.s        ==== -3.497e3    --: typed[DoubleDuration]
    T ~ szu.floor.s        ==== -3.498e3    --: typed[DoubleDuration]
    T ~ szu.round.s        ==== szu.trunc.s --: typed[DoubleDuration]
    T ~ szu.ceil.s         ==== szu.trunc.s --: typed[DoubleDuration]
    T ~ szd.trunc.s        ==== szu.trunc.s --: typed[DoubleDuration]
    T ~ szd.floor.s        ==== szu.floor.s --: typed[DoubleDuration]
    T ~ szd.round.s        ==== szu.floor.s --: typed[DoubleDuration]
    T ~ szd.ceil.s         ==== szu.trunc.s --: typed[DoubleDuration]
    T ~ ssd.into.s         =~~=  3497.151
    T ~ ssd.floor.into.s   ====  3.497e3    --: typed[Double]
    T ~ ssd.trunc.into.s   ====  3.497e3    --: typed[Double]
    T ~ ssd.round.into.s   ====  3.497e3    --: typed[Double]
    T ~ ssd.ceil.into.s    ====  3.498e3    --: typed[Double]
    T ~ ssu.into.s         =~~=  3497.515
    T ~ ssu.floor.into.s   ====  3.497e3    --: typed[Double]
    T ~ ssu.trunc.into.s   ====  3.497e3    --: typed[Double]
    T ~ ssu.round.into.s   ====  3.498e3    --: typed[Double]
    T ~ ssu.ceil.into.s    ====  3.498e3    --: typed[Double]
    T ~ szu.into.s         =~~= -3497.151
    T ~ szu.floor.into.s   ==== -3.498e3    --: typed[Double]
    T ~ szu.round.into.s   ==== -3.497e3    --: typed[Double]
    T ~ szu.ceil.into.s    ==== -3.497e3    --: typed[Double]
    T ~ szu.trunc.into.s   ==== -3.497e3    --: typed[Double]
    T ~ szd.into.s         =~~= -3497.515
    T ~ szd.floor.into.s   ==== -3.498e3    --: typed[Double]
    T ~ szd.round.into.s   ==== -3.498e3    --: typed[Double]
    T ~ szd.ceil.into.s    ==== -3.497e3    --: typed[Double]
    T ~ szd.trunc.into.s   ==== -3.497e3    --: typed[Double]
    T ~ ssd.into.trunc.s   ==== ssd.trunc.into.s
    T ~ ssd.into.floor.s   ==== ssd.floor.into.s
    T ~ ssd.into.round.s   ==== ssd.round.into.s
    T ~ ssd.into.ceil.s    ==== ssd.ceil.into.s
    T ~ ssd.long.s         ====  3497L      --: typed[Long]
    T ~ ssd.floor.long.s   ====  3497L      --: typed[Long]
    T ~ ssd.round.long.s   ====  3497L      --: typed[Long]
    T ~ ssd.ceil.long.s    ====  3498L      --: typed[Long]
    T ~ ssu.long.s         ====  3497L      --: typed[Long]
    T ~ ssu.floor.long.s   ====  3497L      --: typed[Long]
    T ~ ssu.round.long.s   ====  3498L      --: typed[Long]
    T ~ ssu.ceil.long.s    ====  3498L      --: typed[Long]
    T ~ szu.long.s         ==== -3497L      --: typed[Long]
    T ~ szu.floor.long.s   ==== -3498L      --: typed[Long]
    T ~ szu.round.long.s   ==== -3497L      --: typed[Long]
    T ~ szu.ceil.long.s    ==== -3497L      --: typed[Long]
    T ~ szd.long.s         ==== -3497L      --: typed[Long]
    T ~ szd.floor.long.s   ==== -3498L      --: typed[Long]
    T ~ szd.round.long.s   ==== -3498L      --: typed[Long]
    T ~ szd.ceil.long.s    ==== -3497L      --: typed[Long]
    T ~ ssd.long.floor.s   ==== ssd.floor.long.s
    T ~ ssd.long.round.s   ==== ssd.round.long.s
    T ~ ssd.long.ceil.s    ==== ssd.ceil.long.s
    T ~ ssd.checked.s        ====  3497L      --: typed[Long]
    T ~ ssd.floor.checked.s  ====  3497L      --: typed[Long]
    T ~ ssd.round.checked.s  ====  3497L      --: typed[Long]
    T ~ ssd.ceil.checked.s   ====  3498L      --: typed[Long]
    T ~ ssu.checked.s        ====  3497L      --: typed[Long]
    T ~ ssu.floor.checked.s  ====  3497L      --: typed[Long]
    T ~ ssu.round.checked.s  ====  3498L      --: typed[Long]
    T ~ ssu.ceil.checked.s   ====  3498L      --: typed[Long]
    T ~ szu.checked.s        ==== -3497L      --: typed[Long]
    T ~ szu.floor.checked.s  ==== -3498L      --: typed[Long]
    T ~ szu.round.checked.s  ==== -3497L      --: typed[Long]
    T ~ szu.ceil.checked.s   ==== -3497L      --: typed[Long]
    T ~ szd.checked.s        ==== -3497L      --: typed[Long]
    T ~ szd.floor.checked.s  ==== -3498L      --: typed[Long]
    T ~ szd.round.checked.s  ==== -3498L      --: typed[Long]
    T ~ szd.ceil.checked.s   ==== -3497L      --: typed[Long]
    T ~ ssd.checked.floor.s  ==== ssd.floor.checked.s
    T ~ ssd.checked.round.s  ==== ssd.round.checked.s
    T ~ ssd.checked.ceil.s   ==== ssd.ceil.checked.s
    T ~ dnss.checked.s       ==== thrown[ArithmeticException]
    T ~ dpss.checked.s       ==== thrown[ArithmeticException]
    T ~ dnss.floor.checked.s ==== thrown[ArithmeticException]
    T ~ dpss.floor.checked.s ==== thrown[ArithmeticException]
    T ~ dnss.round.checked.s ==== thrown[ArithmeticException]
    T ~ dpss.round.checked.s ==== thrown[ArithmeticException]
    T ~ dnss.ceil.checked.s  ==== thrown[ArithmeticException]
    T ~ dpss.ceil.checked.s  ==== thrown[ArithmeticException]

    val ksd = dd(209829.06)
    val ksu = dd(209850.90)
    val kzu = dd(-209829.06)
    val kzd = dd(-209850.90)
    val dnks = dd((Long.MinValue * 60.0).prev)
    val dpks = dd((Long.MaxValue * 60.0).next)
    T ~ ksd.trunc.m        ==== 20982e1     --: typed[DoubleDuration]
    T ~ ksd.floor.m        ==== ksd.trunc.m --: typed[DoubleDuration]
    T ~ ksd.round.m        ==== ksd.trunc.m --: typed[DoubleDuration]
    T ~ ksd.ceil.m         ==== 20988e1     --: typed[DoubleDuration]
    T ~ ksu.trunc.m        ==== ksd.floor.m --: typed[DoubleDuration]
    T ~ ksu.floor.m        ==== ksd.floor.m --: typed[DoubleDuration]
    T ~ ksu.round.m        ==== ksd.ceil.m  --: typed[DoubleDuration]
    T ~ ksu.ceil.m         ==== ksd.ceil.m  --: typed[DoubleDuration]
    T ~ kzu.trunc.m        ==== -20982e1    --: typed[DoubleDuration]
    T ~ kzu.floor.m        ==== -20988e1    --: typed[DoubleDuration]
    T ~ kzu.round.m        ==== kzu.trunc.m --: typed[DoubleDuration]
    T ~ kzu.ceil.m         ==== kzu.trunc.m --: typed[DoubleDuration]
    T ~ kzd.trunc.m        ==== kzu.trunc.m --: typed[DoubleDuration]
    T ~ kzd.floor.m        ==== kzu.floor.m --: typed[DoubleDuration]
    T ~ kzd.round.m        ==== kzu.floor.m --: typed[DoubleDuration]
    T ~ kzd.ceil.m         ==== kzu.trunc.m --: typed[DoubleDuration]
    T ~ ksd.into.m         =~~=  3497.151
    T ~ ksd.floor.into.m   ====  3.497e3    --: typed[Double]
    T ~ ksd.trunc.into.m   ====  3.497e3    --: typed[Double]
    T ~ ksd.round.into.m   ====  3.497e3    --: typed[Double]
    T ~ ksd.ceil.into.m    ====  3.498e3    --: typed[Double]
    T ~ ksu.into.m         =~~=  3497.515
    T ~ ksu.floor.into.m   ====  3.497e3    --: typed[Double]
    T ~ ksu.trunc.into.m   ====  3.497e3    --: typed[Double]
    T ~ ksu.round.into.m   ====  3.498e3    --: typed[Double]
    T ~ ksu.ceil.into.m    ====  3.498e3    --: typed[Double]
    T ~ kzu.into.m         =~~= -3497.151
    T ~ kzu.floor.into.m   ==== -3.498e3    --: typed[Double]
    T ~ kzu.round.into.m   ==== -3.497e3    --: typed[Double]
    T ~ kzu.ceil.into.m    ==== -3.497e3    --: typed[Double]
    T ~ kzu.trunc.into.m   ==== -3.497e3    --: typed[Double]
    T ~ kzd.into.m         =~~= -3497.515
    T ~ kzd.floor.into.m   ==== -3.498e3    --: typed[Double]
    T ~ kzd.round.into.m   ==== -3.498e3    --: typed[Double]
    T ~ kzd.ceil.into.m    ==== -3.497e3    --: typed[Double]
    T ~ kzd.trunc.into.m   ==== -3.497e3    --: typed[Double]
    T ~ ksd.into.trunc.m   ==== ksd.trunc.into.m
    T ~ ksd.into.floor.m   ==== ksd.floor.into.m
    T ~ ksd.into.round.m   ==== ksd.round.into.m
    T ~ ksd.into.ceil.m    ==== ksd.ceil.into.m
    T ~ ksd.long.m         ====  3497L      --: typed[Long]
    T ~ ksd.floor.long.m   ====  3497L      --: typed[Long]
    T ~ ksd.round.long.m   ====  3497L      --: typed[Long]
    T ~ ksd.ceil.long.m    ====  3498L      --: typed[Long]
    T ~ ksu.long.m         ====  3497L      --: typed[Long]
    T ~ ksu.floor.long.m   ====  3497L      --: typed[Long]
    T ~ ksu.round.long.m   ====  3498L      --: typed[Long]
    T ~ ksu.ceil.long.m    ====  3498L      --: typed[Long]
    T ~ kzu.long.m         ==== -3497L      --: typed[Long]
    T ~ kzu.floor.long.m   ==== -3498L      --: typed[Long]
    T ~ kzu.round.long.m   ==== -3497L      --: typed[Long]
    T ~ kzu.ceil.long.m    ==== -3497L      --: typed[Long]
    T ~ kzd.long.m         ==== -3497L      --: typed[Long]
    T ~ kzd.floor.long.m   ==== -3498L      --: typed[Long]
    T ~ kzd.round.long.m   ==== -3498L      --: typed[Long]
    T ~ kzd.ceil.long.m    ==== -3497L      --: typed[Long]
    T ~ ksd.long.floor.m   ==== ksd.floor.long.m
    T ~ ksd.long.round.m   ==== ksd.round.long.m
    T ~ ksd.long.ceil.m    ==== ksd.ceil.long.m
    T ~ ksd.checked.m        ====  3497L      --: typed[Long]
    T ~ ksd.floor.checked.m  ====  3497L      --: typed[Long]
    T ~ ksd.round.checked.m  ====  3497L      --: typed[Long]
    T ~ ksd.ceil.checked.m   ====  3498L      --: typed[Long]
    T ~ ksu.checked.m        ====  3497L      --: typed[Long]
    T ~ ksu.floor.checked.m  ====  3497L      --: typed[Long]
    T ~ ksu.round.checked.m  ====  3498L      --: typed[Long]
    T ~ ksu.ceil.checked.m   ====  3498L      --: typed[Long]
    T ~ kzu.checked.m        ==== -3497L      --: typed[Long]
    T ~ kzu.floor.checked.m  ==== -3498L      --: typed[Long]
    T ~ kzu.round.checked.m  ==== -3497L      --: typed[Long]
    T ~ kzu.ceil.checked.m   ==== -3497L      --: typed[Long]
    T ~ kzd.checked.m        ==== -3497L      --: typed[Long]
    T ~ kzd.floor.checked.m  ==== -3498L      --: typed[Long]
    T ~ kzd.round.checked.m  ==== -3498L      --: typed[Long]
    T ~ kzd.ceil.checked.m   ==== -3497L      --: typed[Long]
    T ~ ksd.checked.floor.m  ==== ksd.floor.checked.m
    T ~ ksd.checked.round.m  ==== ksd.round.checked.m
    T ~ ksd.checked.ceil.m   ==== ksd.ceil.checked.m
    T ~ dnks.checked.m       ==== thrown[ArithmeticException]
    T ~ dpks.checked.m       ==== thrown[ArithmeticException]
    T ~ dnks.floor.checked.m ==== thrown[ArithmeticException]
    T ~ dpks.floor.checked.m ==== thrown[ArithmeticException]
    T ~ dnks.round.checked.m ==== thrown[ArithmeticException]
    T ~ dpks.round.checked.m ==== thrown[ArithmeticException]
    T ~ dnks.ceil.checked.m  ==== thrown[ArithmeticException]
    T ~ dpks.ceil.checked.m  ==== thrown[ArithmeticException]

    val hsd = dd(12589743.6)
    val hsu = dd(12591054.0)
    val hzu = dd(-12589743.6)
    val hzd = dd(-12591054.0)
    val dnhs = dd((Long.MinValue * 3600.0).prev)
    val dphs = dd((Long.MaxValue * 3600.0).next)
    T ~ hsd.trunc.h        ==== 125892e2    --: typed[DoubleDuration]
    T ~ hsd.floor.h        ==== hsd.trunc.h --: typed[DoubleDuration]
    T ~ hsd.round.h        ==== hsd.trunc.h --: typed[DoubleDuration]
    T ~ hsd.ceil.h         ==== 125928e2    --: typed[DoubleDuration]
    T ~ hsu.trunc.h        ==== hsd.floor.h --: typed[DoubleDuration]
    T ~ hsu.floor.h        ==== hsd.floor.h --: typed[DoubleDuration]
    T ~ hsu.round.h        ==== hsd.ceil.h  --: typed[DoubleDuration]
    T ~ hsu.ceil.h         ==== hsd.ceil.h  --: typed[DoubleDuration]
    T ~ hzu.trunc.h        ==== -125892e2   --: typed[DoubleDuration]
    T ~ hzu.floor.h        ==== -125928e2    --: typed[DoubleDuration]
    T ~ hzu.round.h        ==== hzu.trunc.h --: typed[DoubleDuration]
    T ~ hzu.ceil.h         ==== hzu.trunc.h --: typed[DoubleDuration]
    T ~ hzd.trunc.h        ==== hzu.trunc.h --: typed[DoubleDuration]
    T ~ hzd.floor.h        ==== hzu.floor.h --: typed[DoubleDuration]
    T ~ hzd.round.h        ==== hzu.floor.h --: typed[DoubleDuration]
    T ~ hzd.ceil.h         ==== hzu.trunc.h --: typed[DoubleDuration]
    T ~ hsd.into.h         =~~=  3497.151
    T ~ hsd.floor.into.h   ====  3.497e3    --: typed[Double]
    T ~ hsd.trunc.into.h   ====  3.497e3    --: typed[Double]
    T ~ hsd.round.into.h   ====  3.497e3    --: typed[Double]
    T ~ hsd.ceil.into.h    ====  3.498e3    --: typed[Double]
    T ~ hsu.into.h         =~~=  3497.515
    T ~ hsu.floor.into.h   ====  3.497e3    --: typed[Double]
    T ~ hsu.trunc.into.h   ====  3.497e3    --: typed[Double]
    T ~ hsu.round.into.h   ====  3.498e3    --: typed[Double]
    T ~ hsu.ceil.into.h    ====  3.498e3    --: typed[Double]
    T ~ hzu.into.h         =~~= -3497.151
    T ~ hzu.floor.into.h   ==== -3.498e3    --: typed[Double]
    T ~ hzu.round.into.h   ==== -3.497e3    --: typed[Double]
    T ~ hzu.ceil.into.h    ==== -3.497e3    --: typed[Double]
    T ~ hzu.trunc.into.h   ==== -3.497e3    --: typed[Double]
    T ~ hzd.into.h         =~~= -3497.515
    T ~ hzd.floor.into.h   ==== -3.498e3    --: typed[Double]
    T ~ hzd.round.into.h   ==== -3.498e3    --: typed[Double]
    T ~ hzd.ceil.into.h    ==== -3.497e3    --: typed[Double]
    T ~ hzd.trunc.into.h   ==== -3.497e3    --: typed[Double]
    T ~ hsd.into.trunc.h   ==== hsd.trunc.into.h
    T ~ hsd.into.floor.h   ==== hsd.floor.into.h
    T ~ hsd.into.round.h   ==== hsd.round.into.h
    T ~ hsd.into.ceil.h    ==== hsd.ceil.into.h
    T ~ hsd.long.h         ====  3497L      --: typed[Long]
    T ~ hsd.floor.long.h   ====  3497L      --: typed[Long]
    T ~ hsd.round.long.h   ====  3497L      --: typed[Long]
    T ~ hsd.ceil.long.h    ====  3498L      --: typed[Long]
    T ~ hsu.long.h         ====  3497L      --: typed[Long]
    T ~ hsu.floor.long.h   ====  3497L      --: typed[Long]
    T ~ hsu.round.long.h   ====  3498L      --: typed[Long]
    T ~ hsu.ceil.long.h    ====  3498L      --: typed[Long]
    T ~ hzu.long.h         ==== -3497L      --: typed[Long]
    T ~ hzu.floor.long.h   ==== -3498L      --: typed[Long]
    T ~ hzu.round.long.h   ==== -3497L      --: typed[Long]
    T ~ hzu.ceil.long.h    ==== -3497L      --: typed[Long]
    T ~ hzd.long.h         ==== -3497L      --: typed[Long]
    T ~ hzd.floor.long.h   ==== -3498L      --: typed[Long]
    T ~ hzd.round.long.h   ==== -3498L      --: typed[Long]
    T ~ hzd.ceil.long.h    ==== -3497L      --: typed[Long]
    T ~ hsd.long.floor.h   ==== hsd.floor.long.h
    T ~ hsd.long.round.h   ==== hsd.round.long.h
    T ~ hsd.long.ceil.h    ==== hsd.ceil.long.h
    T ~ hsd.checked.h        ====  3497L      --: typed[Long]
    T ~ hsd.floor.checked.h  ====  3497L      --: typed[Long]
    T ~ hsd.round.checked.h  ====  3497L      --: typed[Long]
    T ~ hsd.ceil.checked.h   ====  3498L      --: typed[Long]
    T ~ hsu.checked.h        ====  3497L      --: typed[Long]
    T ~ hsu.floor.checked.h  ====  3497L      --: typed[Long]
    T ~ hsu.round.checked.h  ====  3498L      --: typed[Long]
    T ~ hsu.ceil.checked.h   ====  3498L      --: typed[Long]
    T ~ hzu.checked.h        ==== -3497L      --: typed[Long]
    T ~ hzu.floor.checked.h  ==== -3498L      --: typed[Long]
    T ~ hzu.round.checked.h  ==== -3497L      --: typed[Long]
    T ~ hzu.ceil.checked.h   ==== -3497L      --: typed[Long]
    T ~ hzd.checked.h        ==== -3497L      --: typed[Long]
    T ~ hzd.floor.checked.h  ==== -3498L      --: typed[Long]
    T ~ hzd.round.checked.h  ==== -3498L      --: typed[Long]
    T ~ hzd.ceil.checked.h   ==== -3497L      --: typed[Long]
    T ~ hsd.checked.floor.h  ==== hsd.floor.checked.h
    T ~ hsd.checked.round.h  ==== hsd.round.checked.h
    T ~ hsd.checked.ceil.h   ==== hsd.ceil.checked.h
    T ~ dnhs.checked.h       ==== thrown[ArithmeticException]
    T ~ dphs.checked.h       ==== thrown[ArithmeticException]
    T ~ dnhs.floor.checked.h ==== thrown[ArithmeticException]
    T ~ dphs.floor.checked.h ==== thrown[ArithmeticException]
    T ~ dnhs.round.checked.h ==== thrown[ArithmeticException]
    T ~ dphs.round.checked.h ==== thrown[ArithmeticException]
    T ~ dnhs.ceil.checked.h  ==== thrown[ArithmeticException]
    T ~ dphs.ceil.checked.h  ==== thrown[ArithmeticException]

    val ysd = dd(302153846.4)
    val ysu = dd(302185296.0)
    val yzu = dd(-302153846.4)
    val yzd = dd(-302185296.0)
    val dnys = dd((Long.MinValue * 86400.0).prev)
    val dpys = dd((Long.MaxValue * 86400.0).next)
    T ~ ysd.trunc.d          ==== 3021408e2           --: typed[DoubleDuration]
    T ~ ysd.floor.d          ==== ysd.trunc.d         --: typed[DoubleDuration]
    T ~ ysd.round.d          ==== ysd.trunc.d         --: typed[DoubleDuration]
    T ~ ysd.ceil.d           ==== 3022272e2           --: typed[DoubleDuration]
    T ~ ysu.trunc.d          ==== ysd.floor.d         --: typed[DoubleDuration]
    T ~ ysu.floor.d          ==== ysd.floor.d         --: typed[DoubleDuration]
    T ~ ysu.round.d          ==== ysd.ceil.d          --: typed[DoubleDuration]
    T ~ ysu.ceil.d           ==== ysd.ceil.d          --: typed[DoubleDuration]
    T ~ yzu.trunc.d          ==== -3021408e2          --: typed[DoubleDuration]
    T ~ yzu.floor.d          ==== -3022272e2          --: typed[DoubleDuration]
    T ~ yzu.round.d          ==== yzu.trunc.d         --: typed[DoubleDuration]
    T ~ yzu.ceil.d           ==== yzu.trunc.d         --: typed[DoubleDuration]
    T ~ yzd.trunc.d          ==== yzu.trunc.d         --: typed[DoubleDuration]
    T ~ yzd.floor.d          ==== yzu.floor.d         --: typed[DoubleDuration]
    T ~ yzd.round.d          ==== yzu.floor.d         --: typed[DoubleDuration]
    T ~ yzd.ceil.d           ==== yzu.trunc.d         --: typed[DoubleDuration]
    T ~ ysd.into.d           =~~=  3497.151
    T ~ ysd.floor.into.d     ====  3.497e3            --: typed[Double]
    T ~ ysd.trunc.into.d     ====  3.497e3            --: typed[Double]
    T ~ ysd.round.into.d     ====  3.497e3            --: typed[Double]
    T ~ ysd.ceil.into.d      ====  3.498e3            --: typed[Double]
    T ~ ysu.into.d           =~~=  3497.515
    T ~ ysu.floor.into.d     ====  3.497e3            --: typed[Double]
    T ~ ysu.trunc.into.d     ====  3.497e3            --: typed[Double]
    T ~ ysu.round.into.d     ====  3.498e3            --: typed[Double]
    T ~ ysu.ceil.into.d      ====  3.498e3            --: typed[Double]
    T ~ yzu.into.d           =~~= -3497.151
    T ~ yzu.floor.into.d     ==== -3.498e3            --: typed[Double]
    T ~ yzu.round.into.d     ==== -3.497e3            --: typed[Double]
    T ~ yzu.ceil.into.d      ==== -3.497e3            --: typed[Double]
    T ~ yzu.trunc.into.d     ==== -3.497e3            --: typed[Double]
    T ~ yzd.into.d           =~~= -3497.515
    T ~ yzd.floor.into.d     ==== -3.498e3            --: typed[Double]
    T ~ yzd.round.into.d     ==== -3.498e3            --: typed[Double]
    T ~ yzd.ceil.into.d      ==== -3.497e3            --: typed[Double]
    T ~ yzd.trunc.into.d     ==== -3.497e3            --: typed[Double]
    T ~ ysd.into.trunc.d     ==== ysd.trunc.into.d
    T ~ ysd.into.floor.d     ==== ysd.floor.into.d
    T ~ ysd.into.round.d     ==== ysd.round.into.d
    T ~ ysd.into.ceil.d      ==== ysd.ceil.into.d
    T ~ ysd.long.d           ====  3497L              --: typed[Long]
    T ~ ysd.floor.long.d     ====  3497L              --: typed[Long]
    T ~ ysd.round.long.d     ====  3497L              --: typed[Long]
    T ~ ysd.ceil.long.d      ====  3498L              --: typed[Long]
    T ~ ysu.long.d           ====  3497L              --: typed[Long]
    T ~ ysu.floor.long.d     ====  3497L              --: typed[Long]
    T ~ ysu.round.long.d     ====  3498L              --: typed[Long]
    T ~ ysu.ceil.long.d      ====  3498L              --: typed[Long]
    T ~ yzu.long.d           ==== -3497L              --: typed[Long]
    T ~ yzu.floor.long.d     ==== -3498L              --: typed[Long]
    T ~ yzu.round.long.d     ==== -3497L              --: typed[Long]
    T ~ yzu.ceil.long.d      ==== -3497L              --: typed[Long]
    T ~ yzd.long.d           ==== -3497L              --: typed[Long]
    T ~ yzd.floor.long.d     ==== -3498L              --: typed[Long]
    T ~ yzd.round.long.d     ==== -3498L              --: typed[Long]
    T ~ yzd.ceil.long.d      ==== -3497L              --: typed[Long]
    T ~ ysd.long.floor.d     ==== ysd.floor.long.d
    T ~ ysd.long.round.d     ==== ysd.round.long.d
    T ~ ysd.long.ceil.d      ==== ysd.ceil.long.d
    T ~ ysd.checked.d          ====  3497L              --: typed[Long]
    T ~ ysd.floor.checked.d    ====  3497L              --: typed[Long]
    T ~ ysd.round.checked.d    ====  3497L              --: typed[Long]
    T ~ ysd.ceil.checked.d     ====  3498L              --: typed[Long]
    T ~ ysu.checked.d          ====  3497L              --: typed[Long]
    T ~ ysu.floor.checked.d    ====  3497L              --: typed[Long]
    T ~ ysu.round.checked.d    ====  3498L              --: typed[Long]
    T ~ ysu.ceil.checked.d     ====  3498L              --: typed[Long]
    T ~ yzu.checked.d          ==== -3497L              --: typed[Long]
    T ~ yzu.floor.checked.d    ==== -3498L              --: typed[Long]
    T ~ yzu.round.checked.d    ==== -3497L              --: typed[Long]
    T ~ yzu.ceil.checked.d     ==== -3497L              --: typed[Long]
    T ~ yzd.checked.d          ==== -3497L              --: typed[Long]
    T ~ yzd.floor.checked.d    ==== -3498L              --: typed[Long]
    T ~ yzd.round.checked.d    ==== -3498L              --: typed[Long]
    T ~ yzd.ceil.checked.d     ==== -3497L              --: typed[Long]
    T ~ ysd.checked.floor.d    ==== ysd.floor.checked.d
    T ~ ysd.checked.round.d    ==== ysd.round.checked.d
    T ~ ysd.checked.ceil.d     ==== ysd.ceil.checked.d
    T ~ dnys.checked.d         ==== thrown[ArithmeticException]
    T ~ dpys.checked.d         ==== thrown[ArithmeticException]
    T ~ dnys.floor.checked.d   ==== thrown[ArithmeticException]
    T ~ dpys.floor.checked.d   ==== thrown[ArithmeticException]
    T ~ dnys.round.checked.d   ==== thrown[ArithmeticException]
    T ~ dpys.round.checked.d   ==== thrown[ArithmeticException]
    T ~ dnys.ceil.checked.d    ==== thrown[ArithmeticException]
    T ~ dpys.ceil.checked.d    ==== thrown[ArithmeticException]
    T ~ ysu.trunc.days       ==== ysu.trunc.d         --: typed[DoubleDuration]
    T ~ ysu.floor.days       ==== ysu.floor.d         --: typed[DoubleDuration]
    T ~ ysu.round.days       ==== ysu.round.d         --: typed[DoubleDuration]
    T ~ ysu.ceil.days        ==== ysu.ceil.d          --: typed[DoubleDuration]
    T ~ ysu.into.days        ==== ysu.into.d          --: typed[Double]
    T ~ ysu.trunc.into.days  ==== ysu.trunc.into.d  --: typed[Double]
    T ~ ysu.floor.into.days  ==== ysu.floor.into.d  --: typed[Double]
    T ~ ysu.round.into.days  ==== ysu.round.into.d  --: typed[Double]
    T ~ ysu.ceil.into.days   ==== ysu.ceil.into.d   --: typed[Double]
    T ~ ysu.long.days        ==== ysu.long.d        --: typed[Long]
    T ~ ysu.floor.long.days  ==== ysu.floor.long.d  --: typed[Long]
    T ~ ysu.round.long.days  ==== ysu.round.long.d  --: typed[Long]
    T ~ ysu.ceil.long.days   ==== ysu.ceil.long.d   --: typed[Long]
    T ~ ysu.checked.days       ==== ysu.checked.d       --: typed[Long]
    T ~ ysu.floor.checked.days ==== ysu.floor.checked.d --: typed[Long]
    T ~ ysu.round.checked.days ==== ysu.round.checked.d --: typed[Long]
    T ~ ysu.ceil.checked.days  ==== ysu.ceil.checked.d  --: typed[Long]

    T ~ List(ysd, yzu, yzd, ysu).sorted ==== List(yzd, yzu, ysd, ysu)
    T ~ nsd.pr ==== "3.481957197151 sec"


  def testNanoInstant(): Unit =
    val t = NanoInstant.now
    val ts = System.nanoTime

    val i = NanoInstant(912835798134L)
    val j: NanoInstant = i + 5.ns_nano
    val k = NanoInstant(Long.MaxValue - 2)
    val h = NanoInstant(Long.MinValue + 2)
    T ~ t               ==== t             --: typed[NanoInstant]
    T ~ i               ==== 912835798134L --: typed[NanoInstant]
    T ~ j               ==== 912835798139L
    T ~ (5.ns_nano + i) ==== j             --: typed[NanoInstant]
    T ~ (k + 5.ns_nano) ==== h             --: typed[NanoInstant]
    T ~ (j - 5.ns_nano) ==== i             --: typed[NanoInstant]
    T ~ (j - i)         ==== 5.ns_nano     --: typed[NanoDuration]
    T ~ (i - j)         ==== -5.ns_nano
    T ~ (h - k)         ==== 5.ns_nano
    T ~ (i to j)        ==== (j - i)       --: typed[NanoDuration]
    T ~ (k to h)        ==== (h - k)
    T ~ (i < j)  ==== true
    T ~ (i < i)  ==== false
    T ~ (j < i)  ==== false
    T ~ (k < h)  ==== true
    T ~ (k < k)  ==== false
    T ~ (h < k)  ==== false
    T ~ (i <= j) ==== true
    T ~ (i <= i) ==== true
    T ~ (j <= i) ==== false
    T ~ (k <= h) ==== true
    T ~ (k <= k) ==== true
    T ~ (h <= k) ==== false
    T ~ (i >= j) ==== false
    T ~ (i >= i) ==== true
    T ~ (j >= i) ==== true
    T ~ (k >= h) ==== false
    T ~ (k >= k) ==== true
    T ~ (h >= k) ==== true
    T ~ (i > j)  ==== false
    T ~ (i > i)  ==== false
    T ~ (j > i)  ==== true
    T ~ (k > h)  ==== false
    T ~ (k > k)  ==== false
    T ~ (h > k)  ==== true
    T ~ (i max j)       ==== j      --: typed[NanoInstant]
    T ~ (j max i)       ==== j
    T ~ (k max h)       ==== h
    T ~ (h max k)       ==== h
    T ~ (i min j)       ==== i      --: typed[NanoInstant]
    T ~ (j min i)       ==== i
    T ~ (k min h)       ==== k
    T ~ (h min k)       ==== k
    T ~ k.clamp(j, h)   ==== k
    T ~ j.clamp(k, h)   ==== k
    T ~ h.clamp(j, k)   ==== k
    T ~ j.clamp(h, k)   ==== h
    T ~ k.in(j, h)      ==== true
    T ~ j.in(k, h)      ==== false
    T ~ h.in(j, k)      ==== false
    T ~ k.checkIn(j, h) ==== k
    T ~ j.checkIn(k, h) ==== thrown[ArithmeticException]
    T ~ h.checkIn(j, k) ==== thrown[ArithmeticException]

    val ts2 = System.nanoTime
    val dt = t.age
    T ~ (ts2 - ts <= dt.unwrap) ==== true
    T ~ dt                      ==== dt  --: typed[NanoDuration]
    T ~ i.pr                    ==== "nanotime=912835798134"
    T ~ List(j, k, h, i).sorted ==== List(i, j, k, h)

  def testDoubleInstant(): Unit =
    val t = DoubleInstant.now
    val nineG = DoubleInstant(1e9)
    val ti = Instant.now
    val tp = t + 5.0.ms
    val tm = t - 5.0.ms

    T ~ DoubleInstant(1e9.next)                ==== 1e9.next         --: typed[DoubleInstant]
    T ~ DoubleInstant(1e9.next).unwrap         ==== 1e9.next         --: typed[Double]
    T ~ (DoubleInstant(ti).unwrap >= t.unwrap) ==== true
    T ~ DoubleInstant.fromSeconds(5, 195215)   ==== 5.000195215      --: typed[DoubleInstant]
    T ~ (nineG + 5.0.ms)                       ==== 1.000000000005e9 --: typed[DoubleInstant]
    T ~ (nineG - 5.0.ms)                       ==== 0.999999999995e9 --: typed[DoubleInstant]
    T ~ (t - nineG)                            ==== (t.unwrap - 1e9) --: typed[DoubleDuration]
    T ~ (nineG to t)                           ==== (t - nineG)      --: typed[DoubleDuration]
    T ~ (nineG <  t) ==== true
    T ~ (nineG <= t) ==== true
    T ~ (nineG >= t) ==== false
    T ~ (nineG >  t) ==== false
    T ~ (t <  t)     ==== false
    T ~ (t <= t)     ==== true
    T ~ (t >= t)     ==== true
    T ~ (t >  t)     ==== false
    T ~ (t <  nineG) ==== false
    T ~ (t <= nineG) ==== false
    T ~ (t >= nineG) ==== true
    T ~ (t >  nineG) ==== true
    T ~ (t max nineG)     ==== t     --: typed[DoubleInstant]
    T ~ (nineG max t)     ==== t
    T ~ (t min nineG)     ==== nineG --: typed[DoubleInstant]
    T ~ (nineG min t)     ==== nineG
    T ~ t.clamp(tm, tp)   ==== t     --: typed[DoubleInstant]
    T ~ tm.clamp(t, tp)   ==== t
    T ~ tp.clamp(tm, t)   ==== t
    T ~ t.clamp(tp, tm)   ==== tp
    T ~ t.in(tm, tp)      ==== true
    T ~ tm.in(t, tp)      ==== false
    T ~ tp.in(tm, t)      ==== false
    T ~ t.checkIn(tm, tp) ==== t     --: typed[DoubleInstant]
    T ~ tm.checkIn(t, tp) ==== thrown[ArithmeticException]
    T ~ tp.checkIn(tm, t) ==== thrown[ArithmeticException]

    var ti2 = Instant.now
    while ti == ti2 do { Thread.sleep(1); ti2 = Instant.now }
    val dt = t.age
    T ~ (dt >= DoubleInstant(ti2) - DoubleInstant(ti)) ==== true

    T ~ nineG.instant                              ==== Instant.ofEpochSecond(1000000000) --: typed[Instant]
    T ~ DoubleInstant(1234567890.1234562).instant  ==== Instant.ofEpochSecond(1234567890L, 123456000)
    T ~ DoubleInstant(12345678901.234562).instant  ==== Instant.ofEpochSecond(12345678901L, 234560000)
    T ~ DoubleInstant(123456789012.34562).instant  ==== Instant.ofEpochSecond(123456789012L, 345600000)
    T ~ DoubleInstant(1234567890123.4562).instant  ==== Instant.ofEpochSecond(1234567890123L, 456000000)
    T ~ DoubleInstant(12345678901234.562).instant  ==== Instant.ofEpochSecond(12345678901234L, 560000000)
    T ~ DoubleInstant(123456789012345.62).instant  ==== Instant.ofEpochSecond(123456789012345L, 600000000)
    T ~ DoubleInstant(1234567890123456.2).instant  ==== Instant.ofEpochSecond(1234567890123456L)
    T ~ DoubleInstant(12345678901234562.0).instant ==== Instant.ofEpochSecond(12345678901234560L)
    T ~ DoubleInstant(23456789012345672.0).instant ==== Instant.ofEpochSecond(23456789012345640L)
    T ~ DoubleInstant(12345678901234562e1).instant ==== Instant.MAX
    T ~ DoubleInstant(-1e20).instant               ==== Instant.MIN
    T ~ nineG.checkedInstant                     ==== nineG.instant --: typed[Instant]
    T ~ DoubleInstant(3.2e16).checkedInstant     ==== thrown[DateTimeException]
    T ~ DoubleInstant(-1e20).checkedInstant      ==== thrown[DateTimeException]
    T ~ DoubleInstant(Double.NaN).checkedInstant ==== thrown[DateTimeException]

    T ~ nineG.filetime                              ==== FileTime.fromMillis(1000000000000L)  --: typed[FileTime]
    T ~ DoubleInstant(1234567890.1234562).filetime  ==== FileTime.from(1234567890123456L, TimeUnit.MICROSECONDS)
    T ~ DoubleInstant(23456789012345672.0).filetime ==== FileTime.from(Instant.ofEpochSecond(23456789012345640L))
    T ~ DoubleInstant(56890123456789012.0).filetime ==== FileTime.from(948168724279816L, TimeUnit.MINUTES)
    T ~ DoubleInstant(56890123456789012e1).filetime ==== FileTime.from(158028120713302L, TimeUnit.HOURS)
    T ~ DoubleInstant(56890123456789012e2).filetime ==== FileTime.from(65845050297209L, TimeUnit.DAYS)
    T ~ DoubleInstant(56890123456789012e7).filetime ==== FileTime.from(6584505029720950784L, TimeUnit.DAYS)
    T ~ DoubleInstant(56890123456789012e8).filetime ==== TemporalCompanion.FileTimeMax
    T ~ DoubleInstant(-1e24).filetime               ==== TemporalCompanion.FileTimeMin
    T ~ DoubleInstant(t.filetime)                   ==== typed[DoubleInstant]
    T ~ DoubleInstant(t.filetime).unwrap            =~~= t.unwrap
    T ~ DoubleInstant(DoubleInstant( 56890123456789012e1).filetime).unwrap =~~=  56890123456789012e1
    T ~ DoubleInstant(DoubleInstant(-56890123456789012e1).filetime).unwrap =~~= -56890123456789012e1
    T ~ nineG.checkedFileTime                              ==== nineG.filetime --: typed[FileTime]
    T ~ DoubleInstant(56890123456789012e8).checkedFileTime ==== thrown[DateTimeException]
    T ~ DoubleInstant(-1e24).checkedFileTime               ==== thrown[DateTimeException]
    T ~ DoubleInstant(Double.NaN).checkedFileTime          ==== thrown[DateTimeException]

    val zoff = ZoneId.systemDefault.getRules.getOffset(nineG.instant)
    T ~ (1.m > Duration.between(t.local, LocalDateTime.now).abs) ==== true
    T ~ nineG.local                                 ==== LocalDateTime.ofEpochSecond(1000000000, 0, zoff) --: typed[LocalDateTime]
    T ~ (DoubleInstant(Instant.MAX) - 24.0.h).local ==== LocalDateTime.MAX
    T ~ (DoubleInstant(Instant.MIN) + 24.0.h).local ==== LocalDateTime.MIN
    T ~ nineG.checkedLocal                                 ==== nineG.local --: typed[LocalDateTime]
    T ~ (DoubleInstant(Instant.MAX) - 24.0.h).checkedLocal ==== thrown[DateTimeException]
    T ~ (DoubleInstant(Instant.MIN) + 24.0.h).checkedLocal ==== thrown[DateTimeException]

    T ~ (1.m > Duration.between(t.offset, OffsetDateTime.now).abs) ==== true
    T ~ nineG.offset                                 ==== OffsetDateTime.ofInstant(nineG.instant, zoff) --: typed[OffsetDateTime]
    T ~ (DoubleInstant(Instant.MAX) - 24.0.h).offset ==== TemporalCompanion.currentMaxOffsetDateTime
    T ~ (DoubleInstant(Instant.MIN) + 24.0.h).offset ==== TemporalCompanion.currentMinOffsetDateTime
    T ~ nineG.checkedOffset                                 ==== nineG.offset --: typed[OffsetDateTime]
    T ~ (DoubleInstant(Instant.MAX) - 24.0.h).checkedOffset ==== thrown[DateTimeException]
    T ~ (DoubleInstant(Instant.MIN) + 24.0.h).checkedOffset ==== thrown[DateTimeException]

    T ~ (1.m > Duration.between(t.utc, OffsetDateTime.now).abs) ==== true
    T ~ nineG.utc                                 ==== OffsetDateTime.ofInstant(nineG.instant, ZoneOffset.UTC) --: typed[OffsetDateTime]
    T ~ (DoubleInstant(Instant.MAX) - 24.0.h).utc ==== TemporalCompanion.MaxUTCDateTime
    T ~ (DoubleInstant(Instant.MIN) + 24.0.h).utc ==== TemporalCompanion.MinUTCDateTime
    T ~ nineG.checkedUTC                                 ==== nineG.utc --: typed[OffsetDateTime]
    T ~ (DoubleInstant(Instant.MAX) - 24.0.h).checkedUTC ==== thrown[DateTimeException]
    T ~ (DoubleInstant(Instant.MIN) + 24.0.h).checkedUTC ==== thrown[DateTimeException]

    T ~ (1.m > Duration.between(t.zoned, ZonedDateTime.now)) ==== true
    T ~ nineG.zoned                                 ==== ZonedDateTime.ofInstant(nineG.instant, ZoneId.systemDefault) --: typed[ZonedDateTime]
    T ~ (DoubleInstant(Instant.MAX) - 24.0.h).zoned ==== TemporalCompanion.currentMaxZonedDateTime
    T ~ (DoubleInstant(Instant.MIN) + 24.0.h).zoned ==== TemporalCompanion.currentMinZonedDateTime
    T ~ nineG.checkedZoned                                 ==== nineG.zoned --: typed[ZonedDateTime]
    T ~ (DoubleInstant(Instant.MAX) - 24.0.h).checkedZoned ==== thrown[DateTimeException]
    T ~ (DoubleInstant(Instant.MIN) + 24.0.h).checkedZoned ==== thrown[DateTimeException]

    val dipu = DoubleInstant(1234567891.6786787)
    val dipd = DoubleInstant(1234516283.4324323)
    val dinu = DoubleInstant(-dipd.unwrap)
    val dind = DoubleInstant(-dipu.unwrap)
    T ~ dipu.trunc.us ====  1234567891.678678  --: typed[DoubleInstant]
    T ~ dipu.floor.us ====  1234567891.678678  --: typed[DoubleInstant]
    T ~ dipu.round.us ====  1234567891.678679  --: typed[DoubleInstant]
    T ~ dipu.ceil.us  ====  1234567891.678679  --: typed[DoubleInstant]
    T ~ dipd.trunc.us ====  1234516283.432432  --: typed[DoubleInstant]
    T ~ dipd.floor.us ====  1234516283.432432  --: typed[DoubleInstant]
    T ~ dipd.round.us ====  1234516283.432432  --: typed[DoubleInstant]
    T ~ dipd.ceil.us  ====  1234516283.432433  --: typed[DoubleInstant]
    T ~ dinu.trunc.us ==== -1234516283.432432  --: typed[DoubleInstant]
    T ~ dinu.floor.us ==== -1234516283.432433  --: typed[DoubleInstant]
    T ~ dinu.round.us ==== -1234516283.432432  --: typed[DoubleInstant]
    T ~ dinu.ceil.us  ==== -1234516283.432432  --: typed[DoubleInstant]
    T ~ dind.trunc.us ==== -1234567891.678678  --: typed[DoubleInstant]
    T ~ dind.floor.us ==== -1234567891.678679  --: typed[DoubleInstant]
    T ~ dind.round.us ==== -1234567891.678679  --: typed[DoubleInstant]
    T ~ dind.ceil.us  ==== -1234567891.678678  --: typed[DoubleInstant]
    T ~ dipu.trunc.ms ====  1234567891.678     --: typed[DoubleInstant]
    T ~ dipu.floor.ms ====  1234567891.678     --: typed[DoubleInstant]
    T ~ dipu.round.ms ====  1234567891.679     --: typed[DoubleInstant]
    T ~ dipu.ceil.ms  ====  1234567891.679     --: typed[DoubleInstant]
    T ~ dipd.trunc.ms ====  1234516283.432     --: typed[DoubleInstant]
    T ~ dipd.floor.ms ====  1234516283.432     --: typed[DoubleInstant]
    T ~ dipd.round.ms ====  1234516283.432     --: typed[DoubleInstant]
    T ~ dipd.ceil.ms  ====  1234516283.433     --: typed[DoubleInstant]
    T ~ dinu.trunc.ms ==== -1234516283.432     --: typed[DoubleInstant]
    T ~ dinu.floor.ms ==== -1234516283.433     --: typed[DoubleInstant]
    T ~ dinu.round.ms ==== -1234516283.432     --: typed[DoubleInstant]
    T ~ dinu.ceil.ms  ==== -1234516283.432     --: typed[DoubleInstant]
    T ~ dind.trunc.ms ==== -1234567891.678     --: typed[DoubleInstant]
    T ~ dind.floor.ms ==== -1234567891.679     --: typed[DoubleInstant]
    T ~ dind.round.ms ==== -1234567891.679     --: typed[DoubleInstant]
    T ~ dind.ceil.ms  ==== -1234567891.678     --: typed[DoubleInstant]
    T ~ dipu.trunc.s  ====  1234567891.0       --: typed[DoubleInstant]
    T ~ dipu.floor.s  ====  1234567891.0       --: typed[DoubleInstant]
    T ~ dipu.round.s  ====  1234567892.0       --: typed[DoubleInstant]
    T ~ dipu.ceil.s   ====  1234567892.0       --: typed[DoubleInstant]
    T ~ dipd.trunc.s  ====  1234516283.0       --: typed[DoubleInstant]
    T ~ dipd.floor.s  ====  1234516283.0       --: typed[DoubleInstant]
    T ~ dipd.round.s  ====  1234516283.0       --: typed[DoubleInstant]
    T ~ dipd.ceil.s   ====  1234516284.0       --: typed[DoubleInstant]
    T ~ dinu.trunc.s  ==== -1234516283.0       --: typed[DoubleInstant]
    T ~ dinu.floor.s  ==== -1234516284.0       --: typed[DoubleInstant]
    T ~ dinu.round.s  ==== -1234516283.0       --: typed[DoubleInstant]
    T ~ dinu.ceil.s   ==== -1234516283.0       --: typed[DoubleInstant]
    T ~ dind.trunc.s  ==== -1234567891.0       --: typed[DoubleInstant]
    T ~ dind.floor.s  ==== -1234567892.0       --: typed[DoubleInstant]
    T ~ dind.round.s  ==== -1234567892.0       --: typed[DoubleInstant]
    T ~ dind.ceil.s   ==== -1234567891.0       --: typed[DoubleInstant]
    T ~ dipu.trunc.m  ====  1234567860.0       --: typed[DoubleInstant]
    T ~ dipu.floor.m  ====  1234567860.0       --: typed[DoubleInstant]
    T ~ dipu.round.m  ====  1234567920.0       --: typed[DoubleInstant]
    T ~ dipu.ceil.m   ====  1234567920.0       --: typed[DoubleInstant]
    T ~ dipd.trunc.m  ====  1234516260.0       --: typed[DoubleInstant]
    T ~ dipd.floor.m  ====  1234516260.0       --: typed[DoubleInstant]
    T ~ dipd.round.m  ====  1234516260.0       --: typed[DoubleInstant]
    T ~ dipd.ceil.m   ====  1234516320.0       --: typed[DoubleInstant]
    T ~ dinu.trunc.m  ==== -1234516260.0       --: typed[DoubleInstant]
    T ~ dinu.floor.m  ==== -1234516320.0       --: typed[DoubleInstant]
    T ~ dinu.round.m  ==== -1234516260.0       --: typed[DoubleInstant]
    T ~ dinu.ceil.m   ==== -1234516260.0       --: typed[DoubleInstant]
    T ~ dind.trunc.m  ==== -1234567860.0       --: typed[DoubleInstant]
    T ~ dind.floor.m  ==== -1234567920.0       --: typed[DoubleInstant]
    T ~ dind.round.m  ==== -1234567920.0       --: typed[DoubleInstant]
    T ~ dind.ceil.m   ==== -1234567860.0       --: typed[DoubleInstant]
    T ~ dipu.trunc.h  ====  1234566000.0       --: typed[DoubleInstant]
    T ~ dipu.floor.h  ====  1234566000.0       --: typed[DoubleInstant]
    T ~ dipu.round.h  ====  1234569600.0       --: typed[DoubleInstant]
    T ~ dipu.ceil.h   ====  1234569600.0       --: typed[DoubleInstant]
    T ~ dipd.trunc.h  ====  1234515600.0       --: typed[DoubleInstant]
    T ~ dipd.floor.h  ====  1234515600.0       --: typed[DoubleInstant]
    T ~ dipd.round.h  ====  1234515600.0       --: typed[DoubleInstant]
    T ~ dipd.ceil.h   ====  1234519200.0       --: typed[DoubleInstant]
    T ~ dinu.trunc.h  ==== -1234515600.0       --: typed[DoubleInstant]
    T ~ dinu.floor.h  ==== -1234519200.0       --: typed[DoubleInstant]
    T ~ dinu.round.h  ==== -1234515600.0       --: typed[DoubleInstant]
    T ~ dinu.ceil.h   ==== -1234515600.0       --: typed[DoubleInstant]
    T ~ dind.trunc.h  ==== -1234566000.0       --: typed[DoubleInstant]
    T ~ dind.floor.h  ==== -1234569600.0       --: typed[DoubleInstant]
    T ~ dind.round.h  ==== -1234569600.0       --: typed[DoubleInstant]
    T ~ dind.ceil.h   ==== -1234566000.0       --: typed[DoubleInstant]
    T ~ dipu.trunc.d  ====  1234483200.0       --: typed[DoubleInstant]
    T ~ dipu.floor.d  ====  1234483200.0       --: typed[DoubleInstant]
    T ~ dipu.round.d  ====  1234569600.0       --: typed[DoubleInstant]
    T ~ dipu.ceil.d   ====  1234569600.0       --: typed[DoubleInstant]
    T ~ dipd.trunc.d  ====  1234483200.0       --: typed[DoubleInstant]
    T ~ dipd.floor.d  ====  1234483200.0       --: typed[DoubleInstant]
    T ~ dipd.round.d  ====  1234483200.0       --: typed[DoubleInstant]
    T ~ dipd.ceil.d   ====  1234569600.0       --: typed[DoubleInstant]
    T ~ dinu.trunc.d  ==== -1234483200.0       --: typed[DoubleInstant]
    T ~ dinu.floor.d  ==== -1234569600.0       --: typed[DoubleInstant]
    T ~ dinu.round.d  ==== -1234483200.0       --: typed[DoubleInstant]
    T ~ dinu.ceil.d   ==== -1234483200.0       --: typed[DoubleInstant]
    T ~ dind.trunc.d  ==== -1234483200.0       --: typed[DoubleInstant]
    T ~ dind.floor.d  ==== -1234569600.0       --: typed[DoubleInstant]
    T ~ dind.round.d  ==== -1234569600.0       --: typed[DoubleInstant]
    T ~ dind.ceil.d   ==== -1234483200.0       --: typed[DoubleInstant]
    T ~ dipu.trunc.days ==== dipu.trunc.d      --: typed[DoubleInstant]
    T ~ dipu.floor.days ==== dipu.floor.d      --: typed[DoubleInstant]
    T ~ dipu.round.days ==== dipu.round.d      --: typed[DoubleInstant]
    T ~ dipu.ceil.days  ==== dipu.ceil.d       --: typed[DoubleInstant]

    T ~ List(dinu, dipu, dind, dipd).sorted ==== List(dind, dinu, dipd, dipu)


  def testInstant(): Unit =
    val i = Instant.now
    val ldt = LocalDateTime.now
    val odt = OffsetDateTime.now
    val zdt = ZonedDateTime.now
    val gig = Instant.ofEpochSecond(1000000000)
    val giggle = Instant.ofEpochSecond(1000000001, 234567890)
    val ogg = gig.offset
    val oggle = giggle.offset
    val big = Duration.between(gig, Instant.MAX)
    val bigger = Duration.between(Instant.MIN, giggle)
    T ~ (gig + 1.s)     ==== Instant.ofEpochSecond(1000000001) --: typed[Instant]
    T ~ (giggle + big)  ==== Instant.MAX
    T ~ (gig +! 1.s)    ==== (gig + 1.s) --: typed[Instant]
    T ~ (giggle +! big) ==== thrown[DateTimeException]
    T ~ (gig - 1.s)     ==== Instant.ofEpochSecond(999999999)  --: typed[Instant]
    T ~ (gig - bigger)  ==== Instant.MIN
    T ~ (gig -! 1.s)    ==== (gig - 1.s)                       --: typed[Instant]
    T ~ (gig -! bigger) ==== thrown[DateTimeException]
    T ~ (giggle - gig)  ==== 1234567890.ns                     --: typed[Duration]
    T ~ (giggle - ogg)  ==== (giggle - gig)                    --: typed[Duration]
    T ~ (giggle to gig) ==== -1234567890.ns                    --: typed[Duration]
    T ~ (giggle to ogg) ==== (giggle to gig)                   --: typed[Duration]
    T ~ (gig < giggle)  ==== true
    T ~ (gig < gig)     ==== false
    T ~ (giggle < gig)  ==== false
    T ~ (gig <= giggle) ==== true
    T ~ (gig <= gig)    ==== true
    T ~ (giggle <= gig) ==== false
    T ~ (gig >= giggle) ==== false
    T ~ (gig >= gig)    ==== true
    T ~ (giggle >= gig) ==== true
    T ~ (gig > giggle)  ==== false
    T ~ (gig > gig)     ==== false 
    T ~ (giggle > gig)  ==== true
    T ~ (gig < oggle)   ==== (gig < giggle)
    T ~ (gig <= oggle)  ==== (gig <= giggle)
    T ~ (gig >= oggle)  ==== (gig >= giggle)
    T ~ (gig > oggle)   ==== (gig > giggle)
    T ~ (gig max giggle)                 ==== giggle      --: typed[Instant]
    T ~ (giggle max gig)                 ==== giggle
    T ~ (gig min giggle)                 ==== gig         --: typed[Instant]
    T ~ (giggle min gig)                 ==== gig
    T ~ (gig + 1.s).clamp(gig, giggle)   ==== (gig + 1.s) --: typed[Instant]
    T ~ gig.clamp(gig + 1.s, giggle)     ==== (gig + 1.s)
    T ~ giggle.clamp(gig, gig + 1.s)     ==== (gig + 1.s)
    T ~ (gig + 1.s).clamp(giggle, gig)   ==== giggle
    T ~ (gig + 1.s).in(gig, giggle)      ==== true
    T ~ gig.in(gig + 1.s, giggle)        ==== false
    T ~ giggle.in(gig, gig + 1.s)        ==== false
    T ~ (gig + 1.s).checkIn(gig, giggle) ==== (gig + 1.s) --: typed[Instant]
    T ~ gig.checkIn(gig + 1.s, giggle)   ==== thrown[DateTimeException]
    T ~ giggle.checkIn(gig, gig + 1.s)   ==== thrown[DateTimeException]
    T ~ i.age                            ==== typed[Duration]
    T ~ (1.m > i.age)                    ==== true
    T ~ gig.D                                   ==== 1e9 --: typed[DoubleInstant]
    T ~ gig.filetime                            ==== FileTime.fromMillis(1000000000000L) --: typed[FileTime]
    T ~ gig.local                               ==== typed[LocalDateTime]
    T ~ (Duration.between(i.local, ldt) < 1.m)  ==== true
    T ~ Instant.MAX.local                       ==== LocalDateTime.MAX
    T ~ Instant.MIN.local                       ==== LocalDateTime.MIN
    T ~ gig.checkedLocal                        ==== gig.local   --: typed[LocalDateTime]
    T ~ Instant.MAX.checkedLocal                ==== thrown[DateTimeException]
    T ~ Instant.MIN.checkedLocal                ==== thrown[DateTimeException]
    T ~ gig.offset                              ==== typed[OffsetDateTime]
    T ~ (Duration.between(i.offset, odt) < 1.m) ==== true
    T ~ Instant.MAX.offset                      ==== LocalDateTime.MAX.atOffset(ZoneId.systemDefault.getRules.getOffset(LocalDateTime.MAX))
    T ~ Instant.MIN.offset                      ==== LocalDateTime.MIN.atOffset(ZoneId.systemDefault.getRules.getOffset(LocalDateTime.MIN))
    T ~ gig.checkedOffset                       ==== gig.offset   --: typed[OffsetDateTime]
    T ~ Instant.MAX.checkedOffset               ==== thrown[DateTimeException]
    T ~ Instant.MIN.checkedOffset               ==== thrown[DateTimeException]
    T ~ gig.utc                                 ==== typed[OffsetDateTime]
    T ~ (Duration.between(i.utc, odt) < 1.m)    ==== true
    T ~ Instant.MAX.utc                         ==== LocalDateTime.MAX.atOffset(ZoneOffset.UTC)
    T ~ Instant.MIN.utc                         ==== LocalDateTime.MIN.atOffset(ZoneOffset.UTC)
    T ~ gig.checkedUTC                          ==== gig.utc   --: typed[OffsetDateTime]
    T ~ Instant.MAX.checkedUTC                  ==== thrown[DateTimeException]
    T ~ Instant.MIN.checkedUTC                  ==== thrown[DateTimeException]
    T ~ gig.zoned                               ==== typed[ZonedDateTime]
    T ~ (Duration.between(i.zoned, zdt) < 1.m)  ==== true
    T ~ Instant.MAX.zoned                       ==== LocalDateTime.MAX.atZone(ZoneId.systemDefault)
    T ~ Instant.MIN.zoned                       ==== LocalDateTime.MIN.atZone(ZoneId.systemDefault)
    T ~ gig.checkedZoned                        ==== gig.zoned   --: typed[ZonedDateTime]
    T ~ Instant.MAX.checkedZoned                ==== thrown[DateTimeException]
    T ~ Instant.MIN.checkedZoned                ==== thrown[DateTimeException]

    val n9 = 999999999
    def ioes(s: Long, nano: Int = 0): Instant = Instant.ofEpochSecond(s, nano)
    T ~ ioes(100, 3000).floor.us      ==== ioes(100, 3000) --: typed[Instant]
    T ~ ioes(100, 3999).floor.us      ==== ioes(100, 3000)
    T ~ ioes(-10, 3000).floor.us      ==== ioes(-10, 3000)
    T ~ ioes(-10, 3999).floor.us      ==== ioes(-10, 3000)
    T ~ ioes(100, 3000).round.us      ==== ioes(100, 3000) --: typed[Instant]
    T ~ ioes(100, 3500).round.us      ==== ioes(100, 3000)
    T ~ ioes(100, 3501).round.us      ==== ioes(100, 4000)
    T ~ ioes(100, 999999501).round.us ==== ioes(101)
    T ~ ioes(-10, 3000).round.us      ==== ioes(-10, 3000)
    T ~ ioes(-10, 3500).round.us      ==== ioes(-10, 3000)
    T ~ ioes(-10, 3501).round.us      ==== ioes(-10, 4000)
    T ~ ioes(-10, 999999501).round.us ==== ioes( -9)
    T ~ ioes(100, 3000).ceil.us       ==== ioes(100, 3000) --: typed[Instant]
    T ~ ioes(100, 3001).ceil.us       ==== ioes(100, 4000)
    T ~ ioes(100, 999999001).ceil.us  ==== ioes(101)
    T ~ ioes(-10, 3000).ceil.us       ==== ioes(-10, 3000)
    T ~ ioes(-10, 3001).ceil.us       ==== ioes(-10, 4000)
    T ~ ioes(-10, 999999001).ceil.us  ==== ioes( -9)
    T ~ ioes(100, 3000000).floor.ms   ==== ioes(100, 3000000) --: typed[Instant]
    T ~ ioes(100, 3999999).floor.ms   ==== ioes(100, 3000000)
    T ~ ioes(-10, 3000000).floor.ms   ==== ioes(-10, 3000000)
    T ~ ioes(-10, 3999999).floor.ms   ==== ioes(-10, 3000000)
    T ~ ioes(100, 3000000).round.ms   ==== ioes(100, 3000000) --: typed[Instant]
    T ~ ioes(100, 3500000).round.ms   ==== ioes(100, 3000000)
    T ~ ioes(100, 3500001).round.ms   ==== ioes(100, 4000000)
    T ~ ioes(100, 999500001).round.ms ==== ioes(101)
    T ~ ioes(-10, 3000000).round.ms   ==== ioes(-10, 3000000)
    T ~ ioes(-10, 3500000).round.ms   ==== ioes(-10, 3000000)
    T ~ ioes(-10, 3500001).round.ms   ==== ioes(-10, 4000000)
    T ~ ioes(-10, 999500001).round.ms ==== ioes( -9)
    T ~ ioes(100, 3000000).ceil.ms    ==== ioes(100, 3000000) --: typed[Instant]
    T ~ ioes(100, 3000001).ceil.ms    ==== ioes(100, 4000000)
    T ~ ioes(100, 999000001).ceil.ms  ==== ioes(101)
    T ~ ioes(-10, 3000000).ceil.ms    ==== ioes(-10, 3000000)
    T ~ ioes(-10, 3000001).ceil.ms    ==== ioes(-10, 4000000)
    T ~ ioes(-10, 999000001).ceil.ms  ==== ioes( -9)
    T ~ ioes(100).floor.s             ==== ioes(100) --: typed[Instant]
    T ~ ioes(100, n9).floor.s         ==== ioes(100)
    T ~ ioes(-10).floor.s             ==== ioes(-10)
    T ~ ioes(-10, n9).floor.s         ==== ioes(-10)
    T ~ ioes(100).round.s             ==== ioes(100) --: typed[Instant]
    T ~ ioes(100, 500000000).round.s  ==== ioes(100)
    T ~ ioes(100, 500000001).round.s  ==== ioes(101)
    T ~ ioes(-10).round.s             ==== ioes(-10)
    T ~ ioes(-10, 500000000).round.s  ==== ioes(-10)
    T ~ ioes(-10, 500000001).round.s  ==== ioes( -9)
    T ~ ioes(100).ceil.s              ==== ioes(100) --: typed[Instant]
    T ~ ioes(100, 1).ceil.s           ==== ioes(101)
    T ~ ioes(-10).ceil.s              ==== ioes(-10)
    T ~ ioes(-10, 1).ceil.s           ==== ioes( -9)
    T ~ ioes(6120).floor.m            ==== ioes(6120) --: typed[Instant]
    T ~ ioes(6179, n9).floor.m        ==== ioes(6120)
    T ~ ioes(6120, 1).floor.m         ==== ioes(6120)
    T ~ ioes(-180).floor.m            ==== ioes(-180)
    T ~ ioes(-121, n9).floor.m        ==== ioes(-180)
    T ~ ioes(-180, 1).floor.m         ==== ioes(-180)
    T ~ ioes(6120).round.m            ==== ioes(6120) --: typed[Instant]
    T ~ ioes(6150).round.m            ==== ioes(6120)
    T ~ ioes(6150, 1).round.m         ==== ioes(6180)
    T ~ ioes(6151).round.m            ==== ioes(6180)
    T ~ ioes(-180).round.m            ==== ioes(-180)
    T ~ ioes(-150).round.m            ==== ioes(-180)
    T ~ ioes(-150, 1).round.m         ==== ioes(-120)
    T ~ ioes(-149).round.m            ==== ioes(-120)
    T ~ ioes(6120).ceil.m             ==== ioes(6120) --: typed[Instant]
    T ~ ioes(6120, 1).ceil.m          ==== ioes(6180)
    T ~ ioes(6121).ceil.m             ==== ioes(6180)
    T ~ ioes(-180).ceil.m             ==== ioes(-180)
    T ~ ioes(-180, 1).ceil.m          ==== ioes(-120)
    T ~ ioes(-179).ceil.m             ==== ioes(-120)
    T ~ ioes(612000).floor.h          ==== ioes(612000) --: typed[Instant]
    T ~ ioes(615599, n9).floor.h      ==== ioes(612000)
    T ~ ioes(612000, 1).floor.h       ==== ioes(612000)
    T ~ ioes(-14400).floor.h          ==== ioes(-14400)
    T ~ ioes(-10801, n9).floor.h      ==== ioes(-14400)
    T ~ ioes(-14400, 1).floor.h       ==== ioes(-14400)
    T ~ ioes(612000).round.h          ==== ioes(612000) --: typed[Instant]
    T ~ ioes(613800).round.h          ==== ioes(612000)
    T ~ ioes(613800, 1).round.h       ==== ioes(615600)
    T ~ ioes(613801).round.h          ==== ioes(615600)
    T ~ ioes(-14400).round.h          ==== ioes(-14400)
    T ~ ioes(-12600).round.h          ==== ioes(-14400)
    T ~ ioes(-12600, 1).round.h       ==== ioes(-10800)
    T ~ ioes(-12599).round.h          ==== ioes(-10800)
    T ~ ioes(612000).ceil.h           ==== ioes(612000) --: typed[Instant]
    T ~ ioes(612000, 1).ceil.h        ==== ioes(615600)
    T ~ ioes(612001).ceil.h           ==== ioes(615600)
    T ~ ioes(-14400).ceil.h           ==== ioes(-14400)
    T ~ ioes(-14400, 1).ceil.h        ==== ioes(-10800)
    T ~ ioes(-14399).ceil.h           ==== ioes(-10800)
    T ~ ioes(1468800).floor.d         ==== ioes(1468800) --: typed[Instant]
    T ~ ioes(1555199, n9).floor.d     ==== ioes(1468800)
    T ~ ioes(1468800, 1).floor.d      ==== ioes(1468800)
    T ~ ioes(-259200).floor.d         ==== ioes(-259200)
    T ~ ioes(-172801, n9).floor.d     ==== ioes(-259200)
    T ~ ioes(-259200, 1).floor.d      ==== ioes(-259200)
    T ~ ioes(1468800).round.d         ==== ioes(1468800) --: typed[Instant]
    T ~ ioes(1512000).round.d         ==== ioes(1468800)
    T ~ ioes(1512000, 1).round.d      ==== ioes(1555200)
    T ~ ioes(1512001).round.d         ==== ioes(1555200)
    T ~ ioes(-259200).round.d         ==== ioes(-259200)
    T ~ ioes(-216000).round.d         ==== ioes(-259200)
    T ~ ioes(-216000, 1).round.d      ==== ioes(-172800)
    T ~ ioes(-215999).round.d         ==== ioes(-172800)
    T ~ ioes(1468800).ceil.d          ==== ioes(1468800) --: typed[Instant]
    T ~ ioes(1468800, 1).ceil.d       ==== ioes(1555200)
    T ~ ioes(1468801).ceil.d          ==== ioes(1555200)
    T ~ ioes(-259200).ceil.d          ==== ioes(-259200)
    T ~ ioes(-259200, 1).ceil.d       ==== ioes(-172800)
    T ~ ioes(-259199).ceil.d          ==== ioes(-172800)
    T ~ i.round.days ==== i.round.d --: typed[Instant]
    T ~ i.floor.days ==== i.floor.d --: typed[Instant]
    T ~ i.ceil.days  ==== i.ceil.d  --: typed[Instant]
    T ~ Instant.MAX.round.us ==== ioes(Instant.MAX.getEpochSecond, 999999000)
    T ~ Instant.MAX.round.ms ==== ioes(Instant.MAX.getEpochSecond, 999000000)
    T ~ Instant.MAX.round.s  ==== ioes(Instant.MAX.getEpochSecond)
    T ~ Instant.MAX.round.m  ==== ioes(Instant.MAX.getEpochSecond.fn(s => s - (s % 60)))
    T ~ Instant.MAX.round.h  ==== ioes(Instant.MAX.getEpochSecond.fn(s => s - (s % 3600)))
    T ~ Instant.MAX.round.d  ==== ioes(Instant.MAX.getEpochSecond.fn(s => s - (s % 86400)))
    T ~ Instant.MAX.ceil.us  ==== Instant.MAX.round.us
    T ~ Instant.MAX.ceil.ms  ==== Instant.MAX.round.ms
    T ~ Instant.MAX.ceil.s   ==== Instant.MAX.round.s 
    T ~ Instant.MAX.ceil.m   ==== Instant.MAX.round.m 
    T ~ Instant.MAX.ceil.h   ==== Instant.MAX.round.h 
    T ~ Instant.MAX.ceil.d   ==== Instant.MAX.round.d 

  def testLocalDateTime(): Unit =
    val l = LocalDateTime.now
    val idt = Instant.now
    val odt = OffsetDateTime.now
    val zdt = ZonedDateTime.now
    val gig = Instant.ofEpochSecond(1000000000).local
    val giggle = Instant.ofEpochSecond(1000000001, 234567890).local
    val big = Duration.between(gig, LocalDateTime.MAX)
    val bigger = Duration.between(LocalDateTime.MIN, giggle)
    T ~ (gig + 1.s)     ==== Instant.ofEpochSecond(1000000001).local --: typed[LocalDateTime]
    T ~ (giggle + big)  ==== LocalDateTime.MAX
    T ~ (gig +! 1.s)    ==== (gig + 1.s) --: typed[LocalDateTime]
    T ~ (giggle +! big) ==== thrown[DateTimeException]
    T ~ (gig - 1.s)     ==== Instant.ofEpochSecond(999999999).local  --: typed[LocalDateTime]
    T ~ (gig - bigger)  ==== LocalDateTime.MIN
    T ~ (gig -! 1.s)    ==== (gig - 1.s)                       --: typed[LocalDateTime]
    T ~ (gig -! bigger) ==== thrown[DateTimeException]
    T ~ (giggle - gig)  ==== 1234567890.ns                     --: typed[Duration]
    T ~ (giggle to gig) ==== -1234567890.ns                    --: typed[Duration]
    T ~ (gig < giggle)  ==== true
    T ~ (gig < gig)     ==== false
    T ~ (giggle < gig)  ==== false
    T ~ (gig <= giggle) ==== true
    T ~ (gig <= gig)    ==== true
    T ~ (giggle <= gig) ==== false
    T ~ (gig >= giggle) ==== false
    T ~ (gig >= gig)    ==== true
    T ~ (giggle >= gig) ==== true
    T ~ (gig > giggle)  ==== false
    T ~ (gig > gig)     ==== false 
    T ~ (giggle > gig)  ==== true
    T ~ (gig max giggle)                 ==== giggle      --: typed[LocalDateTime]
    T ~ (giggle max gig)                 ==== giggle
    T ~ (gig min giggle)                 ==== gig         --: typed[LocalDateTime]
    T ~ (giggle min gig)                 ==== gig
    T ~ (gig + 1.s).clamp(gig, giggle)   ==== (gig + 1.s) --: typed[LocalDateTime]
    T ~ gig.clamp(gig + 1.s, giggle)     ==== (gig + 1.s)
    T ~ giggle.clamp(gig, gig + 1.s)     ==== (gig + 1.s)
    T ~ (gig + 1.s).clamp(giggle, gig)   ==== giggle
    T ~ (gig + 1.s).in(gig, giggle)      ==== true
    T ~ gig.in(gig + 1.s, giggle)        ==== false
    T ~ giggle.in(gig, gig + 1.s)        ==== false
    T ~ (gig + 1.s).checkIn(gig, giggle) ==== (gig + 1.s) --: typed[LocalDateTime]
    T ~ gig.checkIn(gig + 1.s, giggle)   ==== thrown[DateTimeException]
    T ~ giggle.checkIn(gig, gig + 1.s)   ==== thrown[DateTimeException]
    T ~ gig.D                                    ==== 1e9 --: typed[DoubleInstant]
    T ~ gig.filetime                             ==== FileTime.fromMillis(1000000000000L) --: typed[FileTime]
    T ~ gig.instant                              ==== typed[Instant]
    T ~ (Duration.between(l.instant, idt) < 1.m) ==== true
    T ~ gig.offset                               ==== typed[OffsetDateTime]
    T ~ (Duration.between(l.offset, odt) < 1.m)  ==== true
    T ~ gig.utc                                 ==== typed[OffsetDateTime]
    T ~ (Duration.between(l.utc, odt) < 1.m)    ==== true
    T ~ gig.checkedUTC                          ==== gig.utc   --: typed[OffsetDateTime]
    ZoneId.systemDefault.getRules.getOffset(LocalDateTime.MAX).getTotalSeconds match
      case x if x > 0 =>
        T ~ LocalDateTime.MIN.utc        ==== LocalDateTime.MIN.atOffset(ZoneOffset.UTC)
        T ~ LocalDateTime.MIN.checkedUTC ==== thrown[DateTimeException]
      case x if x < 0 =>
        T ~ LocalDateTime.MAX.utc        ==== LocalDateTime.MAX.atOffset(ZoneOffset.UTC)
        T ~ LocalDateTime.MAX.checkedUTC ==== thrown[DateTimeException]
      case _ =>
        println(s"Warning--local time matches UTC, so local/UTC bounds safety not determined")
    T ~ gig.zoned                               ==== typed[ZonedDateTime]
    T ~ (Duration.between(l.zoned, odt) < 1.m)  ==== true

    val n9 = 999999999
    def loes(s: Long, nano: Int = 0): LocalDateTime =
      val i = Instant.ofEpochSecond(s, nano)
      val shift = ZoneId.systemDefault.getRules.getOffset(i).getTotalSeconds
      (i - shift.s).local
    T ~ loes(100, 3000).floor.us      ==== loes(100, 3000) --: typed[LocalDateTime]
    T ~ loes(100, 3999).floor.us      ==== loes(100, 3000)
    T ~ loes(-10, 3000).floor.us      ==== loes(-10, 3000)
    T ~ loes(-10, 3999).floor.us      ==== loes(-10, 3000)
    T ~ loes(100, 3000).round.us      ==== loes(100, 3000) --: typed[LocalDateTime]
    T ~ loes(100, 3500).round.us      ==== loes(100, 3000)
    T ~ loes(100, 3501).round.us      ==== loes(100, 4000)
    T ~ loes(100, 999999501).round.us ==== loes(101)
    T ~ loes(-10, 3000).round.us      ==== loes(-10, 3000)
    T ~ loes(-10, 3500).round.us      ==== loes(-10, 3000)
    T ~ loes(-10, 3501).round.us      ==== loes(-10, 4000)
    T ~ loes(-10, 999999501).round.us ==== loes( -9)
    T ~ loes(100, 3000).ceil.us       ==== loes(100, 3000) --: typed[LocalDateTime]
    T ~ loes(100, 3001).ceil.us       ==== loes(100, 4000)
    T ~ loes(100, 999999001).ceil.us  ==== loes(101)
    T ~ loes(-10, 3000).ceil.us       ==== loes(-10, 3000)
    T ~ loes(-10, 3001).ceil.us       ==== loes(-10, 4000)
    T ~ loes(-10, 999999001).ceil.us  ==== loes( -9)
    T ~ loes(100, 3000000).floor.ms   ==== loes(100, 3000000) --: typed[LocalDateTime]
    T ~ loes(100, 3999999).floor.ms   ==== loes(100, 3000000)
    T ~ loes(-10, 3000000).floor.ms   ==== loes(-10, 3000000)
    T ~ loes(-10, 3999999).floor.ms   ==== loes(-10, 3000000)
    T ~ loes(100, 3000000).round.ms   ==== loes(100, 3000000) --: typed[LocalDateTime]
    T ~ loes(100, 3500000).round.ms   ==== loes(100, 3000000)
    T ~ loes(100, 3500001).round.ms   ==== loes(100, 4000000)
    T ~ loes(100, 999500001).round.ms ==== loes(101)
    T ~ loes(-10, 3000000).round.ms   ==== loes(-10, 3000000)
    T ~ loes(-10, 3500000).round.ms   ==== loes(-10, 3000000)
    T ~ loes(-10, 3500001).round.ms   ==== loes(-10, 4000000)
    T ~ loes(-10, 999500001).round.ms ==== loes( -9)
    T ~ loes(100, 3000000).ceil.ms    ==== loes(100, 3000000) --: typed[LocalDateTime]
    T ~ loes(100, 3000001).ceil.ms    ==== loes(100, 4000000)
    T ~ loes(100, 999000001).ceil.ms  ==== loes(101)
    T ~ loes(-10, 3000000).ceil.ms    ==== loes(-10, 3000000)
    T ~ loes(-10, 3000001).ceil.ms    ==== loes(-10, 4000000)
    T ~ loes(-10, 999000001).ceil.ms  ==== loes( -9)
    T ~ loes(100).floor.s             ==== loes(100) --: typed[LocalDateTime]
    T ~ loes(100, n9).floor.s         ==== loes(100)
    T ~ loes(-10).floor.s             ==== loes(-10)
    T ~ loes(-10, n9).floor.s         ==== loes(-10)
    T ~ loes(100).round.s             ==== loes(100) --: typed[LocalDateTime]
    T ~ loes(100, 500000000).round.s  ==== loes(100)
    T ~ loes(100, 500000001).round.s  ==== loes(101)
    T ~ loes(-10).round.s             ==== loes(-10)
    T ~ loes(-10, 500000000).round.s  ==== loes(-10)
    T ~ loes(-10, 500000001).round.s  ==== loes( -9)
    T ~ loes(100).ceil.s              ==== loes(100) --: typed[LocalDateTime]
    T ~ loes(100, 1).ceil.s           ==== loes(101)
    T ~ loes(-10).ceil.s              ==== loes(-10)
    T ~ loes(-10, 1).ceil.s           ==== loes( -9)
    T ~ loes(6120).floor.m            ==== loes(6120) --: typed[LocalDateTime]
    T ~ loes(6179, n9).floor.m        ==== loes(6120)
    T ~ loes(6120, 1).floor.m         ==== loes(6120)
    T ~ loes(-180).floor.m            ==== loes(-180)
    T ~ loes(-121, n9).floor.m        ==== loes(-180)
    T ~ loes(-180, 1).floor.m         ==== loes(-180)
    T ~ loes(6120).round.m            ==== loes(6120) --: typed[LocalDateTime]
    T ~ loes(6150).round.m            ==== loes(6120)
    T ~ loes(6150, 1).round.m         ==== loes(6180)
    T ~ loes(6151).round.m            ==== loes(6180)
    T ~ loes(-180).round.m            ==== loes(-180)
    T ~ loes(-150).round.m            ==== loes(-180)
    T ~ loes(-150, 1).round.m         ==== loes(-120)
    T ~ loes(-149).round.m            ==== loes(-120)
    T ~ loes(6120).ceil.m             ==== loes(6120) --: typed[LocalDateTime]
    T ~ loes(6120, 1).ceil.m          ==== loes(6180)
    T ~ loes(6121).ceil.m             ==== loes(6180)
    T ~ loes(-180).ceil.m             ==== loes(-180)
    T ~ loes(-180, 1).ceil.m          ==== loes(-120)
    T ~ loes(-179).ceil.m             ==== loes(-120)
    T ~ loes(612000).floor.h          ==== loes(612000) --: typed[LocalDateTime]
    T ~ loes(615599, n9).floor.h      ==== loes(612000)
    T ~ loes(612000, 1).floor.h       ==== loes(612000)
    T ~ loes(-14400).floor.h          ==== loes(-14400)
    T ~ loes(-10801, n9).floor.h      ==== loes(-14400)
    T ~ loes(-14400, 1).floor.h       ==== loes(-14400)
    T ~ loes(612000).round.h          ==== loes(612000) --: typed[LocalDateTime]
    T ~ loes(613800).round.h          ==== loes(612000)
    T ~ loes(613800, 1).round.h       ==== loes(615600)
    T ~ loes(613801).round.h          ==== loes(615600)
    T ~ loes(-14400).round.h          ==== loes(-14400)
    T ~ loes(-12600).round.h          ==== loes(-14400)
    T ~ loes(-12600, 1).round.h       ==== loes(-10800)
    T ~ loes(-12599).round.h          ==== loes(-10800)
    T ~ loes(612000).ceil.h           ==== loes(612000) --: typed[LocalDateTime]
    T ~ loes(612000, 1).ceil.h        ==== loes(615600)
    T ~ loes(612001).ceil.h           ==== loes(615600)
    T ~ loes(-14400).ceil.h           ==== loes(-14400)
    T ~ loes(-14400, 1).ceil.h        ==== loes(-10800)
    T ~ loes(-14399).ceil.h           ==== loes(-10800)
    T ~ loes(1468800).floor.d         ==== loes(1468800) --: typed[LocalDateTime]
    T ~ loes(1555199, n9).floor.d     ==== loes(1468800)
    T ~ loes(1468800, 1).floor.d      ==== loes(1468800)
    T ~ loes(-259200).floor.d         ==== loes(-259200)
    T ~ loes(-172801, n9).floor.d     ==== loes(-259200)
    T ~ loes(-259200, 1).floor.d      ==== loes(-259200)
    T ~ loes(1468800).round.d         ==== loes(1468800) --: typed[LocalDateTime]
    T ~ loes(1512000).round.d         ==== loes(1468800)
    T ~ loes(1512000, 1).round.d      ==== loes(1555200)
    T ~ loes(1512001).round.d         ==== loes(1555200)
    T ~ loes(-259200).round.d         ==== loes(-259200)
    T ~ loes(-216000).round.d         ==== loes(-259200)
    T ~ loes(-216000, 1).round.d      ==== loes(-172800)
    T ~ loes(-215999).round.d         ==== loes(-172800)
    T ~ loes(1468800).ceil.d          ==== loes(1468800) --: typed[LocalDateTime]
    T ~ loes(1468800, 1).ceil.d       ==== loes(1555200)
    T ~ loes(1468801).ceil.d          ==== loes(1555200)
    T ~ loes(-259200).ceil.d          ==== loes(-259200)
    T ~ loes(-259200, 1).ceil.d       ==== loes(-172800)
    T ~ loes(-259199).ceil.d          ==== loes(-172800)
    T ~ l.round.days ==== l.round.d --: typed[LocalDateTime]
    T ~ l.floor.days ==== l.floor.d --: typed[LocalDateTime]
    T ~ l.ceil.days  ==== l.ceil.d  --: typed[LocalDateTime]
    T ~ LocalDateTime.MAX.round.us ==== LocalDateTime.MAX.minus(999.ns)
    T ~ LocalDateTime.MAX.round.ms ==== LocalDateTime.MAX.minus(999999.ns)
    T ~ LocalDateTime.MAX.round.s  ==== LocalDateTime.MAX.minus(999999999.ns)
    T ~ LocalDateTime.MAX.round.m  ==== LocalDateTime.MAX.minus(59.s + 999999999.ns)
    T ~ LocalDateTime.MAX.round.h  ==== LocalDateTime.MAX.minus(59.m + 59.s + 999999999.ns)
    T ~ LocalDateTime.MAX.round.d  ==== LocalDateTime.MAX.minus(23.h + 59.m + 59.s + 999999999.ns)
    T ~ LocalDateTime.MAX.ceil.us  ==== LocalDateTime.MAX.round.us
    T ~ LocalDateTime.MAX.ceil.ms  ==== LocalDateTime.MAX.round.ms
    T ~ LocalDateTime.MAX.ceil.s   ==== LocalDateTime.MAX.round.s 
    T ~ LocalDateTime.MAX.ceil.m   ==== LocalDateTime.MAX.round.m 
    T ~ LocalDateTime.MAX.ceil.h   ==== LocalDateTime.MAX.round.h 
    T ~ LocalDateTime.MAX.ceil.d   ==== LocalDateTime.MAX.round.d


  def testOffsetDateTime(): Unit =
    val o = OffsetDateTime.now
    val idt = Instant.now
    val ldt = LocalDateTime.now
    val udt = OffsetDateTime.now.withOffsetSameInstant(ZoneOffset.UTC)
    val zdt = ZonedDateTime.now
    val gig = Instant.ofEpochSecond(1000000000).offset
    val gofs = gig.getOffset
    val giggle = Instant.ofEpochSecond(1000000001, 234567890).offset
    val big = gig to gig.MaxValue
    val bigger = giggle.MinValue to giggle
    T ~ (gig + 1.s)     ==== Instant.ofEpochSecond(1000000001).offset --: typed[OffsetDateTime]
    T ~ (giggle + big)  ==== giggle.MaxValue
    T ~ (gig +! 1.s)    ==== (gig + 1.s) --: typed[OffsetDateTime]
    T ~ (giggle +! big) ==== thrown[DateTimeException]
    T ~ (gig - 1.s)     ==== Instant.ofEpochSecond(999999999).offset  --: typed[OffsetDateTime]
    T ~ (gig - bigger)  ==== gig.MinValue
    T ~ (gig -! 1.s)    ==== (gig - 1.s)                       --: typed[OffsetDateTime]
    T ~ (gig -! bigger) ==== thrown[DateTimeException]
    T ~ (giggle - gig)  ==== 1234567890.ns                     --: typed[Duration]
    T ~ (giggle to gig) ==== -1234567890.ns                    --: typed[Duration]
    T ~ (gig < giggle)  ==== true
    T ~ (gig < gig)     ==== false
    T ~ (giggle < gig)  ==== false
    T ~ (gig <= giggle) ==== true
    T ~ (gig <= gig)    ==== true
    T ~ (giggle <= gig) ==== false
    T ~ (gig >= giggle) ==== false
    T ~ (gig >= gig)    ==== true
    T ~ (giggle >= gig) ==== true
    T ~ (gig > giggle)  ==== false
    T ~ (gig > gig)     ==== false 
    T ~ (giggle > gig)  ==== true
    T ~ (gig max giggle)                 ==== giggle      --: typed[OffsetDateTime]
    T ~ (giggle max gig)                 ==== giggle
    T ~ (gig min giggle)                 ==== gig         --: typed[OffsetDateTime]
    T ~ (giggle min gig)                 ==== gig
    T ~ (gig + 1.s).clamp(gig, giggle)   ==== (gig + 1.s) --: typed[OffsetDateTime]
    T ~ gig.clamp(gig + 1.s, giggle)     ==== (gig + 1.s)
    T ~ giggle.clamp(gig, gig + 1.s)     ==== (gig + 1.s)
    T ~ (gig + 1.s).clamp(giggle, gig)   ==== giggle
    T ~ (gig + 1.s).in(gig, giggle)      ==== true
    T ~ gig.in(gig + 1.s, giggle)        ==== false
    T ~ giggle.in(gig, gig + 1.s)        ==== false
    T ~ (gig + 1.s).checkIn(gig, giggle) ==== (gig + 1.s) --: typed[OffsetDateTime]
    T ~ gig.checkIn(gig + 1.s, giggle)   ==== thrown[DateTimeException]
    T ~ giggle.checkIn(gig, gig + 1.s)   ==== thrown[DateTimeException]
    T ~ gig.D                                    ==== 1e9 --: typed[DoubleInstant]
    T ~ gig.filetime                             ==== FileTime.fromMillis(1000000000000L) --: typed[FileTime]
    T ~ gig.instant                              ==== typed[Instant]
    T ~ (Duration.between(o.instant, idt) < 1.m) ==== true
    T ~ gig.local                                ==== typed[LocalDateTime]
    T ~ (Duration.between(o.local, ldt) < 1.m)   ==== true
    T ~ gig.utc                                  ==== typed[OffsetDateTime]
    T ~ (Duration.between(o.utc, udt) < 1.m)     ==== true
    T ~ gig.checkedUTC                           ==== gig.utc   --: typed[OffsetDateTime]
    T ~ gig.utc.offset                           ==== gig       --: typed[OffsetDateTime]
    T ~ gig.utc.offset.getOffset                 ==== gig.getOffset
    T ~ gig.utc.checkedOffset                    ==== gig       --: typed[OffsetDateTime]
    T ~ gig.utc.checkedOffset.getOffset          ==== gig.getOffset
    T ~ (Duration.between(o.zoned, zdt) < 1.m)   ==== true
    T ~ gig.checkedZoned                         ==== gig.zoned --: typed[ZonedDateTime]

    if o.getOffset.getTotalSeconds < 0 then
      val farOff = ZoneOffset ofHours 11
      val f = o withOffsetSameInstant farOff
      T ~ f.MinValue.local         ==== LocalDateTime.MIN
      T ~ f.MinValue.checkedLocal  ==== thrown[DateTimeException]
      T ~ f.MinValue.offset        ==== LocalDateTime.MIN.atOffset(ZoneId.systemDefault.getRules.getOffset(LocalDateTime.MIN))
      T ~ f.MinValue.checkedOffset ==== thrown[DateTimeException]
      T ~ f.MinValue.utc           ==== LocalDateTime.MIN.atOffset(ZoneOffset.UTC)
      T ~ f.MinValue.checkedUTC    ==== thrown[DateTimeException]
      T ~ f.MinValue.zoned         ==== LocalDateTime.MIN.atZone(ZoneId.systemDefault)
      T ~ f.MinValue.checkedZoned  ==== thrown[DateTimeException]
    else
      val farOff = ZoneOffset ofHours -9
      val f = o withOffsetSameInstant farOff
      T ~ f.MaxValue.local         ==== LocalDateTime.MAX
      T ~ f.MaxValue.checkedLocal  ==== thrown[DateTimeException]
      T ~ f.MaxValue.offset        ==== LocalDateTime.MAX.atOffset(ZoneId.systemDefault.getRules.getOffset(LocalDateTime.MAX))
      T ~ f.MaxValue.checkedOffset ==== thrown[DateTimeException]
      T ~ f.MaxValue.utc           ==== LocalDateTime.MAX.atOffset(ZoneOffset.UTC)
      T ~ f.MaxValue.checkedUTC    ==== thrown[DateTimeException]
      T ~ f.MaxValue.zoned         ==== LocalDateTime.MAX.atZone(ZoneId.systemDefault)
      T ~ f.MaxValue.checkedZoned  ==== thrown[DateTimeException]

    val n9 = 999999999
    def ooes(s: Long, nano: Int = 0): OffsetDateTime =
      val i = Instant.ofEpochSecond(s, nano)
      val shift = ZoneId.systemDefault.getRules.getOffset(i).getTotalSeconds
      (i - shift.s).offset
    T ~ ooes(100, 3000).floor.us      ==== ooes(100, 3000) --: typed[OffsetDateTime]
    T ~ ooes(100, 3999).floor.us      ==== ooes(100, 3000)
    T ~ ooes(-10, 3000).floor.us      ==== ooes(-10, 3000)
    T ~ ooes(-10, 3999).floor.us      ==== ooes(-10, 3000)
    T ~ ooes(100, 3000).round.us      ==== ooes(100, 3000) --: typed[OffsetDateTime]
    T ~ ooes(100, 3500).round.us      ==== ooes(100, 3000)
    T ~ ooes(100, 3501).round.us      ==== ooes(100, 4000)
    T ~ ooes(100, 999999501).round.us ==== ooes(101)
    T ~ ooes(-10, 3000).round.us      ==== ooes(-10, 3000)
    T ~ ooes(-10, 3500).round.us      ==== ooes(-10, 3000)
    T ~ ooes(-10, 3501).round.us      ==== ooes(-10, 4000)
    T ~ ooes(-10, 999999501).round.us ==== ooes( -9)
    T ~ ooes(100, 3000).ceil.us       ==== ooes(100, 3000) --: typed[OffsetDateTime]
    T ~ ooes(100, 3001).ceil.us       ==== ooes(100, 4000)
    T ~ ooes(100, 999999001).ceil.us  ==== ooes(101)
    T ~ ooes(-10, 3000).ceil.us       ==== ooes(-10, 3000)
    T ~ ooes(-10, 3001).ceil.us       ==== ooes(-10, 4000)
    T ~ ooes(-10, 999999001).ceil.us  ==== ooes( -9)
    T ~ ooes(100, 3000000).floor.ms   ==== ooes(100, 3000000) --: typed[OffsetDateTime]
    T ~ ooes(100, 3999999).floor.ms   ==== ooes(100, 3000000)
    T ~ ooes(-10, 3000000).floor.ms   ==== ooes(-10, 3000000)
    T ~ ooes(-10, 3999999).floor.ms   ==== ooes(-10, 3000000)
    T ~ ooes(100, 3000000).round.ms   ==== ooes(100, 3000000) --: typed[OffsetDateTime]
    T ~ ooes(100, 3500000).round.ms   ==== ooes(100, 3000000)
    T ~ ooes(100, 3500001).round.ms   ==== ooes(100, 4000000)
    T ~ ooes(100, 999500001).round.ms ==== ooes(101)
    T ~ ooes(-10, 3000000).round.ms   ==== ooes(-10, 3000000)
    T ~ ooes(-10, 3500000).round.ms   ==== ooes(-10, 3000000)
    T ~ ooes(-10, 3500001).round.ms   ==== ooes(-10, 4000000)
    T ~ ooes(-10, 999500001).round.ms ==== ooes( -9)
    T ~ ooes(100, 3000000).ceil.ms    ==== ooes(100, 3000000) --: typed[OffsetDateTime]
    T ~ ooes(100, 3000001).ceil.ms    ==== ooes(100, 4000000)
    T ~ ooes(100, 999000001).ceil.ms  ==== ooes(101)
    T ~ ooes(-10, 3000000).ceil.ms    ==== ooes(-10, 3000000)
    T ~ ooes(-10, 3000001).ceil.ms    ==== ooes(-10, 4000000)
    T ~ ooes(-10, 999000001).ceil.ms  ==== ooes( -9)
    T ~ ooes(100).floor.s             ==== ooes(100) --: typed[OffsetDateTime]
    T ~ ooes(100, n9).floor.s         ==== ooes(100)
    T ~ ooes(-10).floor.s             ==== ooes(-10)
    T ~ ooes(-10, n9).floor.s         ==== ooes(-10)
    T ~ ooes(100).round.s             ==== ooes(100) --: typed[OffsetDateTime]
    T ~ ooes(100, 500000000).round.s  ==== ooes(100)
    T ~ ooes(100, 500000001).round.s  ==== ooes(101)
    T ~ ooes(-10).round.s             ==== ooes(-10)
    T ~ ooes(-10, 500000000).round.s  ==== ooes(-10)
    T ~ ooes(-10, 500000001).round.s  ==== ooes( -9)
    T ~ ooes(100).ceil.s              ==== ooes(100) --: typed[OffsetDateTime]
    T ~ ooes(100, 1).ceil.s           ==== ooes(101)
    T ~ ooes(-10).ceil.s              ==== ooes(-10)
    T ~ ooes(-10, 1).ceil.s           ==== ooes( -9)
    T ~ ooes(6120).floor.m            ==== ooes(6120) --: typed[OffsetDateTime]
    T ~ ooes(6179, n9).floor.m        ==== ooes(6120)
    T ~ ooes(6120, 1).floor.m         ==== ooes(6120)
    T ~ ooes(-180).floor.m            ==== ooes(-180)
    T ~ ooes(-121, n9).floor.m        ==== ooes(-180)
    T ~ ooes(-180, 1).floor.m         ==== ooes(-180)
    T ~ ooes(6120).round.m            ==== ooes(6120) --: typed[OffsetDateTime]
    T ~ ooes(6150).round.m            ==== ooes(6120)
    T ~ ooes(6150, 1).round.m         ==== ooes(6180)
    T ~ ooes(6151).round.m            ==== ooes(6180)
    T ~ ooes(-180).round.m            ==== ooes(-180)
    T ~ ooes(-150).round.m            ==== ooes(-180)
    T ~ ooes(-150, 1).round.m         ==== ooes(-120)
    T ~ ooes(-149).round.m            ==== ooes(-120)
    T ~ ooes(6120).ceil.m             ==== ooes(6120) --: typed[OffsetDateTime]
    T ~ ooes(6120, 1).ceil.m          ==== ooes(6180)
    T ~ ooes(6121).ceil.m             ==== ooes(6180)
    T ~ ooes(-180).ceil.m             ==== ooes(-180)
    T ~ ooes(-180, 1).ceil.m          ==== ooes(-120)
    T ~ ooes(-179).ceil.m             ==== ooes(-120)
    T ~ ooes(612000).floor.h          ==== ooes(612000) --: typed[OffsetDateTime]
    T ~ ooes(615599, n9).floor.h      ==== ooes(612000)
    T ~ ooes(612000, 1).floor.h       ==== ooes(612000)
    T ~ ooes(-14400).floor.h          ==== ooes(-14400)
    T ~ ooes(-10801, n9).floor.h      ==== ooes(-14400)
    T ~ ooes(-14400, 1).floor.h       ==== ooes(-14400)
    T ~ ooes(612000).round.h          ==== ooes(612000) --: typed[OffsetDateTime]
    T ~ ooes(613800).round.h          ==== ooes(612000)
    T ~ ooes(613800, 1).round.h       ==== ooes(615600)
    T ~ ooes(613801).round.h          ==== ooes(615600)
    T ~ ooes(-14400).round.h          ==== ooes(-14400)
    T ~ ooes(-12600).round.h          ==== ooes(-14400)
    T ~ ooes(-12600, 1).round.h       ==== ooes(-10800)
    T ~ ooes(-12599).round.h          ==== ooes(-10800)
    T ~ ooes(612000).ceil.h           ==== ooes(612000) --: typed[OffsetDateTime]
    T ~ ooes(612000, 1).ceil.h        ==== ooes(615600)
    T ~ ooes(612001).ceil.h           ==== ooes(615600)
    T ~ ooes(-14400).ceil.h           ==== ooes(-14400)
    T ~ ooes(-14400, 1).ceil.h        ==== ooes(-10800)
    T ~ ooes(-14399).ceil.h           ==== ooes(-10800)
    T ~ ooes(1468800).floor.d         ==== ooes(1468800) --: typed[OffsetDateTime]
    T ~ ooes(1555199, n9).floor.d     ==== ooes(1468800)
    T ~ ooes(1468800, 1).floor.d      ==== ooes(1468800)
    T ~ ooes(-259200).floor.d         ==== ooes(-259200)
    T ~ ooes(-172801, n9).floor.d     ==== ooes(-259200)
    T ~ ooes(-259200, 1).floor.d      ==== ooes(-259200)
    T ~ ooes(1468800).round.d         ==== ooes(1468800) --: typed[OffsetDateTime]
    T ~ ooes(1512000).round.d         ==== ooes(1468800)
    T ~ ooes(1512000, 1).round.d      ==== ooes(1555200)
    T ~ ooes(1512001).round.d         ==== ooes(1555200)
    T ~ ooes(-259200).round.d         ==== ooes(-259200)
    T ~ ooes(-216000).round.d         ==== ooes(-259200)
    T ~ ooes(-216000, 1).round.d      ==== ooes(-172800)
    T ~ ooes(-215999).round.d         ==== ooes(-172800)
    T ~ ooes(1468800).ceil.d          ==== ooes(1468800) --: typed[OffsetDateTime]
    T ~ ooes(1468800, 1).ceil.d       ==== ooes(1555200)
    T ~ ooes(1468801).ceil.d          ==== ooes(1555200)
    T ~ ooes(-259200).ceil.d          ==== ooes(-259200)
    T ~ ooes(-259200, 1).ceil.d       ==== ooes(-172800)
    T ~ ooes(-259199).ceil.d          ==== ooes(-172800)
    T ~ o.round.days ==== o.round.d --: typed[OffsetDateTime]
    T ~ o.floor.days ==== o.floor.d --: typed[OffsetDateTime]
    T ~ o.ceil.days  ==== o.ceil.d  --: typed[OffsetDateTime]
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).round.us ==== LocalDateTime.MAX.atOffset(o.getOffset).minus(999.ns)
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).round.ms ==== LocalDateTime.MAX.atOffset(o.getOffset).minus(999999.ns)
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).round.s  ==== LocalDateTime.MAX.atOffset(o.getOffset).minus(999999999.ns)
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).round.m  ==== LocalDateTime.MAX.atOffset(o.getOffset).minus(59.s + 999999999.ns)
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).round.h  ==== LocalDateTime.MAX.atOffset(o.getOffset).minus(59.m + 59.s + 999999999.ns)
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).round.d  ==== LocalDateTime.MAX.atOffset(o.getOffset).minus(23.h + 59.m + 59.s + 999999999.ns)
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).ceil.us  ==== LocalDateTime.MAX.atOffset(o.getOffset).round.us
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).ceil.ms  ==== LocalDateTime.MAX.atOffset(o.getOffset).round.ms
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).ceil.s   ==== LocalDateTime.MAX.atOffset(o.getOffset).round.s 
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).ceil.m   ==== LocalDateTime.MAX.atOffset(o.getOffset).round.m 
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).ceil.h   ==== LocalDateTime.MAX.atOffset(o.getOffset).round.h 
    T ~ LocalDateTime.MAX.atOffset(o.getOffset).ceil.d   ==== LocalDateTime.MAX.atOffset(o.getOffset).round.d


  def testZonedDateTime(): Unit =
    val z = ZonedDateTime.now
    val idt = Instant.now
    val ldt = LocalDateTime.now
    val odt = OffsetDateTime.now
    val udt = odt.withOffsetSameInstant(ZoneOffset.UTC)
    val gig = Instant.ofEpochSecond(1000000000).zoned
    val gigu = gig.withZoneSameInstant(ZoneId of "UTC")
    val goz = gig.getZone
    val giggle = Instant.ofEpochSecond(1000000001, 234567890).zoned
    val big = gig to gig.MaxValue
    val bigger = giggle.MinValue to giggle
    T ~ (gig + 1.s)     ==== Instant.ofEpochSecond(1000000001).zoned --: typed[ZonedDateTime]
    T ~ (giggle + big)  ==== giggle.MaxValue
    T ~ (gig +! 1.s)    ==== (gig + 1.s) --: typed[ZonedDateTime]
    T ~ (giggle +! big) ==== thrown[DateTimeException]
    T ~ (gig - 1.s)     ==== Instant.ofEpochSecond(999999999).zoned  --: typed[ZonedDateTime]
    T ~ (gig - bigger)  ==== gig.MinValue
    T ~ (gig -! 1.s)    ==== (gig - 1.s)                       --: typed[ZonedDateTime]
    T ~ (gig -! bigger) ==== thrown[DateTimeException]
    T ~ (giggle - gig)  ==== 1234567890.ns                     --: typed[Duration]
    T ~ (giggle to gig) ==== -1234567890.ns                    --: typed[Duration]
    T ~ (gig < giggle)  ==== true
    T ~ (gig < gig)     ==== false
    T ~ (giggle < gig)  ==== false
    T ~ (gig <= giggle) ==== true
    T ~ (gig <= gig)    ==== true
    T ~ (giggle <= gig) ==== false
    T ~ (gig >= giggle) ==== false
    T ~ (gig >= gig)    ==== true
    T ~ (giggle >= gig) ==== true
    T ~ (gig > giggle)  ==== false
    T ~ (gig > gig)     ==== false 
    T ~ (giggle > gig)  ==== true
    T ~ (gig max giggle)                 ==== giggle      --: typed[ZonedDateTime]
    T ~ (giggle max gig)                 ==== giggle
    T ~ (gig min giggle)                 ==== gig         --: typed[ZonedDateTime]
    T ~ (giggle min gig)                 ==== gig
    T ~ (gig + 1.s).clamp(gig, giggle)   ==== (gig + 1.s) --: typed[ZonedDateTime]
    T ~ gig.clamp(gig + 1.s, giggle)     ==== (gig + 1.s)
    T ~ giggle.clamp(gig, gig + 1.s)     ==== (gig + 1.s)
    T ~ (gig + 1.s).clamp(giggle, gig)   ==== giggle
    T ~ (gig + 1.s).in(gig, giggle)      ==== true
    T ~ gig.in(gig + 1.s, giggle)        ==== false
    T ~ giggle.in(gig, gig + 1.s)        ==== false
    T ~ (gig + 1.s).checkIn(gig, giggle) ==== (gig + 1.s) --: typed[ZonedDateTime]
    T ~ gig.checkIn(gig + 1.s, giggle)   ==== thrown[DateTimeException]
    T ~ giggle.checkIn(gig, gig + 1.s)   ==== thrown[DateTimeException]
    T ~ gig.D                            ==== 1e9 --: typed[DoubleInstant]
    T ~ gig.filetime                     ==== FileTime.fromMillis(1000000000000L) --: typed[FileTime]
    T ~ gig.instant                      ==== typed[Instant]
    T ~ (idt - z.instant < 1.m)          ==== true
    T ~ gig.local                        ==== typed[LocalDateTime]
    T ~ (ldt - z.local < 1.m)            ==== true
    T ~ gig.utc                          ==== typed[OffsetDateTime]
    T ~ (udt - z.utc < 1.m)              ==== true
    T ~ gig.checkedUTC                   ==== gig.utc   --: typed[OffsetDateTime]
    T ~ gigu.zoned                       ==== gig       --: typed[ZonedDateTime]
    T ~ gigu.zoned.getZone               ==== gig.getZone 
    T ~ gigu.checkedZoned                ==== gig       --: typed[ZonedDateTime]
    T ~ gigu.checkedZoned.getZone        ==== gig.getZone 
    T ~ (odt - z.offset < 1.m)           ==== true
    T ~ gig.checkedZoned                 ==== gig.zoned --: typed[ZonedDateTime]

    if z.getOffset.getTotalSeconds < 0 then
      val farZone = ZoneId of "Australia/Adelaide"
      val f = z withZoneSameInstant farZone
      T ~ f.MinValue.local         ==== LocalDateTime.MIN
      T ~ f.MinValue.checkedLocal  ==== thrown[DateTimeException]
      T ~ f.MinValue.offset        ==== LocalDateTime.MIN.atOffset(ZoneId.systemDefault.getRules.getOffset(LocalDateTime.MIN))
      T ~ f.MinValue.checkedOffset ==== thrown[DateTimeException]
      T ~ f.MinValue.utc           ==== LocalDateTime.MIN.atOffset(ZoneOffset.UTC)
      T ~ f.MinValue.checkedUTC    ==== thrown[DateTimeException]
      T ~ f.MinValue.zoned         ==== LocalDateTime.MIN.atZone(z.getZone)
      T ~ f.MinValue.checkedZoned  ==== thrown[DateTimeException]
    else
      val farZone = ZoneId of "America/Anchorage"
      val f = z withZoneSameInstant farZone
      T ~ f.MaxValue.local         ==== LocalDateTime.MAX
      T ~ f.MaxValue.checkedLocal  ==== thrown[DateTimeException]
      T ~ f.MaxValue.offset        ==== LocalDateTime.MAX.atOffset(ZoneId.systemDefault.getRules.getOffset(LocalDateTime.MAX))
      T ~ f.MaxValue.checkedOffset ==== thrown[DateTimeException]
      T ~ f.MaxValue.utc           ==== LocalDateTime.MAX.atOffset(ZoneOffset.UTC)
      T ~ f.MaxValue.checkedUTC    ==== thrown[DateTimeException]
      T ~ f.MaxValue.zoned         ==== LocalDateTime.MAX.atZone(z.getZone)
      T ~ f.MaxValue.checkedZoned  ==== thrown[DateTimeException]

    val n9 = 999999999
    def zoes(s: Long, nano: Int = 0): ZonedDateTime =
      val i = Instant.ofEpochSecond(s, nano)
      val shift = ZoneId.systemDefault.getRules.getOffset(i).getTotalSeconds
      (i - shift.s).zoned
    T ~ zoes(100, 3000).floor.us      ==== zoes(100, 3000) --: typed[ZonedDateTime]
    T ~ zoes(100, 3999).floor.us      ==== zoes(100, 3000)
    T ~ zoes(-10, 3000).floor.us      ==== zoes(-10, 3000)
    T ~ zoes(-10, 3999).floor.us      ==== zoes(-10, 3000)
    T ~ zoes(100, 3000).round.us      ==== zoes(100, 3000) --: typed[ZonedDateTime]
    T ~ zoes(100, 3500).round.us      ==== zoes(100, 3000)
    T ~ zoes(100, 3501).round.us      ==== zoes(100, 4000)
    T ~ zoes(100, 999999501).round.us ==== zoes(101)
    T ~ zoes(-10, 3000).round.us      ==== zoes(-10, 3000)
    T ~ zoes(-10, 3500).round.us      ==== zoes(-10, 3000)
    T ~ zoes(-10, 3501).round.us      ==== zoes(-10, 4000)
    T ~ zoes(-10, 999999501).round.us ==== zoes( -9)
    T ~ zoes(100, 3000).ceil.us       ==== zoes(100, 3000) --: typed[ZonedDateTime]
    T ~ zoes(100, 3001).ceil.us       ==== zoes(100, 4000)
    T ~ zoes(100, 999999001).ceil.us  ==== zoes(101)
    T ~ zoes(-10, 3000).ceil.us       ==== zoes(-10, 3000)
    T ~ zoes(-10, 3001).ceil.us       ==== zoes(-10, 4000)
    T ~ zoes(-10, 999999001).ceil.us  ==== zoes( -9)
    T ~ zoes(100, 3000000).floor.ms   ==== zoes(100, 3000000) --: typed[ZonedDateTime]
    T ~ zoes(100, 3999999).floor.ms   ==== zoes(100, 3000000)
    T ~ zoes(-10, 3000000).floor.ms   ==== zoes(-10, 3000000)
    T ~ zoes(-10, 3999999).floor.ms   ==== zoes(-10, 3000000)
    T ~ zoes(100, 3000000).round.ms   ==== zoes(100, 3000000) --: typed[ZonedDateTime]
    T ~ zoes(100, 3500000).round.ms   ==== zoes(100, 3000000)
    T ~ zoes(100, 3500001).round.ms   ==== zoes(100, 4000000)
    T ~ zoes(100, 999500001).round.ms ==== zoes(101)
    T ~ zoes(-10, 3000000).round.ms   ==== zoes(-10, 3000000)
    T ~ zoes(-10, 3500000).round.ms   ==== zoes(-10, 3000000)
    T ~ zoes(-10, 3500001).round.ms   ==== zoes(-10, 4000000)
    T ~ zoes(-10, 999500001).round.ms ==== zoes( -9)
    T ~ zoes(100, 3000000).ceil.ms    ==== zoes(100, 3000000) --: typed[ZonedDateTime]
    T ~ zoes(100, 3000001).ceil.ms    ==== zoes(100, 4000000)
    T ~ zoes(100, 999000001).ceil.ms  ==== zoes(101)
    T ~ zoes(-10, 3000000).ceil.ms    ==== zoes(-10, 3000000)
    T ~ zoes(-10, 3000001).ceil.ms    ==== zoes(-10, 4000000)
    T ~ zoes(-10, 999000001).ceil.ms  ==== zoes( -9)
    T ~ zoes(100).floor.s             ==== zoes(100) --: typed[ZonedDateTime]
    T ~ zoes(100, n9).floor.s         ==== zoes(100)
    T ~ zoes(-10).floor.s             ==== zoes(-10)
    T ~ zoes(-10, n9).floor.s         ==== zoes(-10)
    T ~ zoes(100).round.s             ==== zoes(100) --: typed[ZonedDateTime]
    T ~ zoes(100, 500000000).round.s  ==== zoes(100)
    T ~ zoes(100, 500000001).round.s  ==== zoes(101)
    T ~ zoes(-10).round.s             ==== zoes(-10)
    T ~ zoes(-10, 500000000).round.s  ==== zoes(-10)
    T ~ zoes(-10, 500000001).round.s  ==== zoes( -9)
    T ~ zoes(100).ceil.s              ==== zoes(100) --: typed[ZonedDateTime]
    T ~ zoes(100, 1).ceil.s           ==== zoes(101)
    T ~ zoes(-10).ceil.s              ==== zoes(-10)
    T ~ zoes(-10, 1).ceil.s           ==== zoes( -9)
    T ~ zoes(6120).floor.m            ==== zoes(6120) --: typed[ZonedDateTime]
    T ~ zoes(6179, n9).floor.m        ==== zoes(6120)
    T ~ zoes(6120, 1).floor.m         ==== zoes(6120)
    T ~ zoes(-180).floor.m            ==== zoes(-180)
    T ~ zoes(-121, n9).floor.m        ==== zoes(-180)
    T ~ zoes(-180, 1).floor.m         ==== zoes(-180)
    T ~ zoes(6120).round.m            ==== zoes(6120) --: typed[ZonedDateTime]
    T ~ zoes(6150).round.m            ==== zoes(6120)
    T ~ zoes(6150, 1).round.m         ==== zoes(6180)
    T ~ zoes(6151).round.m            ==== zoes(6180)
    T ~ zoes(-180).round.m            ==== zoes(-180)
    T ~ zoes(-150).round.m            ==== zoes(-180)
    T ~ zoes(-150, 1).round.m         ==== zoes(-120)
    T ~ zoes(-149).round.m            ==== zoes(-120)
    T ~ zoes(6120).ceil.m             ==== zoes(6120) --: typed[ZonedDateTime]
    T ~ zoes(6120, 1).ceil.m          ==== zoes(6180)
    T ~ zoes(6121).ceil.m             ==== zoes(6180)
    T ~ zoes(-180).ceil.m             ==== zoes(-180)
    T ~ zoes(-180, 1).ceil.m          ==== zoes(-120)
    T ~ zoes(-179).ceil.m             ==== zoes(-120)
    T ~ zoes(612000).floor.h          ==== zoes(612000) --: typed[ZonedDateTime]
    T ~ zoes(615599, n9).floor.h      ==== zoes(612000)
    T ~ zoes(612000, 1).floor.h       ==== zoes(612000)
    T ~ zoes(-14400).floor.h          ==== zoes(-14400)
    T ~ zoes(-10801, n9).floor.h      ==== zoes(-14400)
    T ~ zoes(-14400, 1).floor.h       ==== zoes(-14400)
    T ~ zoes(612000).round.h          ==== zoes(612000) --: typed[ZonedDateTime]
    T ~ zoes(613800).round.h          ==== zoes(612000)
    T ~ zoes(613800, 1).round.h       ==== zoes(615600)
    T ~ zoes(613801).round.h          ==== zoes(615600)
    T ~ zoes(-14400).round.h          ==== zoes(-14400)
    T ~ zoes(-12600).round.h          ==== zoes(-14400)
    T ~ zoes(-12600, 1).round.h       ==== zoes(-10800)
    T ~ zoes(-12599).round.h          ==== zoes(-10800)
    T ~ zoes(612000).ceil.h           ==== zoes(612000) --: typed[ZonedDateTime]
    T ~ zoes(612000, 1).ceil.h        ==== zoes(615600)
    T ~ zoes(612001).ceil.h           ==== zoes(615600)
    T ~ zoes(-14400).ceil.h           ==== zoes(-14400)
    T ~ zoes(-14400, 1).ceil.h        ==== zoes(-10800)
    T ~ zoes(-14399).ceil.h           ==== zoes(-10800)
    T ~ zoes(1468800).floor.d         ==== zoes(1468800) --: typed[ZonedDateTime]
    T ~ zoes(1555199, n9).floor.d     ==== zoes(1468800)
    T ~ zoes(1468800, 1).floor.d      ==== zoes(1468800)
    T ~ zoes(-259200).floor.d         ==== zoes(-259200)
    T ~ zoes(-172801, n9).floor.d     ==== zoes(-259200)
    T ~ zoes(-259200, 1).floor.d      ==== zoes(-259200)
    T ~ zoes(1468800).round.d         ==== zoes(1468800) --: typed[ZonedDateTime]
    T ~ zoes(1512000).round.d         ==== zoes(1468800)
    T ~ zoes(1512000, 1).round.d      ==== zoes(1555200)
    T ~ zoes(1512001).round.d         ==== zoes(1555200)
    T ~ zoes(-259200).round.d         ==== zoes(-259200)
    T ~ zoes(-216000).round.d         ==== zoes(-259200)
    T ~ zoes(-216000, 1).round.d      ==== zoes(-172800)
    T ~ zoes(-215999).round.d         ==== zoes(-172800)
    T ~ zoes(1468800).ceil.d          ==== zoes(1468800) --: typed[ZonedDateTime]
    T ~ zoes(1468800, 1).ceil.d       ==== zoes(1555200)
    T ~ zoes(1468801).ceil.d          ==== zoes(1555200)
    T ~ zoes(-259200).ceil.d          ==== zoes(-259200)
    T ~ zoes(-259200, 1).ceil.d       ==== zoes(-172800)
    T ~ zoes(-259199).ceil.d          ==== zoes(-172800)
    T ~ z.round.days ==== z.round.d --: typed[ZonedDateTime]
    T ~ z.floor.days ==== z.floor.d --: typed[ZonedDateTime]
    T ~ z.ceil.days  ==== z.ceil.d  --: typed[ZonedDateTime]
    T ~ LocalDateTime.MAX.atZone(z.getZone).round.us ==== LocalDateTime.MAX.atZone(z.getZone).minus(999.ns)
    T ~ LocalDateTime.MAX.atZone(z.getZone).round.ms ==== LocalDateTime.MAX.atZone(z.getZone).minus(999999.ns)
    T ~ LocalDateTime.MAX.atZone(z.getZone).round.s  ==== LocalDateTime.MAX.atZone(z.getZone).minus(999999999.ns)
    T ~ LocalDateTime.MAX.atZone(z.getZone).round.m  ==== LocalDateTime.MAX.atZone(z.getZone).minus(59.s + 999999999.ns)
    T ~ LocalDateTime.MAX.atZone(z.getZone).round.h  ==== LocalDateTime.MAX.atZone(z.getZone).minus(59.m + 59.s + 999999999.ns)
    T ~ LocalDateTime.MAX.atZone(z.getZone).round.d  ==== LocalDateTime.MAX.atZone(z.getZone).minus(23.h + 59.m + 59.s + 999999999.ns)
    T ~ LocalDateTime.MAX.atZone(z.getZone).ceil.us  ==== LocalDateTime.MAX.atZone(z.getZone).round.us
    T ~ LocalDateTime.MAX.atZone(z.getZone).ceil.ms  ==== LocalDateTime.MAX.atZone(z.getZone).round.ms
    T ~ LocalDateTime.MAX.atZone(z.getZone).ceil.s   ==== LocalDateTime.MAX.atZone(z.getZone).round.s 
    T ~ LocalDateTime.MAX.atZone(z.getZone).ceil.m   ==== LocalDateTime.MAX.atZone(z.getZone).round.m 
    T ~ LocalDateTime.MAX.atZone(z.getZone).ceil.h   ==== LocalDateTime.MAX.atZone(z.getZone).round.h 
    T ~ LocalDateTime.MAX.atZone(z.getZone).ceil.d   ==== LocalDateTime.MAX.atZone(z.getZone).round.d

    val hobart = ZoneId of "Australia/Hobart"
    val weird = Instant.ofEpochSecond(-2345795357L) atZone hobart
    val ordinary = Instant.ofEpochSecond(-2345795355L) atZone hobart
    T ~ weird.getMinute    ==== 59
    T ~ ordinary.getMinute ==== 10
    T ~ (weird + 2.s)      ==== ordinary
    T ~ (ordinary - weird) ==== 2.s
    T ~ ordinary.round.d   ==== (weird - 86399.s)
    T ~ LocalDateTime.MIN.atZone(hobart).floor.m ==== LocalDateTime.MIN.atZone(hobart).ceil.m
}

