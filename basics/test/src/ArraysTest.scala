// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-24 Rex Kerr and Calico Life Sciences, LLC.

package kse.test.basics


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable
import scala.util.boundary
import scala.util.boundary.break

import sourcecode.{Line, given}


class ArraysTest() {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.basics.{_, given}
  import kse.basics.intervals._

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  object C extends NewType[Char] {
    extension (c: C.Type)
      def n: Int = c.value.toInt
      def l: Boolean = c.value.isLetter
      def o: O.Type = O(Some(c.value.toString))
      def ^(i: Int) = (c.value + i).toChar
  }
  extension (ac: Array[C.Type])
    def cs: String =
      MkStr: sb =>
        ac.use()(sb += _.value)

  object O extends NewType[Option[String]] {
    extension (o: O.Type)
      def n: Int = o.value.map(_.length).getOrElse(-1)
      def l: Boolean = o.value.isDefined
      def c: C.Type = C(o.value.map(s => if s.length > 0 then s(s.length - 1) else '$').getOrElse('#'))
  }
  extension (ao: Array[O.Type])
    def os: String =
      MkStr: sb =>
        ao.use()(o => sb += (if o.asInstanceOf[AnyRef] eq null then "@" else o.value.map(_+".").getOrElse("#")))
  
  extension (s: String)
    def c: Array[C.Type] = s.toCharArray.copyWith(c => C(c))

  def arrayInlinedDataTest(): Unit =
    var cuml = 0
    val str = "ch.ix.#n."
    val car = str.c
    val oar = Array[O.Type](O(Some("ch")), O(Some("ix")), O(None), O(Some("n")))
    val ix = Array(2, 3, 1, 1, 3)
    val ex = Array(2, -9, 400, 3)

    val civ = Iv(3, 5)
    val oiv = Iv(1, 3)
    val cpv = 3 to End-4
    val opv = 1 to End-1

    def st = ix.stepper
    def et = ex.stepper

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f: Unit
      cuml

    T ~ car.cs ==== str
    T ~ oar.os ==== str

    T ~ car(End).value   ==== '.'
    T ~ oar(End).value   ==== Some("n")
    T ~ car(End-1).value ==== 'n'
    T ~ oar(End-1).value ==== None

    T ~ End.of(car) ==== car.length - 1
    T ~ End.of(str) ==== str.length - 1

    T ~ (End+1).of("salmon") ==== 6
    T ~ (End+1).of(ix)       ==== 5
    T ~ (End+1).of(civ)      ==== 5
    T ~ (Start-1).of(civ)    ==== 2

    T ~ (Start + 1 to End + 3).of(civ) ==== Iv(4, 8)

    T ~ (car.peek( 2){ cuml += _.n } eq car) ==== true
    T ~ (car.peek( 9){ cuml += _.n } eq car) ==== true
    T ~ (car.peek(-1){ cuml += _.n } eq car) ==== true
    T ~ { val x = cuml; cuml = 0; x }        ==== '.'.toInt
    T ~ car.dup().poke( 2)(_ => C('^')).cs   ==== "ch^ix.#n."
    T ~ car.dup().poke(-1)(_ => C('^')).cs   ==== str

    T ~ z{ car.tap(_.use()(cuml += _.n)) }.cs       ==== str
    T ~ cuml                                        ==== str.map(_.toInt).sum
    T ~ z{ oar.tap(_.use()(cuml += _.n)) }.os       ==== str
    T ~ cuml                                        ==== oar.map(_.n).sum
    T ~ z{ car.tap(_.use(3, 5)(cuml += _.n)) }.cs   ==== str
    T ~ cuml                                        ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ oar.tap(_.use(1, 3)(cuml += _.n)) }.os   ==== str
    T ~ cuml                                        ==== oar.slice(1, 3).map(_.n).sum
    T ~ z{ car.tap(_.use(civ)(cuml += _.n)) }.cs    ==== str
    T ~ cuml                                        ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ oar.tap(_.use(oiv)(cuml += _.n)) }.os    ==== str
    T ~ cuml                                        ==== oar.slice(1, 3).map(_.n).sum
    T ~ z{ car.tap(_.use(3 to 4)(cuml += _.n)) }.cs ==== str
    T ~ cuml                                        ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ oar.tap(_.use(1 to 2)(cuml += _.n)) }.os ==== str
    T ~ cuml                                        ==== oar.slice(1, 3).map(_.n).sum
    T ~ z{ car.tap(_.use(cpv)(cuml += _.n)) }.cs    ==== str
    T ~ cuml                                        ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ oar.tap(_.use(opv)(cuml += _.n)) }.os    ==== str
    T ~ cuml                                        ==== oar.slice(1, 3).map(_.n).sum
    T ~ z{ car.tap(_.use(ix)(cuml += _.n)) }.cs     ==== str
    T ~ cuml                                        ==== ".ihhi".map(_.toInt).sum
    T ~ z{ oar.tap(_.use(ix)(cuml += _.n)) }.os     ==== str
    T ~ cuml                                        ==== ix.map(i => oar(i).n).sum
    T ~ z{ car.tap(_.use(st)(cuml += _.n)) }.cs     ==== str
    T ~ cuml                                        ==== ".ihhi".map(_.toInt).sum
    T ~ z{ oar.tap(_.use(st)(cuml += _.n)) }.os     ==== str
    T ~ cuml                                        ==== ix.map(i => oar(i).n).sum

    T ~ car.dup().tap(_.alter()(      c => if c.l then C(c.value.toUpper) else c)).cs ==== str.toUpperCase
    T ~ car.dup().tap(_.alter(3, 5)(  c => if c.l then C(c.value.toUpper) else c)).cs ==== "ch.IX.#n."
    T ~ car.dup().tap(_.alter(3 to 4)(c => if c.l then C(c.value.toUpper) else c)).cs ==== "ch.IX.#n."
    T ~ car.dup().tap(_.alter(civ)(   c => if c.l then C(c.value.toUpper) else c)).cs ==== "ch.IX.#n."
    T ~ car.dup().tap(_.alter(cpv)(   c => if c.l then C(c.value.toUpper) else c)).cs ==== "ch.IX.#n."
    T ~ car.dup().tap(_.alter(ix)(    c => if c.l then C(c.value.toUpper) else c)).cs ==== "cH.Ix.#n."
    T ~ car.dup().tap(_.alter(st)(    c => if c.l then C(c.value.toUpper) else c)).cs ==== "cH.Ix.#n."

    T ~ n{ car.visit()(cuml += _.n + _) }       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ n{ oar.visit()(cuml += _.n + _) }       ==== oar.map(_.n).sum + oar.length*(oar.length-1)/2
    T ~ n{ car.visit(3, 5)(cuml += _.n + _) }   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ oar.visit(1, 3)(cuml += _.n + _) }   ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ n{ car.visit(civ)(cuml += _.n + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ oar.visit(oiv)(cuml += _.n + _) }    ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ n{ car.visit(3 to 4)(cuml += _.n + _) } ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ oar.visit(1 to 2)(cuml += _.n + _) } ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ n{ car.visit(cpv)(cuml += _.n + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ oar.visit(opv)(cuml += _.n + _) }    ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ n{ car.visit(ix)(cuml += _.n + _) }     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ n{ oar.visit(ix)(cuml += _.n + _) }     ==== ix.map(i => oar(i).n).sum + ix.sum
    T ~ n{ car.visit(st)(cuml += _.n + _) }     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ n{ oar.visit(st)(cuml += _.n + _) }     ==== ix.map(i => oar(i).n).sum + ix.sum

    T ~ n{ car.pairs((x, y) => if x.l && y.l then cuml += 1) }                   ==== 2
    T ~ n{ oar.pairs((x, y) => if x.l != y.l then cuml += 1) }                   ==== 2
    T ~ n{ car.trios((x, y, z) => if x.l && !y.l && z.l then cuml += 1 ) }       ==== 1
    T ~ n{ oar.trios((x, y, z) => if x.l != y.l && y.l != z.l then cuml += 1 ) } ==== 1

    T ~ n{ car.together(oar)((c, o, i) => if c.l && o.n > 1 then cuml += 1+i) }     ==== 3
    T ~ n{ car.together(str)((c, s, i) => if c.l && s > 'c' then cuml += 1+i) }     ==== 19
    T ~ n{ oar.together(car)((o, c, i) => if o.n > 1 && c.l then cuml += 1+i) }     ==== 3
    T ~ n{ oar.together(str)((o, s, i) => if o.n > 1 && s > 'c' then cuml += 1+i) } ==== 2
    T ~ n{ car.together(oar, car)((c, o, d, i) => if c.l && o.n > 1 && d.n > 'c' then cuml += 1+i) } ==== 2
    T ~ n{ car.together(oar, str)((c, o, s, i) => if c.l && o.n > 1 && s > 'c' then cuml += 1+i) }   ==== 2
    T ~ n{ car.together(car, oar)((c, d, o, i) => if c.l && o.n > 1 && d.n > 'c' then cuml += 1+i) } ==== 2
    T ~ n{ car.together(str, oar)((c, s, o, i) => if c.l && o.n > 1 && s > 'c' then cuml += 1+i) }   ==== 2
    T ~ n{ car.together(str, str)((c, s, t, i) => if c.l && s > 'c' && s < 'x' then cuml += 1+i) }   ==== 14
    T ~ n{ oar.together(car, car)((o, c, d, i) => if o.l && c.l && d.n != 'h' then cuml += 1+i) }    ==== 5
    T ~ n{ oar.together(car, str)((o, c, s, i) => if o.l && c.l && s != 'h' then cuml += 1+i) }      ==== 5
    T ~ n{ oar.together(str, car)((o, s, c, i) => if o.l && c.l && s != 'h' then cuml += 1+i) }      ==== 5
    T ~ n{ oar.together(str, str)((o, s, t, i) => if o.l && s != '.' && t != 'h' then cuml += 1+i) } ==== 5

    T ~ n{ car.wander(){  (c, i) => cuml += c.n; i+2 } } ==== str.grouped(2).map(_(0).toInt).sum
    T ~ n{ oar.wander(){  (o, i) => cuml += o.n; i+2 } } ==== Array(0, 2).map(i => oar(i).n).sum
    T ~ n{ car.wander(1){ (c, i) => cuml += c.n; i+2 } } ==== str.grouped(2).filter(_.length == 2).map(_(1).toInt).sum
    T ~ n{ oar.wander(1){ (o, i) => cuml += o.n; i+2 } } ==== Array(1, 3).map(i => oar(i).n).sum
    T ~    car.wander(){  (_, i) =>              i+2 }   ==== str.grouped(2).map(_(0).toInt).length
    T ~    oar.wander(){  (_, i) =>              i+2 }   ==== Array(0, 2).map(i => oar(i).n).length
    T ~    car.wander(1){ (_, i) =>              i+2 }   ==== str.grouped(2).filter(_.length == 2).map(_(1).toInt).length
    T ~    oar.wander(1){ (_, i) =>              i+2 }   ==== Array(1, 3).map(i => oar(i).n).length

    T ~ car.gather(0)()(_ + _.n + _)       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ oar.gather(0)()(_ + _.n + _)       ==== oar.map(_.n).sum + oar.length*(oar.length-1)/2
    T ~ car.gather(0)(3, 5)(_ + _.n + _)   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ oar.gather(0)(1, 3)(_ + _.n + _)   ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ car.gather(0)(civ)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ oar.gather(0)(oiv)(_ + _.n + _)    ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ car.gather(0)(3 to 4)(_ + _.n + _) ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ oar.gather(0)(1 to 2)(_ + _.n + _) ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ car.gather(0)(cpv)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ oar.gather(0)(opv)(_ + _.n + _)    ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ car.gather(0)(ix)(_ + _.n + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ oar.gather(0)(ix)(_ + _.n + _)     ==== ix.map(i => oar(i).n).sum + ix.sum
    T ~ car.gather(0)(st)(_ + _.n + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ oar.gather(0)(st)(_ + _.n + _)     ==== ix.map(i => oar(i).n).sum + ix.sum

    T ~ car.dup()                  =**= car
    T ~ (car.dup() eq car)         ==== false
    T ~ oar.dup()                  =**= oar
    T ~ (oar.dup() eq oar)         ==== false
    T ~ car.dup(_(0) = C('s')).cs  ==== "sh.ix.#n."
    T ~ oar.dup(_(0) = O(None)).os ==== "#ix.#n."

    T ~ car.copyWith(_.n)   =**= car.map(_.n)
    T ~ oar.copyWith(_.n)   =**= oar.map(_.n)
    T ~ car.copyOp(_.n + _) =**= car.zipWithIndex.map{ case (x, i) => x.n + i }
    T ~ oar.copyOp(_.n + _) =**= oar.zipWithIndex.map{ case (x, i) => x.n + i }

    T ~ car.addLeft(2).cs          ==== "\u0000\u0000ch.ix.#n."
    T ~ oar.addLeft(2).os          ==== "@@ch.ix.#n."
    T ~ car.addLeft(2, C('+')).cs  ==== "++ch.ix.#n."
    T ~ oar.addLeft(2, O(None)).os ==== "##ch.ix.#n."
    T ~ car.addRight(2).cs          ==== "ch.ix.#n.\u0000\u0000"
    T ~ oar.addRight(2).os          ==== "ch.ix.#n.@@"
    T ~ car.addRight(2, C('+')).cs  ==== "ch.ix.#n.++"
    T ~ oar.addRight(2, O(None)).os ==== "ch.ix.#n.##"

    T ~ car.where()                         =**= car.indices.toArray
    T ~ car.where(_.l)                      =**= car.zipWithIndex.collect{ case (c, i) if c.l => i }
    T ~ oar.where(_.l)                      =**= oar.zipWithIndex.collect{ case (o, i) if o.l => i }
    T ~ car.whereIn(3, 5  )(_.value == 'x') =**= Array(4)
    T ~ oar.whereIn(1, 3  )(_.l)            =**= Array(1)
    T ~ car.whereIn(3 to 4)(_.value == 'x') =**= Array(4)
    T ~ oar.whereIn(1 to 2)(_.l)            =**= Array(1)
    T ~ car.whereIn(civ   )(_.value == 'x') =**= Array(4)
    T ~ oar.whereIn(oiv   )(_.l)            =**= Array(1)
    T ~ car.whereIn(cpv   )(_.value == 'x') =**= Array(4)
    T ~ oar.whereIn(opv   )(_.l)            =**= Array(1)
    T ~ car.whereFrom(ix)(_.l)              =**= Array(3, 1, 1, 3)
    T ~ oar.whereFrom(ix)(_.l)              =**= Array(3, 1, 1, 3)

    def linc(c: C.Type, i: Int) = if c.l then i+7 else -1
    T ~ car.whereOp(linc)                     =**= car.where(_.l).copyWith(_ + 7)
    T ~ car.whereInOp(1, 7)(linc)             =**= car.whereIn(1, 7)(_.l).copyWith(_ + 7)
    T ~ car.whereInOp(Iv(1, 7))(linc)         =**= car.whereIn(Iv(1, 7))(_.l).copyWith(_ + 7)
    T ~ car.whereInOp(1 to End-2)(linc)       =**= car.whereIn(1 to End-2)(_.l).copyWith(_ + 7)
    T ~ car.whereInOp(1 to 6)(linc)           =**= car.whereIn(1 to 6)(_.l).copyWith(_ + 7)    
    T ~ car.whereFromOp(Array(7, 5, 3))(linc) =**= car.whereFrom(Array(7, 5, 3))(_.l).copyWith(_ + 7)

    T ~ car.dup(_() = C('x')).cs          ==== "xxxxxxxxx"
    T ~ oar.dup(_() = O(None)).os         ==== "####"
    T ~ car.dup(_() = "abcdefghi".c).cs   ==== "abcdefghi"
    T ~ oar.dup(_() = oar.reverse)        =**= oar.reverse
    T ~ car.dup(_(civ) = C('x')).cs       ==== "ch.xx.#n."
    T ~ oar.dup(_(oiv) = O(None)).os      ==== "ch.##n."
    T ~ car.dup(_(civ) = "12".c).cs       ==== "ch.12.#n."
    T ~ oar.dup(_(oiv) = oar).os          ==== "ch.ch.ix.n."
    T ~ car.dup(_(3 to 4) = C('x')).cs    ==== "ch.xx.#n."
    T ~ oar.dup(_(1 to 2) = O(None)).os   ==== "ch.##n."
    T ~ car.dup(_(3 to 4) = "12".c).cs    ==== "ch.12.#n."
    T ~ oar.dup(_(1 to 2) = oar).os       ==== "ch.ch.ix.n."
    T ~ car.dup(_(cpv) = C('x')).cs       ==== "ch.xx.#n."
    T ~ oar.dup(_(opv) = O(None)).os      ==== "ch.##n."
    T ~ car.dup(_(cpv) = "12".c).cs       ==== "ch.12.#n."
    T ~ oar.dup(_(opv) = oar).os          ==== "ch.ch.ix.n."
    T ~ car.dup(_(ix) = C('x')).cs        ==== "cxxxx.#n."
    T ~ oar.dup(_(ix) = O(None)).os       ==== "ch.###"
    T ~ car.dup(_(st) = C('x')).cs        ==== "cxxxx.#n."
    T ~ oar.dup(_(st) = O(None)).os       ==== "ch.###"
    T ~ car.dup(_(_.l) = C('x')).cs       ==== "xx.xx.#x."
    T ~ oar.dup(_(_.l) = O(Some("e"))).os ==== "e.e.#e."

    inline def gc(run: (() => C.Type) => Array[C.Type]): String =
      var x = '0'
      val f = () => { x = (x+1).toChar; C(x) }
      run(f).cs
    inline def go(run: (() => O.Type) => Array[O.Type]): String =
      var x = ""
      val f = () => { x = x + "!"; O(Some(x)) }
      run(f).os
    inline def ic(run: (Int => C.Type) => Array[C.Type]): String =
      val f = (i: Int) => C(('0' + i).toChar)
      run(f).cs
    inline def io(run: (Int => O.Type) => Array[O.Type]): String =
      val f = (i: Int) => O(Some("!"*i))
      run(f).os
    T ~ gc{ f => car.dup(_.set()(f)) }       ==== "123456789"
    T ~ go{ f => oar.dup(_.set()(f)) }       ==== "!.!!.!!!.!!!!."
    T ~ ic{ f => car.dup(_.set()(f)) }       ==== "012345678"
    T ~ io{ f => oar.dup(_.set()(f)) }       ==== ".!.!!.!!!."
    T ~ gc{ f => car.dup(_.set(3, 5)(f)) }   ==== "ch.12.#n."
    T ~ go{ f => oar.dup(_.set(1, 3)(f)) }   ==== "ch.!.!!.n."
    T ~ ic{ f => car.dup(_.set(3, 5)(f)) }   ==== "ch.34.#n."
    T ~ io{ f => oar.dup(_.set(1, 3)(f)) }   ==== "ch.!.!!.n."
    T ~ gc{ f => car.dup(_.set(3 to 4)(f)) } ==== "ch.12.#n."
    T ~ go{ f => oar.dup(_.set(1 to 2)(f)) } ==== "ch.!.!!.n."
    T ~ ic{ f => car.dup(_.set(3 to 4)(f)) } ==== "ch.34.#n."
    T ~ io{ f => oar.dup(_.set(1 to 2)(f)) } ==== "ch.!.!!.n."
    T ~ gc{ f => car.dup(_.set(civ)(f)) }    ==== "ch.12.#n."
    T ~ go{ f => oar.dup(_.set(oiv)(f)) }    ==== "ch.!.!!.n."
    T ~ ic{ f => car.dup(_.set(civ)(f)) }    ==== "ch.34.#n."
    T ~ io{ f => oar.dup(_.set(oiv)(f)) }    ==== "ch.!.!!.n."
    T ~ gc{ f => car.dup(_.set(cpv)(f)) }    ==== "ch.12.#n."
    T ~ go{ f => oar.dup(_.set(opv)(f)) }    ==== "ch.!.!!.n."
    T ~ ic{ f => car.dup(_.set(cpv)(f)) }    ==== "ch.34.#n."
    T ~ io{ f => oar.dup(_.set(opv)(f)) }    ==== "ch.!.!!.n."
    T ~ gc{ f => car.dup(_.set(ix)(f)) }     ==== "c415x.#n."
    T ~ go{ f => oar.dup(_.set(ix)(f)) }     ==== "ch.!!!!.!.!!!!!."
    T ~ ic{ f => car.dup(_.set(ix)(f)) }     ==== "c123x.#n."
    T ~ io{ f => oar.dup(_.set(ix)(f)) }     ==== "ch.!.!!.!!!."
    T ~ gc{ f => car.dup(_.set(st)(f)) }     ==== "c415x.#n."
    T ~ go{ f => oar.dup(_.set(st)(f)) }     ==== "ch.!!!!.!.!!!!!."
    T ~ ic{ f => car.dup(_.set(st)(f)) }     ==== "c123x.#n."
    T ~ io{ f => oar.dup(_.set(st)(f)) }     ==== "ch.!.!!.!!!."
    T ~ gc{ f => car.dup(_.set(_.l)(f)) }    ==== "12.34.#5."
    T ~ go{ f => oar.dup(_.set(_.l)(f)) }    ==== "!.!!.#!!!."
    T ~ ic{ f => car.dup(_.set(_.l)(f)) }    ==== "01.34.#7."
    T ~ io{ f => oar.dup(_.set(_.l)(f)) }    ==== ".!.#!!!."

    inline def fc(run: ((C.Type, Int) => C.Type) => Array[C.Type]): String =
      val f = (c: C.Type, i: Int) => C(c ^ i)
      run(f).cs
    inline def fo(run: ((O.Type, Int) => O.Type) => Array[O.Type]): String =
      val f = (o: O.Type, i: Int) => O(o.value.map(_ + "!"*i) orElse Some(i.toString))
      run(f).os
    T ~ fc{ f => car.dup(_.edit()(f)) }       ==== "ci0l|3)u6"
    T ~ fo{ f => oar.dup(_.edit()(f)) }       ==== "ch.ix!.2.n!!!."
    T ~ fc{ f => car.dup(_.edit(3, 5)(f)) }   ==== "ch.l|.#n."
    T ~ fo{ f => oar.dup(_.edit(1, 3)(f)) }   ==== "ch.ix!.2.n."
    T ~ fc{ f => car.dup(_.edit(3 to 4)(f)) } ==== "ch.l|.#n."
    T ~ fo{ f => oar.dup(_.edit(1 to 2)(f)) } ==== "ch.ix!.2.n."
    T ~ fc{ f => car.dup(_.edit(civ)(f)) }    ==== "ch.l|.#n."
    T ~ fo{ f => oar.dup(_.edit(oiv)(f)) }    ==== "ch.ix!.2.n."
    T ~ fc{ f => car.dup(_.edit(cpv)(f)) }    ==== "ch.l|.#n."
    T ~ fo{ f => oar.dup(_.edit(opv)(f)) }    ==== "ch.ix!.2.n."
    T ~ fc{ f => car.dup(_.edit(ix)(f)) }     ==== "cj0ox.#n."
    T ~ fo{ f => oar.dup(_.edit(ix)(f)) }     ==== "ch.ix!!.2.n!!!!!!."
    T ~ fc{ f => car.dup(_.edit(st)(f)) }     ==== "cj0ox.#n."
    T ~ fo{ f => oar.dup(_.edit(st)(f)) }     ==== "ch.ix!!.2.n!!!!!!."

    val cx = "___________".c
    val ox = Array.fill(6)(O(Some("_")))
    var ninja = 0
    T ~ cx.dup(a => ninja = car.inject(a)).cs            ==== "ch.ix.#n.__"
    T ~ { val x = ninja; ninja = 0; x }                  ==== car.length
    T ~ ox.dup(a => ninja = oar.inject(a)).os            ==== "ch.ix.#n._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== oar.length
    T ~ cx.dup(a => ninja = car.inject(a, 2)).cs         ==== "__ch.ix.#n."
    T ~ { val x = ninja; ninja = 0; x }                  ==== car.length
    T ~ ox.dup(a => ninja = oar.inject(a, 2)).os         ==== "_._.ch.ix.#n."
    T ~ { val x = ninja; ninja = 0; x }                  ==== oar.length
    T ~ cx.dup(a => ninja = car.inject(a)(3, 5)).cs      ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(1, 3)).os      ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(3, 5)).cs   ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(1, 3)).os   ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(3 to 4)).cs    ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(1 to 2)).os    ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(3 to 4)).cs ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(1 to 2)).os ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(3, 5)).cs      ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(1, 3)).os      ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(3, 5)).cs   ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(1, 3)).os   ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(civ)).cs       ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(oiv)).os       ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(civ)).cs    ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(oiv)).os    ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(cpv)).cs       ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(opv)).os       ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(cpv)).cs    ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(opv)).os    ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(ix)).cs        ==== ".ihhi______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ ox.dup(a => ninja = oar.inject(a)(ix)).os        ==== "#n.ix.ix.n._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ cx.dup(a => ninja = car.inject(a, 1)(ix)).cs     ==== "_.ihhi_____"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ ox.dup(a => ninja = oar.inject(a, 1)(ix)).os     ==== "_.#n.ix.ix.n."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ cx.dup(a => ninja = car.inject(a)(st)).cs        ==== ".ihhi______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ ox.dup(a => ninja = oar.inject(a)(st)).os        ==== "#n.ix.ix.n._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ cx.dup(a => ninja = car.inject(a, 1)(st)).cs     ==== "_.ihhi_____"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ ox.dup(a => ninja = oar.inject(a, 1)(st)).os     ==== "_.#n.ix.ix.n."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ cx.dup(a => ninja = car.inject(a)(_.l)).cs       ==== "chixn______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== car.count(_.l)
    T ~ ox.dup(a => ninja = oar.inject(a)(_.l)).os       ==== "ch.ix.n._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== oar.count(_.l)
    T ~ cx.dup(a => ninja = car.inject(a, 2)(_.l)).cs    ==== "__chixn____"
    T ~ { val x = ninja; ninja = 0; x }                  ==== car.count(_.l)
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(_.l)).os    ==== "_._.ch.ix.n._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== oar.count(_.l)

    val ax = "___________".arr
    T ~ ax.dup(a => ninja = car.injectOp(a)()((c, i) => c ^ i)).str          ==== "ci0l|3)u6__"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== car.length
    T ~ ax.dup(a => ninja = car.injectOp(a, 2)()((c, i) => c ^ i)).str       ==== "__ci0l|3)u6"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== car.length
    T ~ ax.dup(a => ninja = car.injectOp(a)(3, 5)((c, i) => c ^ i)).str      ==== "l|_________"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a, 2)(3, 5)((c, i) => c ^ i)).str   ==== "__l|_______"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a)(3 to 4)((c, i) => c ^ i)).str    ==== "l|_________"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a, 2)(3 to 4)((c, i) => c ^ i)).str ==== "__l|_______"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a)(3, 5)((c, i) => c ^ i)).str      ==== "l|_________"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a, 2)(3, 5)((c, i) => c ^ i)).str   ==== "__l|_______"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a)(civ)((c, i) => c ^ i)).str       ==== "l|_________"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a, 2)(civ)((c, i) => c ^ i)).str    ==== "__l|_______"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a)(cpv)((c, i) => c ^ i)).str       ==== "l|_________"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a, 2)(cpv)((c, i) => c ^ i)).str    ==== "__l|_______"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 2
    T ~ ax.dup(a => ninja = car.injectOp(a)(ix)((c, i) => c ^ i)).str        ==== "0liil______"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 5
    T ~ ax.dup(a => ninja = car.injectOp(a, 1)(ix)((c, i) => c ^ i)).str     ==== "_0liil_____"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 5
    T ~ ax.dup(a => ninja = car.injectOp(a)(st)((c, i) => c ^ i)).str        ==== "0liil______"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 5
    T ~ ax.dup(a => ninja = car.injectOp(a, 1)(st)((c, i) => c ^ i)).str     ==== "_0liil_____"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== 5
    T ~ ax.dup(a => ninja = car.injectOp(a)(_.l)((c, i) => c ^ i)).str       ==== "cil|u______"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== car.count(_.l)
    T ~ ax.dup(a => ninja = car.injectOp(a, 2)(_.l)((c, i) => c ^ i)).str    ==== "__cil|u____"
    T ~ { val x = ninja; ninja = 0; x }                                      ==== car.count(_.l)

    T ~ car.select(3, 5).cs   ==== "ix"
    T ~ oar.select(1, 3).os   ==== "ix.#"
    T ~ car.select(3 to 4).cs ==== "ix"
    T ~ oar.select(1 to 2).os ==== "ix.#"
    T ~ car.select(civ).cs    ==== "ix"
    T ~ oar.select(oiv).os    ==== "ix.#"
    T ~ car.select(cpv).cs    ==== "ix"
    T ~ oar.select(opv).os    ==== "ix.#"
    T ~ car.select(ix).cs     ==== ".ihhi"
    T ~ oar.select(ix).os     ==== "#n.ix.ix.n."
    T ~ car.select(st).cs     ==== ".ihhi"
    T ~ oar.select(st).os     ==== "#n.ix.ix.n."
    T ~ car.select(_.l).cs    ==== "chixn"
    T ~ oar.select(_.l).os    ==== "ch.ix.n."

    T ~ car.selectOp(3, 5)(  (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "i.#"
    T ~ oar.selectOp(1, 3)(  (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "x-"
    T ~ car.selectOp(3 to 4)((c, i) => if i%2 == 0 then O(None) else c.o).os ==== "i.#"
    T ~ oar.selectOp(1 to 2)((o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "x-"
    T ~ car.selectOp(civ)(   (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "i.#"
    T ~ oar.selectOp(oiv)(   (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "x-"
    T ~ car.selectOp(cpv)(   (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "i.#"
    T ~ oar.selectOp(opv)(   (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "x-"
    T ~ car.selectOp(ix)(    (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "#i.h.h.i."
    T ~ oar.selectOp(ix)(    (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "-nxxn"
    T ~ car.selectOp(st)(    (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "#i.h.h.i."
    T ~ oar.selectOp(st)(    (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "-nxxn"
    T ~ car.selectOp(_.l)(   (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "#h.i.#n."
    T ~ oar.selectOp(_.l)(   (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "-xn"

    T ~ car.fuse[Int]((c, i, add) => if !c.l then add(i) else Array(c.o).os.foreach(c => add(c.toInt))) =**= Array(99, 46, 104, 46, 2, 105, 46, 120, 46, 5, 6, 110, 46, 8)

    val test = "cheesefactories".arr
    val tidx = test.where(_ == 'e')
    val qidx = Array(9, 4, 3, 3, 4)
    T ~ test.diced(tidx).map(_.str)                       =**= Array("ch", "s", "factori", "s")
    T ~ test.diced(tidx, "", "endpoints").map(_.str)      =**= Array("ch", "s", "factori", "s")
    T ~ test.diced(tidx, "no endpoints").map(_.str)       =**= Array("s", "factori")
    T ~ test.diced(tidx, "", "no endpoints").map(_.str)   =**= Array("s", "factori")
    T ~ test.diced(tidx, "()").map(_.str)                 =**= Array("ch", "", "s", "factori", "s")
    T ~ test.diced(tidx, "()", "endpoints").map(_.str)    =**= Array("ch", "", "s", "factori", "s")
    T ~ test.diced(tidx, "()", "no endpoints").map(_.str) =**= Array("", "s", "factori")
    T ~ test.diced(tidx, "(]").map(_.str)                 =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.diced(tidx, "(]", "endpoints").map(_.str)    =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.diced(tidx, "(]", "no endpoints").map(_.str) =**= Array("e", "se", "factorie")
    T ~ test.diced(tidx, "[)").map(_.str)                 =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.diced(tidx, "[)", "endpoints").map(_.str)    =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.diced(tidx, "[)", "no endpoints").map(_.str) =**= Array("e", "es", "efactori")
    T ~ test.diced(tidx, "[]").map(_.str)                 =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.diced(tidx, "[]", "endpoints").map(_.str)    =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.diced(tidx, "[]", "no endpoints").map(_.str) =**= Array("ee", "ese", "efactorie")
    T ~ test.diced(qidx).map(_.str)                       =**= Array("cheesefac", "cafe", "efactories")
    T ~ test.diced(qidx, "()").map(_.str)                 =**= Array("cheesefac", "cafe", "", "", "", "efactories")
    T ~ test.diced(qidx, "(]").map(_.str)                 =**= Array("cheesefact", "cafes", "e", "", "s", "efactories")
    T ~ test.diced(qidx, "[)").map(_.str)                 =**= Array("cheesefac", "tcafe", "s", "", "e", "sefactories")
    T ~ test.diced(qidx, "[]").map(_.str)                 =**= Array("cheesefact", "tcafes", "se", "e", "es", "sefactories")
    T ~ "".arr.diced(Array.empty[Int]).length             ==== 0
    T ~ "".arr.diced(Array.empty[Int], "()").map(_.str)   =**= Array("")
    T ~ "".arr.diced(Array.empty[Int], "(]").map(_.str)   =**= Array("")
    T ~ "".arr.diced(Array.empty[Int], "[)").map(_.str)   =**= Array("")
    T ~ "".arr.diced(Array.empty[Int], "[]").map(_.str)   =**= Array("")
    T ~ test.diced(_ == 'e').map(_.str)                   =**= Array("ch", "s", "factori", "s")
    T ~ test.diced(_ == 'e', "").map(_.str)               =**= Array("ch", "s", "factori", "s")
    T ~ test.diced(_ == 'e', "()").map(_.str)             =**= Array("ch", "", "s", "factori", "s")
    T ~ test.diced(_ == 'e', "(]").map(_.str)             =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.diced(_ == 'e', "[)").map(_.str)             =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.diced(_ == 'e', "[]").map(_.str)             =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ "".arr.diced(_ == 'e').length                     ==== 0
    T ~ "".arr.diced(_ == 'e', "()").map(_.str)           =**= Array("")
    T ~ "".arr.diced(_ == 'e', "(]").map(_.str)           =**= Array("")
    T ~ "".arr.diced(_ == 'e', "[)").map(_.str)           =**= Array("")
    T ~ "".arr.diced(_ == 'e', "[]").map(_.str)           =**= Array("")


  def arrayClippedInlinedDataTest(): Unit =
    var cuml = 0
    val str = "ch.#ik."
    val car = str.c

    val ix = Array(2, 3, 1, 1, 3)
    val ex = Array(2, 0, -1, 3, 6, 7, -9, 400) 
    def st = ix.stepper
    def et = ex.stepper

    val civ = Iv(3, 5)
    val cpv = 3 to End-2
    val eiv = Iv(3, 9)
    val fiv = Iv(-2, 5)
    val fpv = End-9 to End-2
    val biv = Iv(-2, 9)
    val niv = Iv(8, 9)
    val npv = End-9 to End-8

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f: Unit
      cuml

    T ~ car.clip(2)(C('!'))          ==== car(2)
    T ~ car.clip(-2)(C('!'))         ==== C('!')
    T ~ car.clip(car.length)(C('!')) ==== C('!')
    T ~ str.clip(2)('!')             ==== str(2)
    T ~ str.clip(-2)('!')            ==== '!'
    T ~ str.clip(str.length)('!')    ==== '!'
    T ~ car.clip.get(2)              ==== Some(car(2))
    T ~ car.clip.get(-2)             ==== None
    T ~ car.clip.get(car.length)     ==== None
    T ~ str.clip.get(2)              ==== Some(str(2))
    T ~ str.clip.get(-2)             ==== None
    T ~ str.clip.get(car.length)     ==== None

    T ~ z{ car.tap(_.clip.use(3, 5)(cuml += _.n)) }.cs   ==== str
    T ~ cuml                                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.clip.use(3 to 4)(cuml += _.n)) }.cs ==== str
    T ~ cuml                                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.clip.use(civ)(cuml += _.n)) }.cs    ==== str
    T ~ cuml                                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.clip.use(cpv)(cuml += _.n)) }.cs    ==== str
    T ~ cuml                                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.clip.use(ix)(cuml += _.n)) }.cs     ==== str
    T ~ cuml                                             ==== ".#hh#".map(_.toInt).sum
    T ~ z{ car.tap(_.clip.use(st)(cuml += _.n)) }.cs     ==== str
    T ~ cuml                                             ==== ".#hh#".map(_.toInt).sum

    T ~ n{ car.tap(_.clip.use(3, 9)(cuml += _.n)) }    ==== str.substring(3).map(_.toInt).sum
    T ~ n{ car.tap(_.clip.use(3 to 8)(cuml += _.n)) }  ==== str.substring(3).map(_.toInt).sum
    T ~ n{ car.tap(_.clip.use(eiv)(cuml += _.n)) }     ==== str.substring(3).map(_.toInt).sum
    T ~    car.tap(_.     use(3, 9)(cuml += _.n))      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(3 to 8)(cuml += _.n))    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(eiv)(cuml += _.n))       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.tap(_.clip.use(-2, 5)(cuml += _.n)) }   ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.tap(_.clip.use(-2 to 4)(cuml += _.n)) } ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.tap(_.clip.use(fiv)(cuml += _.n)) }     ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.tap(_.clip.use(fpv)(cuml += _.n)) }     ==== str.substring(0, 5).map(_.toInt).sum
    T ~    car.tap(_.     use(-2, 5)(cuml += _.n))     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(-2 to 4)(cuml += _.n))   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(fiv)(cuml += _.n))       ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(fpv)(cuml += _.n))       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.tap(_.clip.use(-2, 9)(cuml += _.n)) }   ==== str.map(_.toInt).sum
    T ~ n{ car.tap(_.clip.use(-2 to 9)(cuml += _.n)) } ==== str.map(_.toInt).sum
    T ~ n{ car.tap(_.clip.use(biv)(cuml += _.n)) }     ==== str.map(_.toInt).sum
    T ~    car.tap(_.     use(-2, 9)(cuml += _.n))     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(-2 to 9)(cuml += _.n))   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(biv)(cuml += _.n))       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.tap(_.clip.use(8, 9)(cuml += _.n)) }    ==== 0
    T ~ n{ car.tap(_.clip.use(8 to 9)(cuml += _.n)) }  ==== 0
    T ~ n{ car.tap(_.clip.use(niv)(cuml += _.n)) }     ==== 0
    T ~ n{ car.tap(_.clip.use(npv)(cuml += _.n)) }     ==== 0
    T ~    car.tap(_.     use(8, 9)(cuml += _.n))      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(8 to 9)(cuml += _.n))    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(niv)(cuml += _.n))       ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(npv)(cuml += _.n))       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.tap(_.clip.use(ex)(cuml += _.n)) }     ==== ".c#.".map(_.toInt).sum
    T ~ n{ car.tap(_.clip.use(et)(cuml += _.n)) }     ==== ".c#.".map(_.toInt).sum
    T ~    car.tap(_.     use(ex)(cuml += _.n))        ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.tap(_.     use(et)(cuml += _.n))        ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup().tap(_.clip.alter(3, 9   ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== "ch.#IK."
    T ~ car.dup().tap(_.clip.alter(3 to 8 ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== "ch.#IK."
    T ~ car.dup().tap(_.clip.alter(eiv    ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== "ch.#IK."
    T ~ car.dup().tap(_.clip.alter(-2, 5  ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== "CH.#Ik."
    T ~ car.dup().tap(_.clip.alter(-2 to 4){ c => if c.l then C(c.value.toUpper) else c }).cs ==== "CH.#Ik."
    T ~ car.dup().tap(_.clip.alter(fiv    ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== "CH.#Ik."
    T ~ car.dup().tap(_.clip.alter(fpv    ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== "CH.#Ik."
    T ~ car.dup().tap(_.clip.alter(8, 10  ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== str
    T ~ car.dup().tap(_.clip.alter(8 to 9 ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== str
    T ~ car.dup().tap(_.clip.alter(niv    ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== str
    T ~ car.dup().tap(_.clip.alter(npv    ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== str
    T ~ car.dup().tap(_.clip.alter(ex     ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== "Ch.#ik."
    T ~ car.dup().tap(_.clip.alter(et     ){ c => if c.l then C(c.value.toUpper) else c }).cs ==== "Ch.#ik."

    def sm(i: Int, j: Int) = j*(j+1)/2 - i*(i-1)/2
    T ~ n{ car.clip.visit(3, 5)(cuml += _.n + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ car.clip.visit(3 to 4)(cuml += _.n + _) }  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ car.clip.visit(civ)(cuml += _.n + _) }     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ car.clip.visit(cpv)(cuml += _.n + _) }     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ car.clip.visit(ix)(cuml += _.n + _) }      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ n{ car.clip.visit(st)(cuml += _.n + _) }      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ n{ car.clip.visit(3, 9)(cuml += _.n + _) }    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ n{ car.clip.visit(3 to 8)(cuml += _.n + _) }  ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ n{ car.clip.visit(eiv)(cuml += _.n + _) }     ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~    car.     visit(3, 9)(cuml += _.n + _)      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(3 to 8)(cuml += _.n + _)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(eiv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.visit(-2, 5)(cuml += _.n + _) }   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ car.clip.visit(-2 to 4)(cuml += _.n + _) } ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ car.clip.visit(fiv)(cuml += _.n + _) }     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ car.clip.visit(fpv)(cuml += _.n + _) }     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~    car.     visit(-2, 5)(cuml += _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(-2 to 4)(cuml += _.n + _)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(fiv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(fpv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.visit(-2, 9)(cuml += _.n + _) }   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ n{ car.clip.visit(-2 to 9)(cuml += _.n + _) } ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ n{ car.clip.visit(biv)(cuml += _.n + _) }     ==== str.map(_.toInt).sum + sm(0, 6)
    T ~    car.     visit(-2, 9)(cuml += _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(-2 to 9)(cuml += _.n + _)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(biv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.visit(8, 9)(cuml += _.n + _) }    ==== 0
    T ~ n{ car.clip.visit(8 to 9)(cuml += _.n + _) }  ==== 0
    T ~ n{ car.clip.visit(niv)(cuml += _.n + _) }     ==== 0
    T ~ n{ car.clip.visit(npv)(cuml += _.n + _) }     ==== 0
    T ~    car.     visit(8, 9)(cuml += _.n + _)      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(8 to 9)(cuml += _.n + _)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(niv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(npv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.visit(ex)(cuml += _.n + _) }      ==== ".c#.".map(_.toInt).sum + 11
    T ~ n{ car.clip.visit(et)(cuml += _.n + _) }      ==== ".c#.".map(_.toInt).sum + 11
    T ~    car.     visit(ex)(cuml += _.n + _)        ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(et)(cuml += _.n + _)        ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(3, 5)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.gather(0)(3 to 4)(_ + _.n + _)  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.gather(0)(civ)(_ + _.n + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.gather(0)(cpv)(_ + _.n + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.gather(0)(ix)(_ + _.n + _)      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ car.clip.gather(0)(st)(_ + _.n + _)      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ car.clip.gather(0)(3, 9)(_ + _.n + _)    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.clip.gather(0)(3 to 8)(_ + _.n + _)  ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.clip.gather(0)(eiv)(_ + _.n + _)     ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.     gather(0)(3, 9)(_ + _.n + _)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(3 to 8)(_ + _.n + _)  ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(eiv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(-2, 5)(_ + _.n + _)   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.gather(0)(-2 to 4)(_ + _.n + _) ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.gather(0)(fiv)(_ + _.n + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.gather(0)(fpv)(_ + _.n + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.     gather(0)(-2, 5)(_ + _.n + _)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(-2 to 4)(_ + _.n + _) ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(fiv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(fpv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(-2, 9)(_ + _.n + _)   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.clip.gather(0)(-2 to 9)(_ + _.n + _) ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.clip.gather(0)(biv)(_ + _.n + _)     ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.     gather(0)(-2, 9)(_ + _.n + _)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(-2 to 9)(_ + _.n + _) ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(biv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(8, 9)(_ + _.n + _)    ==== 0
    T ~ car.clip.gather(0)(8 to 9)(_ + _.n + _)  ==== 0
    T ~ car.clip.gather(0)(niv)(_ + _.n + _)     ==== 0
    T ~ car.clip.gather(0)(npv)(_ + _.n + _)     ==== 0
    T ~ car.     gather(0)(8, 9)(_ + _.n + _)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(8 to 9)(_ + _.n + _)  ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(niv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(npv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(ex)(_ + _.n + _)      ==== ".c#.".map(_.toInt).sum + 11
    T ~ car.clip.gather(0)(et)(_ + _.n + _)      ==== ".c#.".map(_.toInt).sum + 11
    T ~ car.     gather(0)(ex)(_ + _.n + _)      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(et)(_ + _.n + _)      ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.whereIn(3, 5)(_.l)          =**= Array(4)
    T ~ car.clip.whereIn(3 to 4)(_.l)        =**= Array(4)
    T ~ car.clip.whereIn(civ)(_.l)           =**= Array(4)
    T ~ car.clip.whereIn(cpv)(_.l)           =**= Array(4)
    T ~ car.clip.whereIn(3, 9)(_.l)          =**= Array(4, 5)
    T ~ car.clip.whereIn(3 to 8)(_.l)        =**= Array(4, 5)
    T ~ car.clip.whereIn(eiv)(_.l)           =**= Array(4, 5)
    T ~ car.clip.whereIn(-2, 5)(_.l)         =**= Array(0, 1, 4)
    T ~ car.clip.whereIn(-2 to 4)(_.l)       =**= Array(0, 1, 4)
    T ~ car.clip.whereIn(fiv)(_.l)           =**= Array(0, 1, 4)
    T ~ car.clip.whereIn(fpv)(_.l)           =**= Array(0, 1, 4)
    T ~ car.clip.whereIn(-2, 9)(_.l)         =**= Array(0, 1, 4, 5)
    T ~ car.clip.whereIn(-2 to 8)(_.l)       =**= Array(0, 1, 4, 5)
    T ~ car.clip.whereIn(biv)(_.l)           =**= Array(0, 1, 4, 5)
    T ~ car.clip.whereIn(8, 9)(_.l).length   ==== 0
    T ~ car.clip.whereIn(8 to 9)(_.l).length ==== 0
    T ~ car.clip.whereIn(niv)(_.l).length    ==== 0
    T ~ car.clip.whereIn(npv)(_.l).length    ==== 0
    T ~ car.clip.whereFrom(ex)(! _.l)        =**= Array(2, 3, 6)

    def linc(c: C.Type, i: Int) = if c.l then i+7 else -1
    def ninc(c: C.Type, i: Int) = if c.l then -1 else i+7
    T ~ car.clip.whereInOp(1, 12)(linc)         =**= car.clip.whereIn(1, 12)(_.l).copyWith(_ + 7)
    T ~ car.clip.whereInOp(-3, 5)(linc)         =**= car.clip.whereIn(-3, 5)(_.l).copyWith(_ + 7)
    T ~ car.clip.whereInOp(Iv(1, 12))(linc)     =**= car.clip.whereIn(Iv(1, 12))(_.l).copyWith(_ + 7)
    T ~ car.clip.whereInOp(Iv(-3, 5))(linc)     =**= car.clip.whereIn(Iv(-3, 5))(_.l).copyWith(_ + 7)
    T ~ car.clip.whereInOp(End-9 to End-2)(linc) =**= car.clip.whereIn(End-9 to End-2)(_.l).copyWith(_ + 7)
    T ~ car.clip.whereInOp(1 to 12)(linc)        =**= car.clip.whereIn(1 to 12)(_.l).copyWith(_ + 7)
    T ~ car.clip.whereInOp(-3 to 5)(linc)        =**= car.clip.whereIn(-3 to 5)(_.l).copyWith(_ + 7)
    T ~ car.clip.whereFromOp(ex)(ninc)           =**= car.clip.whereFrom(ex)(! _.l).copyWith(_ + 7)

    val ca9 = "ABCDEFGHI".c
    val ca7 = "1234567".c
    val ca3 = "890".c
    val ca1 = "%".c

    T ~ car.dup(_.clip() = ca7).cs          ==== "1234567"
    T ~ car.dup(_.clip(civ) = C('x')).cs    ==== "ch.xxk."
    T ~ car.dup(_.clip(civ) = ca3).cs       ==== "ch.89k."
    T ~ car.dup(_.clip(3 to 4) = C('x')).cs ==== "ch.xxk."
    T ~ car.dup(_.clip(3 to 4) = ca3).cs    ==== "ch.89k."
    T ~ car.dup(_.clip(cpv) = C('x')).cs    ==== "ch.xxk."
    T ~ car.dup(_.clip(cpv) = ca3).cs       ==== "ch.89k."
    T ~ car.dup(_.clip(ix) = C('x')).cs     ==== "cxxxik."
    T ~ car.dup(_.clip(ix) = ca7).cs        ==== "c415ik."    
    T ~ car.dup(_.clip(st) = C('x')).cs     ==== "cxxxik."
    T ~ car.dup(_.clip(st) = ca7).cs        ==== "c415ik."

    T ~ car.dup(_.clip() = ca3).cs ==== "890#ik."
    T ~ car.dup(_() = ca3).cs      ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(eiv)    = C('x')).cs ==== "ch.xxxx"
    T ~ car.dup(_.clip(3 to 8) = C('x')).cs ==== "ch.xxxx"
    T ~ car.dup(_.clip(eiv)    = ca7).cs    ==== "ch.1234"
    T ~ car.dup(_.clip(3 to 8) = ca7).cs    ==== "ch.1234"
    T ~ car.dup(_.clip(eiv)    = ca3).cs    ==== "ch.890."
    T ~ car.dup(_.clip(3 to 8) = ca3).cs    ==== "ch.890."
    T ~ car.dup(_(eiv)         = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(3 to 8)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(eiv)         = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(3 to 8)      = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(eiv)         = ca3).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(3 to 8)      = ca3).cs    ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(fiv)     = C('x')).cs ==== "xxxxxk."
    T ~ car.dup(_.clip(fpv)     = C('x')).cs ==== "xxxxxk."
    T ~ car.dup(_.clip(-2 to 4) = C('x')).cs ==== "xxxxxk."
    T ~ car.dup(_.clip(fiv)     = ca7).cs    ==== "12345k."
    T ~ car.dup(_.clip(fpv)     = ca7).cs    ==== "12345k."
    T ~ car.dup(_.clip(-2 to 4) = ca7).cs    ==== "12345k."
    T ~ car.dup(_.clip(fiv)     = ca3).cs    ==== "890#ik."
    T ~ car.dup(_.clip(fpv)     = ca3).cs    ==== "890#ik."
    T ~ car.dup(_.clip(-2 to 4) = ca3).cs    ==== "890#ik."
    T ~ car.dup(_(fiv)          = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(fpv)          = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(-2 to 4)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(fiv)          = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(fpv)          = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(-2 to 4)      = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(biv)     = C('x')).cs ==== "xxxxxxx"
    T ~ car.dup(_.clip(-2 to 8) = C('x')).cs ==== "xxxxxxx"
    T ~ car.dup(_.clip(biv)     = ca7).cs    ==== "1234567"
    T ~ car.dup(_.clip(-2 to 8) = ca7).cs    ==== "1234567"
    T ~ car.dup(_.clip(biv)     = ca3).cs    ==== "890#ik."
    T ~ car.dup(_.clip(-2 to 8) = ca3).cs    ==== "890#ik."

    T ~ car.dup(_.clip(niv)    = C('x')).cs ==== "ch.#ik."
    T ~ car.dup(_.clip(npv)    = C('x')).cs ==== "ch.#ik."
    T ~ car.dup(_.clip(8 to 9) = C('x')).cs ==== "ch.#ik."
    T ~ car.dup(_.clip(niv)    = ca1).cs    ==== "ch.#ik."
    T ~ car.dup(_.clip(npv)    = ca1).cs    ==== "ch.#ik."
    T ~ car.dup(_.clip(8 to 9) = ca1).cs    ==== "ch.#ik."
    T ~ car.dup(_(niv)         = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(npv)         = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(8 to 9)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(niv)         = ca1).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(npv)         = ca1).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(8 to 9)      = ca1).cs    ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(ix) = ca3).cs ==== "c089ik."    
    T ~ car.dup(_.clip(st) = ca3).cs ==== "c089ik."    
    T ~ car.dup(_(ix)      = ca3).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(st)      = ca3).cs ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(ex) = C('x')).cs ==== "xhxxikx"
    T ~ car.dup(_.clip(et) = C('x')).cs ==== "xhxxikx"
    T ~ car.dup(_.clip(ex) = ca7).cs    ==== "2h13ik4"
    T ~ car.dup(_.clip(et) = ca7).cs    ==== "2h13ik4"
    T ~ car.dup(_.clip(ex) = ca3).cs    ==== "9h80ik."
    T ~ car.dup(_.clip(et) = ca3).cs    ==== "9h80ik."
    T ~ car.dup(_(ex)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(et)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(ex)      = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(et)      = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]

    inline def gc(run: (() => C.Type) => Array[C.Type]): String =
      var x = '0'
      val f = () => { x = (x+1).toChar; C(x) }
      run(f).cs
    inline def ic(run: (Int => C.Type) => Array[C.Type]): String =
      val f = (i: Int) => C(('0' + i).toChar)
      run(f).cs
    T ~ gc{ f => car.dup(_.clip.set(3, 5  )(f)) } ==== "ch.12k."
    T ~ ic{ f => car.dup(_.clip.set(3, 5  )(f)) } ==== "ch.34k."
    T ~ gc{ f => car.dup(_.clip.set(civ   )(f)) } ==== "ch.12k."
    T ~ ic{ f => car.dup(_.clip.set(civ   )(f)) } ==== "ch.34k."
    T ~ gc{ f => car.dup(_.clip.set(3 to 4)(f)) } ==== "ch.12k."
    T ~ ic{ f => car.dup(_.clip.set(3 to 4)(f)) } ==== "ch.34k."
    T ~ gc{ f => car.dup(_.clip.set(cpv   )(f)) } ==== "ch.12k."
    T ~ ic{ f => car.dup(_.clip.set(cpv   )(f)) } ==== "ch.34k."
    T ~ gc{ f => car.dup(_.clip.set(ix    )(f)) } ==== "c415ik."
    T ~ ic{ f => car.dup(_.clip.set(ix    )(f)) } ==== "c123ik."    
    T ~ gc{ f => car.dup(_.clip.set(st    )(f)) } ==== "c415ik."
    T ~ ic{ f => car.dup(_.clip.set(st    )(f)) } ==== "c123ik."

    T ~ gc{ f => car.dup(_.clip.set(3, 9  )(f)) } ==== "ch.1234"
    T ~ ic{ f => car.dup(_.clip.set(3, 9  )(f)) } ==== "ch.3456"
    T ~ gc{ f => car.dup(_.clip.set(eiv   )(f)) } ==== "ch.1234"
    T ~ ic{ f => car.dup(_.clip.set(eiv   )(f)) } ==== "ch.3456"
    T ~ gc{ f => car.dup(_.clip.set(3 to 8)(f)) } ==== "ch.1234"
    T ~ ic{ f => car.dup(_.clip.set(3 to 8)(f)) } ==== "ch.3456"
    T ~ gc{ f => car.dup(_.     set(3, 9  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(3, 9  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(eiv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(eiv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(3 to 8)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(3 to 8)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ gc{ f => car.dup(_.clip.set(-2, 5  )(f)) } ==== "12345k."
    T ~ ic{ f => car.dup(_.clip.set(-2, 5  )(f)) } ==== "01234k."
    T ~ gc{ f => car.dup(_.clip.set(-2 to 4)(f)) } ==== "12345k."
    T ~ ic{ f => car.dup(_.clip.set(-2 to 4)(f)) } ==== "01234k."
    T ~ gc{ f => car.dup(_.clip.set(fiv    )(f)) } ==== "12345k."
    T ~ ic{ f => car.dup(_.clip.set(fiv    )(f)) } ==== "01234k."
    T ~ gc{ f => car.dup(_.clip.set(fpv    )(f)) } ==== "12345k."
    T ~ ic{ f => car.dup(_.clip.set(fpv    )(f)) } ==== "01234k."
    T ~ gc{ f => car.dup(_.     set(-2, 5  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(-2, 5  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(-2 to 4)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(-2 to 4)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(fiv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(fiv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(fpv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(fpv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ gc{ f => car.dup(_.clip.set(-2, 9  )(f)) } ==== "1234567"
    T ~ ic{ f => car.dup(_.clip.set(-2, 9  )(f)) } ==== "0123456"
    T ~ gc{ f => car.dup(_.clip.set(-2 to 8)(f)) } ==== "1234567"
    T ~ ic{ f => car.dup(_.clip.set(-2 to 8)(f)) } ==== "0123456"
    T ~ gc{ f => car.dup(_.clip.set(biv    )(f)) } ==== "1234567"
    T ~ ic{ f => car.dup(_.clip.set(biv    )(f)) } ==== "0123456"

    T ~ gc{ f => car.dup(_.clip.set(8, 10 )(f)) } ==== "ch.#ik."
    T ~ ic{ f => car.dup(_.clip.set(8, 10 )(f)) } ==== "ch.#ik."
    T ~ gc{ f => car.dup(_.clip.set(8 to 9)(f)) } ==== "ch.#ik."
    T ~ ic{ f => car.dup(_.clip.set(8 to 9)(f)) } ==== "ch.#ik."
    T ~ gc{ f => car.dup(_.clip.set(niv   )(f)) } ==== "ch.#ik."
    T ~ ic{ f => car.dup(_.clip.set(niv   )(f)) } ==== "ch.#ik."
    T ~ gc{ f => car.dup(_.clip.set(npv   )(f)) } ==== "ch.#ik."
    T ~ ic{ f => car.dup(_.clip.set(npv   )(f)) } ==== "ch.#ik."
    T ~ gc{ f => car.dup(_.     set(8, 10 )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(8, 10 )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(8 to 9)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(8 to 9)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(niv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(niv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(npv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(npv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ gc{ f => car.dup(_.clip.set(ex)(f)) } ==== "2h13ik4"
    T ~ ic{ f => car.dup(_.clip.set(ex)(f)) } ==== "0h23ik6"
    T ~ gc{ f => car.dup(_.clip.set(et)(f)) } ==== "2h13ik4"
    T ~ ic{ f => car.dup(_.clip.set(et)(f)) } ==== "0h23ik6"
    T ~ gc{ f => car.dup(_.     set(ex)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(ex)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(et)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(et)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]

    inline def fc(run: ((C.Type, Int) => C.Type) => Array[C.Type]): String =
      val f = (c: C.Type, i: Int) => C(c ^ i)
      run(f).cs
    T ~ fc{ f => car.dup(_.clip.edit(3, 5  )(f)) } ==== "ch.&mk."
    T ~ fc{ f => car.dup(_.clip.edit(civ   )(f)) } ==== "ch.&mk."
    T ~ fc{ f => car.dup(_.clip.edit(3 to 4)(f)) } ==== "ch.&mk."
    T ~ fc{ f => car.dup(_.clip.edit(cpv   )(f)) } ==== "ch.&mk."
    T ~ fc{ f => car.dup(_.clip.edit(ix    )(f)) } ==== "cj0)ik."    
    T ~ fc{ f => car.dup(_.clip.edit(st    )(f)) } ==== "cj0)ik."
    T ~ fc{ f => car.dup(_.     edit(3, 9  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     edit(eiv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     edit(3 to 8)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.clip.edit(3, 9  )(f)) } ==== "ch.&mp4"
    T ~ fc{ f => car.dup(_.clip.edit(eiv   )(f)) } ==== "ch.&mp4"
    T ~ fc{ f => car.dup(_.clip.edit(3 to 8)(f)) } ==== "ch.&mp4"
    T ~ fc{ f => car.dup(_.     edit(-2, 5  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     edit(-2 to 4)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     edit(fiv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     edit(fpv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.clip.edit(-2, 5  )(f)) } ==== "ci0&mk."
    T ~ fc{ f => car.dup(_.clip.edit(-2 to 4)(f)) } ==== "ci0&mk."
    T ~ fc{ f => car.dup(_.clip.edit(fiv    )(f)) } ==== "ci0&mk."
    T ~ fc{ f => car.dup(_.clip.edit(fpv    )(f)) } ==== "ci0&mk."
    T ~ fc{ f => car.dup(_.clip.edit(-2, 9  )(f)) } ==== "ci0&mp4"
    T ~ fc{ f => car.dup(_.clip.edit(-2 to 8)(f)) } ==== "ci0&mp4"
    T ~ fc{ f => car.dup(_.clip.edit(biv    )(f)) } ==== "ci0&mp4"
    T ~ fc{ f => car.dup(_.     edit(8, 10 )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     edit(8 to 9)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     edit(niv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     edit(npv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.clip.edit(8, 10 )(f)) } ==== "ch.#ik."
    T ~ fc{ f => car.dup(_.clip.edit(8 to 9)(f)) } ==== "ch.#ik."
    T ~ fc{ f => car.dup(_.clip.edit(niv   )(f)) } ==== "ch.#ik."
    T ~ fc{ f => car.dup(_.clip.edit(npv   )(f)) } ==== "ch.#ik."
    T ~ fc{ f => car.dup(_.     edit(ex)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     edit(et)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.clip.edit(ex)(f)) } ==== "ch0&ik4"
    T ~ fc{ f => car.dup(_.clip.edit(et)(f)) } ==== "ch0&ik4"

    var ninja = 0
    T ~ ca9.dup(a => ninja += car.clip.inject(a)).cs            ==== "ch.#ik.HI"
    T ~ ca9.dup(a => ninja += car.clip.inject(a, 2)).cs         ==== "ABch.#ik."
    T ~ { val x = ninja; ninja = 0; x }                         ==== 2*car.length
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(3, 5)).cs      ==== "#i34567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(3, 5)).cs   ==== "12#i567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(3 to 4)).cs    ==== "#i34567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(3 to 4)).cs ==== "12#i567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(civ)).cs       ==== "#i34567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(civ)).cs    ==== "12#i567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(cpv)).cs       ==== "#i34567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(cpv)).cs    ==== "12#i567"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 8*2
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(ix)).cs        ==== ".#hh#67"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(ix)).cs     ==== "12.#hh#"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(st)).cs        ==== ".#hh#67"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(st)).cs     ==== "12.#hh#"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 4*5

    T ~ ca3.dup(a => ninja += car.clip.inject(a)).cs            ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)).cs         ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)).cs         ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 3+5+0

    T ~ ca1.dup(a => ninja += car.clip.inject(a)(3, 5)).cs      ==== "#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(3, 5)).cs   ==== "89#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(3, 5)).cs   ==== "890"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(3 to 4)).cs    ==== "#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(3 to 4)).cs ==== "89#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(3 to 4)).cs ==== "890"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(civ)).cs       ==== "#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(civ)).cs    ==== "89#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(civ)).cs    ==== "890"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(cpv)).cs       ==== "#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(cpv)).cs    ==== "89#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(cpv)).cs    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 4*(1+1+0)

    T ~ ca3.dup(a => ninja += car.clip.inject(a)(3, 9)).cs      ==== "#ik"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 5)(3, 9)).cs   ==== "12345#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(3, 9)).cs   ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(3 to 8)).cs    ==== "#ik"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 5)(3 to 8)).cs ==== "12345#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(3 to 8)).cs ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(eiv)).cs       ==== "#ik"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 5)(eiv)).cs    ==== "12345#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(eiv)).cs    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 3*(3+2+0)

    T ~ ca3.dup(a => ninja += car.clip.inject(a)(-2, 5)).cs      ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 3)(-2, 5)).cs   ==== "123ch.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(-2, 5)).cs   ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(-2 to 4)).cs    ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 3)(-2 to 4)).cs ==== "123ch.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(-2 to 4)).cs ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(fiv)).cs        ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 3)(fiv)).cs     ==== "123ch.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(fiv)).cs     ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(fpv)).cs        ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 3)(fpv)).cs     ==== "123ch.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(fpv)).cs     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 4*(3+4+0)

    T ~ ca3.dup(a => ninja += car.clip.inject(a)(-2, 9)).cs      ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(-2, 9)).cs   ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(-2, 9)).cs   ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(-2 to 8)).cs    ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(-2 to 8)).cs ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(-2 to 8)).cs ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(biv)).cs        ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(biv)).cs     ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(biv)).cs     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 3*(3+5+0)

    T ~ ca1.dup(a => ninja += car.clip.inject(a)(8, 10)).cs     ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a, 2)(8, 10)).cs  ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(8 to 9)).cs    ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a, 2)(8 to 9)).cs ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(niv)).cs       ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a, 2)(niv)).cs    ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(npv)).cs       ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a, 2)(npv)).cs    ==== "%"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 0

    T ~ ca3.dup(a => ninja += car.clip.inject(a)(ix)).cs        ==== ".#h"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 1)(ix)).cs     ==== "8.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(ix)).cs     ==== "890"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, -1)(ix)).cs    ==== ".#hh#67"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(st)).cs        ==== ".#h"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 1)(st)).cs     ==== "8.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(st)).cs     ==== "890"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, -1)(st)).cs    ==== ".#hh#67"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 2*(3+2+0+5)    
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(ex)).cs        ==== ".c#.567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 4)(ex)).cs     ==== "1234.c#"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 9)(ex)).cs     ==== "1234567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, -1)(ex)).cs    ==== ".c#.567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(et)).cs        ==== ".c#.567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 4)(et)).cs     ==== "1234.c#"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 9)(et)).cs     ==== "1234567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, -1)(et)).cs    ==== ".c#.567"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 2*(4+3+0+4)

    T ~ ca7.dup(a => ninja += car.clip.inject(a)(_.l)).cs       ==== "chik567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(_.l)).cs    ==== "12chik7"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(_.l)).cs       ==== "chi"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(_.l)).cs    ==== "89c"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 2*4 + 3 + 1

    val aa9 = "ABCDEFGHI".arr
    val aa7 = "1234567".arr
    val aa3 = "890".arr
    val aa1 = "%".arr
    T ~ aa9.dup(a => ninja += car.clip.injectOp(a)()((c, i) => c ^ i)).str          ==== "ci0&mp4HI"
    T ~ aa9.dup(a => ninja += car.clip.injectOp(a, 2)()((c, i) => c ^ i)).str       ==== "ABci0&mp4"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 2*car.length
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a)(3, 5)((c, i) => c ^ i)).str      ==== "&m34567"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(3, 5)((c, i) => c ^ i)).str   ==== "12&m567"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a)(3 to 4)((c, i) => c ^ i)).str    ==== "&m34567"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(3 to 4)((c, i) => c ^ i)).str ==== "12&m567"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a)(civ)((c, i) => c ^ i)).str       ==== "&m34567"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(civ)((c, i) => c ^ i)).str    ==== "12&m567"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a)(cpv)((c, i) => c ^ i)).str       ==== "&m34567"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(cpv)((c, i) => c ^ i)).str    ==== "12&m567"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 8*2
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a)(ix)((c, i) => c ^ i)).str        ==== "0&ii&67"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(ix)((c, i) => c ^ i)).str     ==== "120&ii&"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a)(st)((c, i) => c ^ i)).str        ==== "0&ii&67"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(st)((c, i) => c ^ i)).str     ==== "120&ii&"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 4*5

    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)()((c, i) => c ^ i)).str          ==== "ci0"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)()((c, i) => c ^ i)).str       ==== "12ci0&m"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)()((c, i) => c ^ i)).str       ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 3+5+0

    T ~ aa1.dup(a => ninja += car.clip.injectOp(a)(3, 5)((c, i) => c ^ i)).str      ==== "&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 2)(3, 5)((c, i) => c ^ i)).str   ==== "89&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(3, 5)((c, i) => c ^ i)).str   ==== "890"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a)(3 to 4)((c, i) => c ^ i)).str    ==== "&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 2)(3 to 4)((c, i) => c ^ i)).str ==== "89&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(3 to 4)((c, i) => c ^ i)).str ==== "890"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a)(civ)((c, i) => c ^ i)).str       ==== "&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 2)(civ)((c, i) => c ^ i)).str    ==== "89&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(civ)((c, i) => c ^ i)).str    ==== "890"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a)(cpv)((c, i) => c ^ i)).str       ==== "&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 2)(cpv)((c, i) => c ^ i)).str    ==== "89&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(cpv)((c, i) => c ^ i)).str    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 4*(1+1+0)

    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(3, 9)((c, i) => c ^ i)).str      ==== "&mp"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 5)(3, 9)((c, i) => c ^ i)).str   ==== "12345&m"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(3, 9)((c, i) => c ^ i)).str   ==== "890"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(3 to 8)((c, i) => c ^ i)).str    ==== "&mp"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 5)(3 to 8)((c, i) => c ^ i)).str ==== "12345&m"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(3 to 8)((c, i) => c ^ i)).str ==== "890"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(eiv)((c, i) => c ^ i)).str       ==== "&mp"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 5)(eiv)((c, i) => c ^ i)).str    ==== "12345&m"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(eiv)((c, i) => c ^ i)).str    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 3*(3+2+0)

    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(-2, 5)((c, i) => c ^ i)).str      ==== "ci0"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 3)(-2, 5)((c, i) => c ^ i)).str   ==== "123ci0&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(-2, 5)((c, i) => c ^ i)).str   ==== "890"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(-2 to 4)((c, i) => c ^ i)).str    ==== "ci0"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 3)(-2 to 4)((c, i) => c ^ i)).str ==== "123ci0&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(-2 to 4)((c, i) => c ^ i)).str ==== "890"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(fiv)((c, i) => c ^ i)).str        ==== "ci0"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 3)(fiv)((c, i) => c ^ i)).str     ==== "123ci0&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(fiv)((c, i) => c ^ i)).str     ==== "890"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(fpv)((c, i) => c ^ i)).str        ==== "ci0"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 3)(fpv)((c, i) => c ^ i)).str     ==== "123ci0&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(fpv)((c, i) => c ^ i)).str     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                                                ==== 4*(3+4+0)

    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(-2, 9)((c, i) => c ^ i)).str      ==== "ci0"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(-2, 9)((c, i) => c ^ i)).str   ==== "12ci0&m"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(-2, 9)((c, i) => c ^ i)).str   ==== "890"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(-2 to 8)((c, i) => c ^ i)).str    ==== "ci0"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(-2 to 8)((c, i) => c ^ i)).str ==== "12ci0&m"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(-2 to 8)((c, i) => c ^ i)).str ==== "890"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(biv)((c, i) => c ^ i)).str        ==== "ci0"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(biv)((c, i) => c ^ i)).str     ==== "12ci0&m"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(biv)((c, i) => c ^ i)).str     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                                                ==== 3*(3+5+0)

    T ~ aa1.dup(a => ninja += car.clip.injectOp(a)(8, 10)((c, i) => c ^ i)).str     ==== "%"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a, 2)(8, 10)((c, i) => c ^ i)).str  ==== "%"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a)(8 to 9)((c, i) => c ^ i)).str    ==== "%"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a, 2)(8 to 9)((c, i) => c ^ i)).str ==== "%"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a)(niv)((c, i) => c ^ i)).str       ==== "%"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a, 2)(niv)((c, i) => c ^ i)).str    ==== "%"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a)(npv)((c, i) => c ^ i)).str       ==== "%"
    T ~ aa1.dup(a => ninja += car.clip.injectOp(a, 2)(npv)((c, i) => c ^ i)).str    ==== "%"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 0

    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(ix)((c, i) => c ^ i)).str        ==== "0&i"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 1)(ix)((c, i) => c ^ i)).str     ==== "80&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(ix)((c, i) => c ^ i)).str     ==== "890"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, -1)(ix)((c, i) => c ^ i)).str    ==== "0&ii&67"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(st)((c, i) => c ^ i)).str        ==== "0&i"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 1)(st)((c, i) => c ^ i)).str     ==== "80&"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 4)(st)((c, i) => c ^ i)).str     ==== "890"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, -1)(st)((c, i) => c ^ i)).str    ==== "0&ii&67"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 2*(3+2+0+5)    
    val ab7 = "abcdefg".arr
    T ~ ab7.dup(a => ninja += car.clip.injectOp(a)(ex)((c, i) => c ^ i)).str        ==== "0c&4efg"
    T ~ ab7.dup(a => ninja += car.clip.injectOp(a, 4)(ex)((c, i) => c ^ i)).str     ==== "abcd0c&"
    T ~ ab7.dup(a => ninja += car.clip.injectOp(a, 9)(ex)((c, i) => c ^ i)).str     ==== "abcdefg"
    T ~ ab7.dup(a => ninja += car.clip.injectOp(a, -1)(ex)((c, i) => c ^ i)).str    ==== "0c&4efg"
    T ~ ab7.dup(a => ninja += car.clip.injectOp(a)(et)((c, i) => c ^ i)).str        ==== "0c&4efg"
    T ~ ab7.dup(a => ninja += car.clip.injectOp(a, 4)(et)((c, i) => c ^ i)).str     ==== "abcd0c&"
    T ~ ab7.dup(a => ninja += car.clip.injectOp(a, 9)(et)((c, i) => c ^ i)).str     ==== "abcdefg"
    T ~ ab7.dup(a => ninja += car.clip.injectOp(a, -1)(et)((c, i) => c ^ i)).str    ==== "0c&4efg"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 2*(4+3+0+4)

    T ~ aa7.dup(a => ninja += car.clip.injectOp(a)(_.l)((c, i) => c ^ i)).str       ==== "cimp567"
    T ~ aa7.dup(a => ninja += car.clip.injectOp(a, 2)(_.l)((c, i) => c ^ i)).str    ==== "12cimp7"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a)(_.l)((c, i) => c ^ i)).str       ==== "cim"
    T ~ aa3.dup(a => ninja += car.clip.injectOp(a, 2)(_.l)((c, i) => c ^ i)).str    ==== "89c"
    T ~ { val x = ninja; ninja = 0; x }                                               ==== 2*4 + 3 + 1

    T ~ car.clip.select(3, 5).cs   ==== "#i"
    T ~ car.clip.select(3 to 4).cs ==== "#i"
    T ~ car.clip.select(civ).cs    ==== "#i"
    T ~ car.clip.select(cpv).cs    ==== "#i"
    T ~ car.clip.select(ix).cs     ==== ".#hh#"
    T ~ car.clip.select(st).cs     ==== ".#hh#"

    T ~ car.clip.select(3, 9).cs   ==== "#ik."
    T ~ car.clip.select(3 to 8).cs ==== "#ik."
    T ~ car.clip.select(eiv).cs    ==== "#ik."

    T ~ car.clip.select(-2, 5).cs   ==== "ch.#i"
    T ~ car.clip.select(-2 to 4).cs ==== "ch.#i"
    T ~ car.clip.select(fiv).cs     ==== "ch.#i"
    T ~ car.clip.select(fpv).cs     ==== "ch.#i"

    T ~ car.clip.select(-2, 9).cs   ==== "ch.#ik."
    T ~ car.clip.select(-2 to 8).cs ==== "ch.#ik."
    T ~ car.clip.select(biv).cs     ==== "ch.#ik."

    T ~ car.clip.select(8, 10).cs  ==== ""
    T ~ car.clip.select(8 to 9).cs ==== ""
    T ~ car.clip.select(niv).cs    ==== ""
    T ~ car.clip.select(npv).cs    ==== ""

    T ~ car.clip.select(et).cs    ==== ".c#."
    T ~ car.clip.select(et).cs    ==== ".c#."

    T ~ car.clip.selectOp(3, 5  )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.selectOp(3 to 4)((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.selectOp(civ   )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.selectOp(cpv   )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.selectOp(ix    )((c, i) => c.value + i) =**= "0&ii&".map(_.toInt)
    T ~ car.clip.selectOp(st    )((c, i) => c.value + i) =**= "0&ii&".map(_.toInt)
    T ~ car.clip.selectOp(3, 5  )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(3 to 4)((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(civ   )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(cpv   )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(ix    )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(st    )((c, i) => c.value + i) ==== typed[Array[Int]]

    T ~ car.clip.selectOp(3, 9  )((c, i) => c.value + i) =**= "&mp4".map(_.toInt)
    T ~ car.clip.selectOp(3 to 8)((c, i) => c.value + i) =**= "&mp4".map(_.toInt)
    T ~ car.clip.selectOp(eiv   )((c, i) => c.value + i) =**= "&mp4".map(_.toInt)

    T ~ car.clip.selectOp(-2, 5  )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.selectOp(-2 to 4)((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.selectOp(fiv    )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.selectOp(fpv    )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)

    T ~ car.clip.selectOp(-2, 9  )((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)
    T ~ car.clip.selectOp(-2 to 8)((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)
    T ~ car.clip.selectOp(biv    )((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)

    T ~ car.clip.selectOp(8, 10 )((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.selectOp(8 to 9)((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.selectOp(niv   )((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.selectOp(npv   )((c, i) => c.value + i) =**= "".map(_.toInt)

    T ~ car.clip.selectOp(ex)((c, i) => c.value + i) =**= "0c&4".map(_.toInt)
    T ~ car.clip.selectOp(et)((c, i) => c.value + i) =**= "0c&4".map(_.toInt)

    val test = "cheesefactories".arr
    val tidx = test.where(_ == 'e')
    val qidx = Array(
      9, 4, 3, 3, 4,
      20, 20, 21, Int.MaxValue-1, Int.MaxValue, Int.MaxValue, Int.MaxValue-1, Int.MaxValue,
      Int.MinValue, Int.MinValue+1, Int.MinValue, Int.MinValue, 
      0, -1, 0, -1, 1, -1000,
      Int.MaxValue, test.length-1, test.length, test.length-1, test.length, test.length-2
    )
    T ~ test.clip.diced(tidx).map(_.str)                       =**= Array("ch", "s", "factori", "s")
    T ~ test.clip.diced(tidx, "", "endpoints").map(_.str)      =**= Array("ch", "s", "factori", "s")
    T ~ test.clip.diced(tidx, "no endpoints").map(_.str)       =**= Array("s", "factori")
    T ~ test.clip.diced(tidx, "", "no endpoints").map(_.str)   =**= Array("s", "factori")
    T ~ test.clip.diced(tidx, "()").map(_.str)                 =**= Array("ch", "", "s", "factori", "s")
    T ~ test.clip.diced(tidx, "()", "endpoints").map(_.str)    =**= Array("ch", "", "s", "factori", "s")
    T ~ test.clip.diced(tidx, "()", "no endpoints").map(_.str) =**= Array("", "s", "factori")
    T ~ test.clip.diced(tidx, "(]").map(_.str)                 =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.clip.diced(tidx, "(]", "endpoints").map(_.str)    =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.clip.diced(tidx, "(]", "no endpoints").map(_.str) =**= Array("e", "se", "factorie")
    T ~ test.clip.diced(tidx, "[)").map(_.str)                 =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.clip.diced(tidx, "[)", "endpoints").map(_.str)    =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.clip.diced(tidx, "[)", "no endpoints").map(_.str) =**= Array("e", "es", "efactori")
    T ~ test.clip.diced(tidx, "[]").map(_.str)                 =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.clip.diced(tidx, "[]", "endpoints").map(_.str)    =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.clip.diced(tidx, "[]", "no endpoints").map(_.str) =**= Array("ee", "ese", "efactorie")
    T ~ test.clip.diced(qidx).map(_.str)                       =**= Array("cheesefac", "cafe", "efactories", "seirotcafeseehc", "c", "c", "cheesefactories", "s", "s")
    T ~ test.clip.diced(qidx, "()").map(_.str)                 =**= Array("cheesefac", "cafe", "", "", "", "efactories", "seirotcafeseehc", "", "", "", "", "c", "c", "cheesefactories", "", "", "", "", "s", "s")
    T ~ test.clip.diced(qidx, "(]").map(_.str)                 =**= Array("cheesefact", "cafes", "e", "", "s", "efactories", "seirotcafeseehc", "c", "", "c", "", "ch", "c", "cheesefactories", "s", "", "s", "", "se", "s")
    T ~ test.clip.diced(qidx, "[)").map(_.str)                 =**= Array("cheesefac", "tcafe", "s", "", "e", "sefactories", "seirotcafeseehc", "", "c", "", "c", "c", "hc", "cheesefactories", "", "s", "", "s", "s", "es")
    T ~ test.clip.diced(qidx, "[]").map(_.str)                 =**= Array("cheesefact", "tcafes", "se", "e", "es", "sefactories", "seirotcafeseehc", "c", "c", "c", "c", "ch", "hc", "cheesefactories", "s", "s", "s", "s", "se", "es")
    T ~ "".arr.clip.diced(Array.empty[Int]).length             ==== 0
    T ~ "".arr.clip.diced(Array.empty[Int], "()").map(_.str)   =**= Array("")
    T ~ "".arr.clip.diced(Array.empty[Int], "(]").map(_.str)   =**= Array("")
    T ~ "".arr.clip.diced(Array.empty[Int], "[)").map(_.str)   =**= Array("")
    T ~ "".arr.clip.diced(Array.empty[Int], "[]").map(_.str)   =**= Array("")


  def arrayBreakInlinedDataTest(): Unit =
    import shortcut.{ quittable => qt }

    inline def qIf[Q >: shortcut.Quits.type <: shortcut.Type](p: Boolean)(using boundary.Label[Q]) =
      shortcut.quit(p).?

    inline def sIf[S >: shortcut.Skips.type <: shortcut.Type](p: Boolean)(using boundary.Label[S]) =
      shortcut.skip(p).?

    var cuml = 0
    val str = "ch.#ik."
    val car = str.c

    val ix = Array(2, 3, 1, 1, 3)
    def st = ix.stepper
    val civ = Iv(3, 5)
    val cpv = 3 to End-2

    val div = Iv(1, 5)
    val dpv = 1 to End-2

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f: Unit
      cuml

    T ~ z{ car.tap(_.flex.use()(cuml += _.n)) }.cs       ==== str
    T ~ cuml                                             ==== str.map(_.toInt).sum
    T ~ z{ car.tap(_.flex.use(3, 5)(cuml += _.n)) }.cs   ==== str
    T ~ cuml                                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.flex.use(3 to 4)(cuml += _.n)) }.cs ==== str
    T ~ cuml                                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.flex.use(civ)(cuml += _.n)) }.cs    ==== str
    T ~ cuml                                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.flex.use(cpv)(cuml += _.n)) }.cs    ==== str
    T ~ cuml                                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.flex.use(ix)(cuml += _.n)) }.cs     ==== str
    T ~ cuml                                             ==== ".#hh#".map(_.toInt).sum
    T ~ z{ car.tap(_.flex.use(st)(cuml += _.n)) }.cs     ==== str
    T ~ cuml                                             ==== ".#hh#".map(_.toInt).sum

    T ~ z{ car.tap(_.flex.use(){ c => qIf(!c.l); cuml += c.n }) }.cs      ==== str
    T ~ cuml                                                              ==== str.take(2).map(_.toInt).sum
    T ~ z{ car.tap(_.flex.use(3, 5){ c => qIf(c.l); cuml += c.n }) }.cs   ==== str
    T ~ cuml                                                              ==== str(3).toInt
    T ~ z{ car.tap(_.flex.use(3 to 4){ c => qIf(c.l); cuml += c.n }) }.cs ==== str
    T ~ cuml                                                              ==== str(3).toInt
    T ~ z{ car.tap(_.flex.use(civ){ c => qIf(c.l); cuml += c.n }) }.cs    ==== str
    T ~ cuml                                                              ==== str(3).toInt
    T ~ z{ car.tap(_.flex.use(cpv){ c => qIf(c.l); cuml += c.n }) }.cs    ==== str
    T ~ cuml                                                              ==== str(3).toInt
    T ~ z{ car.tap(_.flex.use(ix){ c => qIf(c.l); cuml += c.n }) }.cs     ==== str
    T ~ cuml                                                              ==== ".#".map(_.toInt).sum
    T ~ z{ car.tap(_.flex.use(st){ c => qIf(c.l); cuml += c.n }) }.cs     ==== str
    T ~ cuml                                                              ==== ".#".map(_.toInt).sum

    T ~ car.dup().tap(_.flex.alter(){ c => qIf(c.value=='#'); C(if !c.l then '-' else c.value.toUpper) }).cs       ==== "CH-#ik."
    T ~ car.dup().tap(_.flex.alter(3, 5){ c => qIf(c.value=='i'); C(if !c.l then '-' else c.value.toUpper) }).cs   ==== "ch.-ik."
    T ~ car.dup().tap(_.flex.alter(3 to 4){ c => qIf(c.value=='i'); C(if !c.l then '-' else c.value.toUpper) }).cs ==== "ch.-ik."
    T ~ car.dup().tap(_.flex.alter(civ){ c => qIf(c.value=='i'); C(if !c.l then '-' else c.value.toUpper) }).cs    ==== "ch.-ik."
    T ~ car.dup().tap(_.flex.alter(cpv){ c => qIf(c.value=='i'); C(if !c.l then '-' else c.value.toUpper) }).cs    ==== "ch.-ik."
    T ~ car.dup().tap(_.flex.alter(ix){ c => qIf(c.value=='#'); C(if !c.l then '-' else c.value.toUpper) }).cs     ==== "ch-#ik."
    T ~ car.dup().tap(_.flex.alter(st){ c => qIf(c.value=='#'); C(if !c.l then '-' else c.value.toUpper) }).cs     ==== "ch-#ik."

    T ~ n{ qt{ car.visit(){ (c, i) => qIf(!c.l); cuml += c.n + i } } }      ==== str.take(2).map(_.toInt).sum + 1
    T ~ n{ qt{ car.visit(3, 5){   (c, i) => qIf(c.l); cuml += c.n + i } } } ==== str(3).toInt + 3
    T ~ n{ qt{ car.visit(civ){    (c, i) => qIf(c.l); cuml += c.n + i } } } ==== str(3).toInt + 3
    T ~ n{ qt{ car.visit(3 to 4){ (c, i) => qIf(c.l); cuml += c.n + i } } } ==== str(3).toInt + 3
    T ~ n{ qt{ car.visit(cpv){    (c, i) => qIf(c.l); cuml += c.n + i } } } ==== str(3).toInt + 3
    T ~ n{ qt{ car.visit(ix){ (c, i) => qIf(c.l); cuml += c.n + i } } }     ==== ".#".map(_.toInt).sum + 5
    T ~ n{ qt{ car.visit(st){ (c, i) => qIf(c.l); cuml += c.n + i } } }     ==== ".#".map(_.toInt).sum + 5

    T ~ car.flex.gather(0)()(_ + _.n + _)       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ car.flex.gather(0)(3, 5)(_ + _.n + _)   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.flex.gather(0)(civ)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.flex.gather(0)(3 to 4)(_ + _.n + _) ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.flex.gather(0)(cpv)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.flex.gather(0)(ix)(_ + _.n + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ car.flex.gather(0)(st)(_ + _.n + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum

    T ~ car.flex.gather(0)(){ (a, c, i) => qIf(!c.l); a + c.n + i }      ==== str.take(2).map(_.toInt).sum + 1
    T ~ car.flex.gather(0)(3, 5){ (a, c, i) => qIf(c.l); a + c.n + i }   ==== str(3).toInt + 3
    T ~ car.flex.gather(0)(civ){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3
    T ~ car.flex.gather(0)(3 to 4){ (a, c, i) => qIf(c.l); a + c.n + i } ==== str(3).toInt + 3
    T ~ car.flex.gather(0)(cpv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3
    T ~ car.flex.gather(0)(ix){ (a, c, i) => qIf(c.l); a + c.n + i }     ==== ".#".map(_.toInt).sum + 5
    T ~ car.flex.gather(0)(st){ (a, c, i) => qIf(c.l); a + c.n + i }     ==== ".#".map(_.toInt).sum + 5

    T ~ car.flex.copyWith{ c => sIf(!c.l); c.n }                              =**= car.filter(_.l).map(_.n)
    T ~ car.flex.copyWith{ c => qIf(!c.l); c.n }                              =**= car.take(2).map(_.n)
    T ~ car.flex.copyOp((c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "-h-#-k-"
    T ~ car.flex.copyOp((c, i) => if i%2 == 0 then '-' else c.value)          ==== typed[Array[Char]]
    T ~ car.flex.copyOp{(c, i) => sIf(i%2 == 0); c.value }.mkString           ==== "h#k"
    T ~ car.flex.copyOp{(c, i) => qIf(i == 1 || i == 4); c.value }.mkString   ==== "c"

    T ~ car.flex.where(_.l)                          =**= car.zipWithIndex.collect{ case (c, i) if c.l => i }
    T ~ car.flex.where{ c => qIf(c.value>'i'); c.l } =**= car.zipWithIndex.takeWhile(_._1.value <= 'i').collect{ case (c, i) if c.l => i }

    T ~ car.flex.whereIn(1, 5  ){ c =>                    !c.l } =**= Array(2, 3)
    T ~ car.flex.whereIn(1 to 4){ c =>                    !c.l } =**= Array(2, 3)
    T ~ car.flex.whereIn(div   ){ c =>                    !c.l } =**= Array(2, 3)
    T ~ car.flex.whereIn(dpv   ){ c =>                    !c.l } =**= Array(2, 3)
    T ~ car.flex.whereIn(1, 5  ){ c => qIf(c.value=='#'); !c.l } =**= Array(2)
    T ~ car.flex.whereIn(1 to 4){ c => qIf(c.value=='#'); !c.l } =**= Array(2)
    T ~ car.flex.whereIn(div   ){ c => qIf(c.value=='#'); !c.l } =**= Array(2)
    T ~ car.flex.whereIn(dpv   ){ c => qIf(c.value=='#'); !c.l } =**= Array(2)

    T ~ car.flex.whereFrom(Array(2, 0, 3, 6)){ c =>                    !c.l } =**= Array(2, 3, 6)
    T ~ car.flex.whereFrom(Array(2, 0, 3, 6)){ c => qIf(c.value=='#'); !c.l } =**= Array(2)

    def ninc(c: C.Type, i: Int) = if c.l then -1 else i+7
    def nxnc[Q >: shortcut.Quits.type <: shortcut.Type](c: C.Type, i: Int)(using lb: boundary.Label[Q]) =
      qIf(c.value == '#')
      if c.l then -1 else i+7
    T ~ car.flex.whereInOp(1, 5)(ninc)                =**= car.flex.whereIn(1, 5)(! _.l).copyWith(_ + 7)
    T ~ car.flex.whereInOp(Iv(1, 5))(ninc)            =**= car.flex.whereIn(Iv(1, 5))(! _.l).copyWith(_ + 7)
    T ~ car.flex.whereInOp(1 to End-2)(ninc)          =**= car.flex.whereIn(1 to End-2)(! _.l).copyWith(_ + 7)
    T ~ car.flex.whereInOp(1 to 4)(ninc)              =**= car.flex.whereIn(1 to 4)(! _.l).copyWith(_ + 7)
    T ~ car.flex.whereInOp(1, 5)(nxnc)                =**= car.flex.whereIn(1, 5){ c => qIf(c.value=='#'); !c.l }.copyWith(_ + 7)
    T ~ car.flex.whereInOp(Iv(1, 5))(nxnc)            =**= car.flex.whereIn(Iv(1, 5)){ c => qIf(c.value=='#'); !c.l }.copyWith(_ + 7)
    T ~ car.flex.whereInOp(1 to End-2)(nxnc)          =**= car.flex.whereIn(1 to End-2){ c => qIf(c.value=='#'); !c.l }.copyWith(_ + 7)
    T ~ car.flex.whereInOp(1 to 4)(nxnc)              =**= car.flex.whereIn(1 to 4){ c => qIf(c.value=='#'); !c.l }.copyWith(_ + 7)
    T ~ car.flex.whereFromOp(Array(2, 0, 3, 6))(ninc) =**= car.flex.whereFrom(Array(2, 0, 3, 6))(! _.l).copyWith(_ + 7)
    T ~ car.flex.whereFromOp(Array(2, 0, 3, 6))(nxnc) =**= car.flex.whereFrom(Array(2, 0, 3, 6)){ c => qIf(c.value=='#'); !c.l }.copyWith(_ + 7)

    T ~ car.dup{ a => qt{ var x = '0'; a.set(){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs       ==== "1h.#ik."
    T ~ car.dup{ a => qt{ a.set(){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs                 ==== "0h.#ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(3, 5){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs   ==== "ch.1ik."
    T ~ car.dup{ a => qt{ a.set(3, 5){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs             ==== "ch.3ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(3 to 4){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs ==== "ch.1ik."
    T ~ car.dup{ a => qt{ a.set(3 to 4){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs           ==== "ch.3ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(civ){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs    ==== "ch.1ik."
    T ~ car.dup{ a => qt{ a.set(civ){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs              ==== "ch.3ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(cpv){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs    ==== "ch.1ik."
    T ~ car.dup{ a => qt{ a.set(cpv){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs              ==== "ch.3ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(ix){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs     ==== "ch1#ik."
    T ~ car.dup{ a => qt{ a.set(ix){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs               ==== "ch23ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(st){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs     ==== "ch1#ik."
    T ~ car.dup{ a => qt{ a.set(st){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs               ==== "ch23ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(_.l){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs    ==== "1h.#ik."
    T ~ car.dup{ a => qt{ a.set(_.l){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs              ==== "0h.#ik."

    T ~ car.dup{ a => qt{ a.edit(){ (c, i) => qIf(i>3 || c.value=='&'); C(c ^ i) } } }.cs                 ==== "ci0&ik."
    T ~ car.dup{ a => qt{ a.edit(3, 5){ (c, i) => qIf(i>3 || c.value=='&'); C(c ^ i) } } }.cs             ==== "ch.&ik."
    T ~ car.dup{ a => qt{ a.edit(3 to 4){ (c, i) => qIf(i>3 || c.value=='&'); C(c ^ i) } } }.cs           ==== "ch.&ik."
    T ~ car.dup{ a => qt{ a.edit(civ){ (c, i) => qIf(i>3 || c.value=='&'); C(c ^ i) } } }.cs              ==== "ch.&ik."
    T ~ car.dup{ a => qt{ a.edit(cpv){ (c, i) => qIf(i>3 || c.value=='&'); C(c ^ i) } } }.cs              ==== "ch.&ik."
    T ~ car.dup{ a => qt{ a.edit(ix){ (c, i) => qIf(i>3 || c.value=='&'); C(c ^ i) } } }.cs               ==== "cj0&ik."
    T ~ car.dup{ a => qt{ a.edit(st){ (c, i) => qIf(i>3 || c.value=='&'); C(c ^ i) } } }.cs               ==== "cj0&ik."

    val cx = "_________".c
    var ninja = 0
    T ~ cx.dup(a => ninja = car.flex.inject(a)(_.l)).cs                              ==== "chik_____"
    T ~ { val x = ninja; ninja = 0; x }                                                   ==== car.count(_.l)
    T ~ cx.dup(a => ninja = car.flex.inject(a, 2)(_.l)).cs                           ==== "__chik___"
    T ~ { val x = ninja; ninja = 0; x }                                                   ==== car.count(_.l)
    T ~ cx.dup(a => ninja = car.flex.inject(a){ c => qIf(c.value=='#'); c.l }).cs    ==== "ch_______"
    T ~ { val x = ninja; ninja = 0; x }                                                   ==== car.takeWhile(_.value != '#').count(_.l)
    T ~ cx.dup(a => ninja = car.flex.inject(a, 2){ c => qIf(c.value=='#'); c.l }).cs ==== "__ch_____"
    T ~ { val x = ninja; ninja = 0; x }                                                   ==== car.takeWhile(_.value != '#').count(_.l)

    val ax = "_________".arr
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(      ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "cim______"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(      ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "___cim___"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(1, 5  ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "i________"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(1, 5  ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "___i_____"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(1, 5  ){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "im_______"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(1, 5  ){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "___im____"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(1 to 4){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "i________"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(1 to 4){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "___i_____"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(1 to 4){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "im_______"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(1 to 4){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "___im____"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(div   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "i________"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(div   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "___i_____"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(div   ){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "im_______"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(div   ){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "___im____"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(dpv   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "i________"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(dpv   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "___i_____"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(dpv   ){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "im_______"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(dpv   ){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "___im____"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(ix    ){ (c, i) => sIf(i==2); qIf( c.l); c ^ i }).str ==== "&________"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(ix    ){ (c, i) => sIf(i==2); qIf( c.l); c ^ i }).str ==== "___&_____"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a   )(st    ){ (c, i) => sIf(i==2); qIf( c.l); c ^ i }).str ==== "&________"
    T ~ ax.dup(a => ninja += car.flex.injectOp(a, 3)(st    ){ (c, i) => sIf(i==2); qIf( c.l); c ^ i }).str ==== "___&_____"
    T ~ { val x = ninja; ninja = 0; x } ==== 2*3 + 2*4*(1+2) + 2*(1+1)

    T ~ car.flex.select(_.l).cs                              ==== "chik"
    T ~ car.flex.select{ c => qIf(c.value=='#'); c.l }.cs    ==== "ch"

    T ~ car.flex.selectOp(3, 5)(  (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "#-"
    T ~ car.flex.selectOp(3 to 4)((c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "#-"
    T ~ car.flex.selectOp(civ)(   (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "#-"
    T ~ car.flex.selectOp(cpv)(   (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "#-"
    T ~ car.flex.selectOp(ix)(    (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "-#hh#"
    T ~ car.flex.selectOp(st)(    (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "-#hh#"
    T ~ car.flex.selectOp(3, 5)(  (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.flex.selectOp(3 to 4)((c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.flex.selectOp(civ)(   (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.flex.selectOp(cpv)(   (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.flex.selectOp(ix)(    (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.flex.selectOp(st)(    (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]

    T ~ car.flex.selectOp(3, 5){  (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#"
    T ~ car.flex.selectOp(3 to 4){(c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#"
    T ~ car.flex.selectOp(civ){   (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#"
    T ~ car.flex.selectOp(cpv){   (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#"
    T ~ car.flex.selectOp(ix){    (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#hh#"
    T ~ car.flex.selectOp(st){    (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#hh#"

    T ~ car.flex.selectOp(3, 5){  (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.flex.selectOp(3 to 4){(c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.flex.selectOp(civ){   (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.flex.selectOp(cpv){   (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.flex.selectOp(ix){    (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== ".#"
    T ~ car.flex.selectOp(st){    (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== ".#"

    val lar = "ch.ix.#n.".c
    T ~ lar.flex.fuse[Int]((c, i, add) => if !c.l then add(i) else Array(c.o).os.foreach(c => add(c.toInt))) =**= Array(99, 46, 104, 46, 2, 105, 46, 120, 46, 5, 6, 110, 46, 8)
    T ~ lar.flex.fuse[Int]((c, i, add) => if !c.l then { qIf(i>5); add(i) } else Array(c.o).os.foreach(c => add(c.toInt))) =**= Array(99, 46, 104, 46, 2, 105, 46, 120, 46, 5)

    val test = "cheesefactories".arr
    T ~ test.flex.diced(_ == 'e').map(_.str)                               =**= Array("ch", "s", "factori", "s")
    T ~ test.flex.diced(_ == 'e', "").map(_.str)                           =**= Array("ch", "s", "factori", "s")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "").map(_.str)   =**= Array("ch", "s", "facto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "").map(_.str)   =**= Array("ch", "s")
    T ~ test.flex.diced(_ == 'e', "()").map(_.str)                         =**= Array("ch", "", "s", "factori", "s")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "()").map(_.str) =**= Array("ch", "", "s", "facto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "()").map(_.str) =**= Array("ch", "", "s", "")
    T ~ test.flex.diced(_ == 'e', "(]").map(_.str)                         =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "(]").map(_.str) =**= Array("che", "e", "se", "facto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "(]").map(_.str) =**= Array("che", "e", "se", "")
    T ~ test.flex.diced(_ == 'e', "[)").map(_.str)                         =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "[)").map(_.str) =**= Array("ch", "e", "es", "efacto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "[)").map(_.str) =**= Array("ch", "e", "es", "e")
    T ~ test.flex.diced(_ == 'e', "[]").map(_.str)                         =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "[]").map(_.str) =**= Array("che", "ee", "ese", "efacto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "[]").map(_.str) =**= Array("che", "ee", "ese", "e")


  def arrayFancyIntervalTest(): Unit =
    import shortcut.{ quittable => qt }

    inline def qIf[Q >: shortcut.Quits.type <: shortcut.Type](p: Boolean)(using boundary.Label[Q]) =
      shortcut.quit(p).?

    inline def sIf[S >: shortcut.Skips.type <: shortcut.Type](p: Boolean)(using boundary.Label[S]) =
      shortcut.skip(p).?

    var cuml = 0
    val str = "ch.#ik."
    val car = str.c

    val ix = Array(2, 3, 1, 1, 3)
    val ex = Array(2, 0, -1, 3, 6, 7, -9, 400) 
    def st = ix.stepper
    def et = ex.stepper

    val civ = Iv(3, 5)
    val cpv = 3 to End-2
    val eiv = Iv(3, 9)
    val fiv = Iv(-2, 5)
    val fpv = End-9 to End-2
    val biv = Iv(-2, 9)
    val niv = Iv(8, 9)
    val npv = End-9 to End-8

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f: Unit
      cuml

    T ~ car.clip.flex ==== typed[FancyArray[C.Type]]
    T ~ car.flex.clip ==== typed[FancyArray[C.Type]]
    T ~ car.fancy      ==== typed[FancyArray[C.Type]]

    T ~ z{ car.tap(_.flex.clip.use(3, 5)(cuml += _.n)) }.cs   ==== str
    T ~ cuml                                                  ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.flex.clip.use(3 to 4)(cuml += _.n)) }.cs ==== str
    T ~ cuml                                                  ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.flex.clip.use(civ)(cuml += _.n)) }.cs    ==== str
    T ~ cuml                                                  ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.flex.clip.use(cpv)(cuml += _.n)) }.cs    ==== str
    T ~ cuml                                                  ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.tap(_.flex.clip.use(ix)(cuml += _.n)) }.cs     ==== str
    T ~ cuml                                                  ==== ".#hh#".map(_.toInt).sum
    T ~ z{ car.tap(_.flex.clip.use(st)(cuml += _.n)) }.cs     ==== str
    T ~ cuml                                                  ==== ".#hh#".map(_.toInt).sum

    T ~ z{ car.tap(_.flex.clip.use(3, 5){ c => qIf(c.l); cuml += c.n }) }.cs   ==== str
    T ~ cuml                                                                   ==== str(3).toInt
    T ~ z{ car.tap(_.flex.clip.use(3 to 4){ c => qIf(c.l); cuml += c.n }) }.cs ==== str
    T ~ cuml                                                                   ==== str(3).toInt
    T ~ z{ car.tap(_.flex.clip.use(civ){ c => qIf(c.l); cuml += c.n }) }.cs    ==== str
    T ~ cuml                                                                   ==== str(3).toInt
    T ~ z{ car.tap(_.flex.clip.use(cpv){ c => qIf(c.l); cuml += c.n }) }.cs    ==== str
    T ~ cuml                                                                   ==== str(3).toInt
    T ~ z{ car.tap(_.flex.clip.use(ix){ c => qIf(c.l); cuml += c.n }) }.cs     ==== str
    T ~ cuml                                                                   ==== ".#".map(_.toInt).sum
    T ~ z{ car.tap(_.flex.clip.use(st){ c => qIf(c.l); cuml += c.n }) }.cs     ==== str
    T ~ cuml                                                                   ==== ".#".map(_.toInt).sum

    T ~ n{ car.tap(_.flex.clip.use(3, 9)(cuml += _.n)) }    ==== str.substring(3).map(_.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(3 to 8)(cuml += _.n)) }  ==== str.substring(3).map(_.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(eiv)(cuml += _.n)) }     ==== str.substring(3).map(_.toInt).sum

    T ~ n{ car.tap(_.flex.clip.use(3, 9){ c => qIf(c.l); cuml += c.n }) }    ==== str(3).toInt
    T ~ n{ car.tap(_.flex.clip.use(3 to 8){ c => qIf(c.l); cuml += c.n }) }  ==== str(3).toInt
    T ~ n{ car.tap(_.flex.clip.use(eiv){ c => qIf(c.l); cuml += c.n }) }     ==== str(3).toInt

    T ~ n{ car.tap(_.flex.clip.use(-2, 5)(cuml += _.n)) }   ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(-2 to 4)(cuml += _.n)) } ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(fiv)(cuml += _.n)) }     ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(fpv)(cuml += _.n)) }     ==== str.substring(0, 5).map(_.toInt).sum

    T ~ n{ car.tap(_.flex.clip.use(-2, 5){ c => qIf(!c.l); cuml += c.n }) }   ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(-2 to 4){ c => qIf(!c.l); cuml += c.n }) } ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(fiv){ c => qIf(!c.l); cuml += c.n }) }     ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(fpv){ c => qIf(!c.l); cuml += c.n }) }     ==== car.takeWhile(_.l).map(_.value.toInt).sum

    T ~ n{ car.tap(_.flex.clip.use(-2, 9)(cuml += _.n)) }   ==== str.map(_.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(-2 to 9)(cuml += _.n)) } ==== str.map(_.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(biv)(cuml += _.n)) }     ==== str.map(_.toInt).sum

    T ~ n{ car.tap(_.flex.clip.use(-2, 9){ c => qIf(!c.l); cuml += c.n }) }   ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(-2 to 9){ c => qIf(!c.l); cuml += c.n }) } ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(biv){ c => qIf(!c.l); cuml += c.n }) }     ==== car.takeWhile(_.l).map(_.value.toInt).sum

    T ~ n{ car.tap(_.flex.clip.use(8, 9)(cuml += _.n)) }   ==== 0
    T ~ n{ car.tap(_.flex.clip.use(8 to 9)(cuml += _.n)) } ==== 0
    T ~ n{ car.tap(_.flex.clip.use(niv)(cuml += _.n)) }    ==== 0
    T ~ n{ car.tap(_.flex.clip.use(npv)(cuml += _.n)) }    ==== 0

    T ~ n{ car.tap(_.flex.clip.use(8, 9){ c => qIf(c.l); cuml += c.n }) }    ==== 0
    T ~ n{ car.tap(_.flex.clip.use(8 to 9){ c => qIf(c.l); cuml += c.n }) }  ==== 0
    T ~ n{ car.tap(_.flex.clip.use(niv){ c => qIf(c.l); cuml += c.n }) }     ==== 0
    T ~ n{ car.tap(_.flex.clip.use(npv){ c => qIf(c.l); cuml += c.n }) }     ==== 0

    T ~ n{ car.tap(_.flex.clip.use(ex)(cuml += _.n)) } ==== ".c#.".map(_.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(et)(cuml += _.n)) } ==== ".c#.".map(_.toInt).sum

    T ~ n{ car.tap(_.flex.clip.use(ex){ c => qIf(c.value == '#'); cuml += c.n }) } ==== ".c".map(_.toInt).sum
    T ~ n{ car.tap(_.flex.clip.use(et){ c => qIf(c.value == '#'); cuml += c.n }) } ==== ".c".map(_.toInt).sum

    T ~ car.dup().tap(_.clip.flex.alter(3, 9   ){ c => qIf(c.value=='k'); if !c.l then C('-') else c }).cs ==== "ch.-ik."
    T ~ car.dup().tap(_.clip.flex.alter(3 to 8 ){ c => qIf(c.value=='k'); if !c.l then C('-') else c }).cs ==== "ch.-ik."
    T ~ car.dup().tap(_.clip.flex.alter(eiv    ){ c => qIf(c.value=='k'); if !c.l then C('-') else c }).cs ==== "ch.-ik."
    T ~ car.dup().tap(_.clip.flex.alter(-2, 5  ){ c => qIf(c.value=='#'); if !c.l then C('-') else c }).cs ==== "ch-#ik."
    T ~ car.dup().tap(_.clip.flex.alter(-2 to 4){ c => qIf(c.value=='#'); if !c.l then C('-') else c }).cs ==== "ch-#ik."
    T ~ car.dup().tap(_.clip.flex.alter(fiv    ){ c => qIf(c.value=='#'); if !c.l then C('-') else c }).cs ==== "ch-#ik."
    T ~ car.dup().tap(_.clip.flex.alter(fpv    ){ c => qIf(c.value=='#'); if !c.l then C('-') else c }).cs ==== "ch-#ik."
    T ~ car.dup().tap(_.clip.flex.alter(8, 10  ){ c => qIf(c.value=='-'); if !c.l then C('-') else c }).cs ==== str
    T ~ car.dup().tap(_.clip.flex.alter(8 to 9 ){ c => qIf(c.value=='-'); if !c.l then C('-') else c }).cs ==== str
    T ~ car.dup().tap(_.clip.flex.alter(niv    ){ c => qIf(c.value=='-'); if !c.l then C('-') else c }).cs ==== str
    T ~ car.dup().tap(_.clip.flex.alter(npv    ){ c => qIf(c.value=='-'); if !c.l then C('-') else c }).cs ==== str
    T ~ car.dup().tap(_.clip.flex.alter(ex     ){ c => qIf(c.value=='#'); if !c.l then C('-') else c }).cs ==== "ch-#ik."
    T ~ car.dup().tap(_.clip.flex.alter(et     ){ c => qIf(c.value=='#'); if !c.l then C('-') else c }).cs ==== "ch-#ik."

    def sm(i: Int, j: Int) = j*(j+1)/2 - i*(i-1)/2
    T ~ car.clip.flex.gather(0)(3, 5)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.flex.gather(0)(3 to 4)(_ + _.n + _)  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.flex.gather(0)(civ)(_ + _.n + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.flex.gather(0)(cpv)(_ + _.n + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.flex.gather(0)(ix)(_ + _.n + _)      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ car.clip.flex.gather(0)(st)(_ + _.n + _)      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ car.clip.flex.gather(0)(3, 5){ (a, c, i) => qIf(c.l); a + c.n + i }   ==== str(3).toInt + 3
    T ~ car.clip.flex.gather(0)(civ){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3
    T ~ car.clip.flex.gather(0)(3 to 4){ (a, c, i) => qIf(c.l); a + c.n + i } ==== str(3).toInt + 3
    T ~ car.clip.flex.gather(0)(cpv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3
    T ~ car.clip.flex.gather(0)(ix){ (a, c, i) => qIf(c.l); a + c.n + i }     ==== ".#".map(_.toInt).sum + 5
    T ~ car.clip.flex.gather(0)(st){ (a, c, i) => qIf(c.l); a + c.n + i }     ==== ".#".map(_.toInt).sum + 5

    T ~ car.clip.flex.gather(0)(3, 9)(_ + _.n + _)   ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.clip.flex.gather(0)(3 to 8)(_ + _.n + _) ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.clip.flex.gather(0)(eiv)(_ + _.n + _)    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)

    T ~ car.clip.flex.gather(0)(3, 9){ (a, c, i) => qIf(c.l); a + c.n + i }   ==== str(3).toInt + 3
    T ~ car.clip.flex.gather(0)(3 to 8){ (a, c, i) => qIf(c.l); a + c.n + i } ==== str(3).toInt + 3
    T ~ car.clip.flex.gather(0)(eiv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3

    T ~ car.clip.flex.gather(0)(-2, 5)(_ + _.n + _)   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.flex.gather(0)(-2 to 4)(_ + _.n + _) ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.flex.gather(0)(fiv)(_ + _.n + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.flex.gather(0)(fpv)(_ + _.n + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)

    T ~ car.clip.flex.gather(0)(-2, 5){ (a, c, i) => qIf(!c.l); a + c.n + i }   ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.flex.gather(0)(-2 to 4){ (a, c, i) => qIf(!c.l); a + c.n + i } ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.flex.gather(0)(fiv){ (a, c, i) => qIf(!c.l); a + c.n + i }     ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.flex.gather(0)(fpv){ (a, c, i) => qIf(!c.l); a + c.n + i }     ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)

    T ~ car.clip.flex.gather(0)(-2, 9)(_ + _.n + _)   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.clip.flex.gather(0)(-2 to 9)(_ + _.n + _) ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.clip.flex.gather(0)(biv)(_ + _.n + _)     ==== str.map(_.toInt).sum + sm(0, 6)

    T ~ car.clip.flex.gather(0)(-2, 9){ (a, c, i) => qIf(!c.l); a + c.n + i }   ==== str.take(2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.flex.gather(0)(-2 to 9){ (a, c, i) => qIf(!c.l); a + c.n + i } ==== str.take(2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.flex.gather(0)(biv){ (a, c, i) => qIf(!c.l); a + c.n + i }     ==== str.take(2).map(_.toInt).sum + sm(0, 1)

    T ~ car.clip.flex.gather(0)(8, 9)(_ + _.n + _)   ==== 0
    T ~ car.clip.flex.gather(0)(8 to 9)(_ + _.n + _) ==== 0
    T ~ car.clip.flex.gather(0)(niv)(_ + _.n + _)    ==== 0
    T ~ car.clip.flex.gather(0)(npv)(_ + _.n + _)    ==== 0

    T ~ car.clip.flex.gather(0)(8, 9){ (a, c, i) => qIf(c.l); a + c.n + i }   ==== 0
    T ~ car.clip.flex.gather(0)(8 to 9){ (a, c, i) => qIf(c.l); a + c.n + i } ==== 0
    T ~ car.clip.flex.gather(0)(niv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== 0
    T ~ car.clip.flex.gather(0)(npv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== 0

    T ~ car.clip.flex.gather(0)(ex)(_ + _.n + _) ==== ".c#.".map(_.toInt).sum + 11
    T ~ car.clip.flex.gather(0)(et)(_ + _.n + _) ==== ".c#.".map(_.toInt).sum + 11

    T ~ car.clip.flex.gather(0)(ex){ (a, c, i) => qIf(c.value == '#'); a + c.n + i } ==== ".c".map(_.toInt).sum + 2
    T ~ car.clip.flex.gather(0)(et){ (a, c, i) => qIf(c.value == '#'); a + c.n + i } ==== ".c".map(_.toInt).sum + 2

    T ~ car.clip.flex.whereIn(3, 9   ){ c =>                    c.l } =**= Array(4, 5)
    T ~ car.clip.flex.whereIn(3 to 8 ){ c =>                    c.l } =**= Array(4, 5)
    T ~ car.clip.flex.whereIn(eiv    ){ c =>                    c.l } =**= Array(4, 5)
    T ~ car.clip.flex.whereIn(-2, 5  ){ c =>                    c.l } =**= Array(0, 1, 4)
    T ~ car.clip.flex.whereIn(-2 to 4){ c =>                    c.l } =**= Array(0, 1, 4)
    T ~ car.clip.flex.whereIn(fiv    ){ c =>                    c.l } =**= Array(0, 1, 4)
    T ~ car.clip.flex.whereIn(fpv    ){ c =>                    c.l } =**= Array(0, 1, 4)
    T ~ car.clip.flex.whereIn(3, 9   ){ c => qIf(c.value=='k'); c.l } =**= Array(4)
    T ~ car.clip.flex.whereIn(3 to 8 ){ c => qIf(c.value=='k'); c.l } =**= Array(4)
    T ~ car.clip.flex.whereIn(eiv    ){ c => qIf(c.value=='k'); c.l } =**= Array(4)
    T ~ car.clip.flex.whereIn(-2, 5  ){ c => qIf(c.value=='#'); c.l } =**= Array(0, 1)
    T ~ car.clip.flex.whereIn(-2 to 4){ c => qIf(c.value=='#'); c.l } =**= Array(0, 1)
    T ~ car.clip.flex.whereIn(fiv    ){ c => qIf(c.value=='#'); c.l } =**= Array(0, 1)
    T ~ car.clip.flex.whereIn(fpv    ){ c => qIf(c.value=='#'); c.l } =**= Array(0, 1)

    T ~ car.clip.flex.whereFrom(Array(2, -3, 9, 7, 5, 3, 4)){ c =>                    c.l } =**= Array(5, 4)
    T ~ car.clip.flex.whereFrom(Array(2, -3, 9, 7, 5, 3, 4)){ c => qIf(c.value=='#'); c.l } =**= Array(5)

    def linc(c: C.Type, i: Int) = if c.l then i+7 else -1
    def li_#[Q >: shortcut.Quits.type <: shortcut.Type](c: C.Type, i: Int)(using lb: boundary.Label[Q]) =
      qIf(c.value == '#')
      if c.l then i+7 else -1
    def li_k[Q >: shortcut.Quits.type <: shortcut.Type](c: C.Type, i: Int)(using lb: boundary.Label[Q]) =
      qIf(c.value == 'k')
      if c.l then i+7 else -1
    T ~ car.fancy.whereInOp(3, 9)(linc)           =**= car.fancy.whereIn(3, 9)(_.l).copyWith(_ + 7)
    T ~ car.fancy.whereInOp(Iv(3, 9))(linc)       =**= car.fancy.whereIn(Iv(3, 9))(_.l).copyWith(_ + 7)
    T ~ car.fancy.whereInOp(3 to 8)(linc)         =**= car.fancy.whereIn(3 to 8)(_.l).copyWith(_ + 7)
    T ~ car.fancy.whereInOp(-2, 5)(linc)          =**= car.fancy.whereIn(-2, 5)(_.l).copyWith(_ + 7)
    T ~ car.fancy.whereInOp(Iv(-2, 5))(linc)      =**= car.fancy.whereIn(Iv(-2, 5))(_.l).copyWith(_ + 7)
    T ~ car.fancy.whereInOp(End-9 to End-2)(linc) =**= car.fancy.whereIn(End-9 to End-2)(_.l).copyWith(_ + 7)
    T ~ car.fancy.whereInOp(3, 9)(li_k)           =**= car.fancy.whereIn(3, 9){ c => qIf(c.value=='k'); c.l }.copyWith(_ + 7)
    T ~ car.fancy.whereInOp(Iv(3, 9))(li_k)       =**= car.fancy.whereIn(Iv(3, 9)){ c => qIf(c.value=='k'); c.l }.copyWith(_ + 7)
    T ~ car.fancy.whereInOp(3 to 8)(li_k)         =**= car.fancy.whereIn(3 to 8){ c => qIf(c.value=='k'); c.l }.copyWith(_ + 7)
    T ~ car.fancy.whereInOp(-2, 5)(li_#)          =**= car.fancy.whereIn(-2, 5){ c => qIf(c.value=='#'); c.l }.copyWith(_ + 7)
    T ~ car.fancy.whereInOp(Iv(-2, 5))(li_#)      =**= car.fancy.whereIn(Iv(-2, 5)){ c => qIf(c.value=='#'); c.l }.copyWith(_ + 7)
    T ~ car.fancy.whereInOp(End-9 to End-2)(li_#) =**= car.fancy.whereIn(End-9 to End-2){ c => qIf(c.value=='#'); c.l }.copyWith(_ + 7)

    T ~ car.fancy.whereFromOp(Array(2, -3, 9, 7, 5, 3, 4))(linc) =**= car.fancy.whereFrom(Array(2, -3, 9, 7, 5, 3, 4)){ c =>                    c.l }.copyWith(_ + 7)
    T ~ car.fancy.whereFromOp(Array(2, -3, 9, 7, 5, 3, 4))(li_#) =**= car.fancy.whereFrom(Array(2, -3, 9, 7, 5, 3, 4)){ c => qIf(c.value=='#'); c.l }.copyWith(_ + 7)

    val ca7 = "1234567".c
    val ca3 = "890".c
    var ninja = 0
    T ~ ca7.dup(a => ninja += car.flex.clip.inject(a)(_.l)).cs     ==== "chik567"
    T ~ ca7.dup(a => ninja += car.flex.clip.inject(a, 2)(_.l)).cs  ==== "12chik7"
    T ~ ca7.dup(a => ninja += car.flex.clip.inject(a, -2)(_.l)).cs ==== "chik567"
    T ~ ca3.dup(a => ninja += car.flex.clip.inject(a)(_.l)).cs     ==== "chi"
    T ~ ca3.dup(a => ninja += car.flex.clip.inject(a, 2)(_.l)).cs  ==== "89c"
    T ~ ca3.dup(a => ninja += car.flex.clip.inject(a, 8)(_.l)).cs  ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                                 ==== 4 + 4 + 4 + 3 + 1 + 0
    T ~ ca7.dup(a => ninja += car.flex.clip.inject(a){ c => qIf(c.value == 'i'); c.l }).cs     ==== "ch34567"
    T ~ ca7.dup(a => ninja += car.flex.clip.inject(a, 2){ c => qIf(c.value == 'i'); c.l }).cs  ==== "12ch567"
    T ~ ca7.dup(a => ninja += car.flex.clip.inject(a, 6){ c => qIf(c.value == 'i'); c.l }).cs  ==== "123456c"
    T ~ ca7.dup(a => ninja += car.flex.clip.inject(a, -3){ c => qIf(c.value == 'i'); c.l }).cs ==== "ch34567"
    T ~ ca3.dup(a => ninja += car.flex.clip.inject(a){ c => qIf(c.value == 'i'); c.l }).cs     ==== "ch0"
    T ~ ca3.dup(a => ninja += car.flex.clip.inject(a, 2){ c => qIf(c.value == 'i'); c.l }).cs  ==== "89c"
    T ~ ca3.dup(a => ninja += car.flex.clip.inject(a, -3){ c => qIf(c.value == 'i'); c.l }).cs ==== "ch0"
    T ~ ca3.dup(a => ninja += car.flex.clip.inject(a, 8){ c => qIf(c.value == 'i'); c.l }).cs  ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                                                             ==== 2+2+1+2+2+1+2+0

    val aa7 = "ABCDEFG".arr
    val aa1 = "H".arr
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ciCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(){ (c, i) => sIf(!c.l); qIf(i==9); c ^ i }).str ==== "cimpEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "cimDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 5 )(){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "ABCDEci"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 9 )(){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "ABCDEFG"
    T ~ { val x = ninja; ninja = 0; x } ==== 2+4+3+2+0
    val ejv = Iv(1, 9)
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(1, 9  ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "imCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(1, 9  ){ (c, i) => sIf(!c.l); qIf(i==7); c ^ i }).str ==== "impDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 6 )(1, 9  ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "ABCDEFi"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(1, 9  ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "imCDEFG"
    T ~ aa1.dup(a => ninja += car.clip.flex.injectOp(a, -2)(1, 9  ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "i"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(1 to 8){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "imCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(1 to 8){ (c, i) => sIf(!c.l); qIf(i==7); c ^ i }).str ==== "impDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 6 )(1 to 8){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "ABCDEFi"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(1 to 8){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "imCDEFG"
    T ~ aa1.dup(a => ninja += car.clip.flex.injectOp(a, -2)(1 to 8){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "i"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(ejv   ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "imCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(ejv   ){ (c, i) => sIf(!c.l); qIf(i==7); c ^ i }).str ==== "impDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 6 )(ejv   ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "ABCDEFi"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(ejv   ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "imCDEFG"
    T ~ aa1.dup(a => ninja += car.clip.flex.injectOp(a, -2)(ejv   ){ (c, i) => sIf(!c.l); qIf(i==5); c ^ i }).str ==== "i"
    T ~ { val x = ninja; ninja = 0; x } ==== 3*(2+3+1+2+1)
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(-2, 5  ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ciCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(-2, 5  ){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "cimDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 6 )(-2, 5  ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFc"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(-2, 5  ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ciCDEFG"
    T ~ aa1.dup(a => ninja += car.clip.flex.injectOp(a, -2)(-2, 5  ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "c"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(-2 to 4){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ciCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(-2 to 4){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "cimDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 6 )(-2 to 4){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFc"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(-2 to 4){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ciCDEFG"
    T ~ aa1.dup(a => ninja += car.clip.flex.injectOp(a, -2)(-2 to 4){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "c"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(fiv    ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ciCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(fiv    ){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "cimDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 6 )(fiv    ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFc"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(fiv    ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ciCDEFG"
    T ~ aa1.dup(a => ninja += car.clip.flex.injectOp(a, -2)(fiv    ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "c"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(fpv    ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ciCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(fpv    ){ (c, i) => sIf(!c.l); qIf(i==6); c ^ i }).str ==== "cimDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 6 )(fpv    ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFc"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(fpv    ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ciCDEFG"
    T ~ aa1.dup(a => ninja += car.clip.flex.injectOp(a, -2)(fpv    ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "c"
    T ~ { val x = ninja; ninja = 0; x } ==== 4*(2+3+1+2+1)
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(8, 10 ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -7)(8, 10 ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 8 )(8, 10 ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(8 to 9){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -7)(8 to 9){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 8 )(8 to 9){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(niv   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -7)(niv   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 8 )(niv   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(npv   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -7)(npv   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 8 )(npv   ){ (c, i) => sIf(!c.l); qIf(i==4); c ^ i }).str ==== "ABCDEFG"
    T ~ { val x = ninja; ninja = 0; x } ==== 0
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(ex){ (c, i) => sIf(c.l); qIf(i==6); c ^ i }).str ==== "0&CDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(ex){ (c, i) => sIf(c.l); qIf(i==8); c ^ i }).str ==== "0&4DEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(ex){ (c, i) => sIf(c.l); qIf(i==6); c ^ i }).str ==== "0&CDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 6 )(ex){ (c, i) => sIf(c.l); qIf(i==6); c ^ i }).str ==== "ABCDEF0"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 9 )(ex){ (c, i) => sIf(c.l); qIf(i==6); c ^ i }).str ==== "ABCDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(et){ (c, i) => sIf(c.l); qIf(i==6); c ^ i }).str ==== "0&CDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a    )(et){ (c, i) => sIf(c.l); qIf(i==8); c ^ i }).str ==== "0&4DEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, -2)(et){ (c, i) => sIf(c.l); qIf(i==6); c ^ i }).str ==== "0&CDEFG"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 6 )(et){ (c, i) => sIf(c.l); qIf(i==6); c ^ i }).str ==== "ABCDEF0"
    T ~ aa7.dup(a => ninja += car.clip.flex.injectOp(a, 9 )(et){ (c, i) => sIf(c.l); qIf(i==6); c ^ i }).str ==== "ABCDEFG"
    T ~ { val x = ninja; ninja = 0; x } ==== 2*(2+3+2+1+0)

    T ~ car.clip.flex.selectOp(3, 5  )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.flex.selectOp(3 to 4)((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.flex.selectOp(civ   )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.flex.selectOp(cpv   )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.flex.selectOp(ix    )((c, i) => c.value + i) =**= "0&ii&".map(_.toInt)
    T ~ car.clip.flex.selectOp(st    )((c, i) => c.value + i) =**= "0&ii&".map(_.toInt)
    T ~ car.clip.flex.selectOp(3, 5  )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.flex.selectOp(3 to 4)((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.flex.selectOp(civ   )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.flex.selectOp(cpv   )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.flex.selectOp(ix    )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.flex.selectOp(st    )((c, i) => c.value + i) ==== typed[Array[Int]]

    T ~ car.clip.flex.selectOp(3, 9  )((c, i) => c.value + i) =**= "&mp4".map(_.toInt)
    T ~ car.clip.flex.selectOp(3 to 8)((c, i) => c.value + i) =**= "&mp4".map(_.toInt)
    T ~ car.clip.flex.selectOp(eiv   )((c, i) => c.value + i) =**= "&mp4".map(_.toInt)

    T ~ car.clip.flex.selectOp(-2, 5  )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.flex.selectOp(-2 to 4)((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.flex.selectOp(fiv    )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.flex.selectOp(fpv    )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)

    T ~ car.clip.flex.selectOp(-2, 9  )((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)
    T ~ car.clip.flex.selectOp(-2 to 8)((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)
    T ~ car.clip.flex.selectOp(biv    )((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)

    T ~ car.clip.flex.selectOp(8, 10 )((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.flex.selectOp(8 to 9)((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.flex.selectOp(niv   )((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.flex.selectOp(npv   )((c, i) => c.value + i) =**= "".map(_.toInt)

    T ~ car.clip.flex.selectOp(ex)((c, i) => c.value + i) =**= "0c&4".map(_.toInt)
    T ~ car.clip.flex.selectOp(et)((c, i) => c.value + i) =**= "0c&4".map(_.toInt)

    T ~ car.clip.flex.selectOp(3, 5){  (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.clip.flex.selectOp(3 to 4){(c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.clip.flex.selectOp(civ){   (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.clip.flex.selectOp(cpv){   (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.clip.flex.selectOp(ix){    (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== ".#"
    T ~ car.clip.flex.selectOp(st){    (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== ".#"

    T ~ car.clip.flex.selectOp(3, 9  ){ (c, i) => qIf(c.value == 'k'); c.value + i } =**= "&m".map(_.toInt)
    T ~ car.clip.flex.selectOp(3 to 8){ (c, i) => qIf(c.value == 'k'); c.value + i } =**= "&m".map(_.toInt)
    T ~ car.clip.flex.selectOp(eiv   ){ (c, i) => qIf(c.value == 'k'); c.value + i } =**= "&m".map(_.toInt)

    T ~ car.clip.flex.selectOp(3, 9  ){ (c, i) => sIf(c.value == 'k'); c.value + i } =**= "&m4".map(_.toInt)
    T ~ car.clip.flex.selectOp(3 to 8){ (c, i) => sIf(c.value == 'k'); c.value + i } =**= "&m4".map(_.toInt)
    T ~ car.clip.flex.selectOp(eiv   ){ (c, i) => sIf(c.value == 'k'); c.value + i } =**= "&m4".map(_.toInt)

    T ~ car.clip.flex.selectOp(-2, 5  ){ (c, i) => qIf(c.value == '#'); c.value + i } =**= "ci0".map(_.toInt)
    T ~ car.clip.flex.selectOp(-2 to 4){ (c, i) => qIf(c.value == '#'); c.value + i } =**= "ci0".map(_.toInt)
    T ~ car.clip.flex.selectOp(fiv    ){ (c, i) => qIf(c.value == '#'); c.value + i } =**= "ci0".map(_.toInt)
    T ~ car.clip.flex.selectOp(fpv    ){ (c, i) => qIf(c.value == '#'); c.value + i } =**= "ci0".map(_.toInt)

    T ~ car.clip.flex.selectOp(-2, 5  ){ (c, i) => sIf(c.value == '#'); c.value + i } =**= "ci0m".map(_.toInt)
    T ~ car.clip.flex.selectOp(-2 to 4){ (c, i) => sIf(c.value == '#'); c.value + i } =**= "ci0m".map(_.toInt)
    T ~ car.clip.flex.selectOp(fiv    ){ (c, i) => sIf(c.value == '#'); c.value + i } =**= "ci0m".map(_.toInt)
    T ~ car.clip.flex.selectOp(fpv    ){ (c, i) => sIf(c.value == '#'); c.value + i } =**= "ci0m".map(_.toInt)

    T ~ car.clip.flex.selectOp(-2, 9  ){ (c, i) => qIf(c.value == 'i'); c.value + i } =**= "ci0&".map(_.toInt)
    T ~ car.clip.flex.selectOp(-2 to 8){ (c, i) => qIf(c.value == 'i'); c.value + i } =**= "ci0&".map(_.toInt)
    T ~ car.clip.flex.selectOp(biv    ){ (c, i) => qIf(c.value == 'i'); c.value + i } =**= "ci0&".map(_.toInt)

    T ~ car.clip.flex.selectOp(-2, 9  ){ (c, i) => sIf(c.value == 'i'); c.value + i } =**= "ci0&p4".map(_.toInt)
    T ~ car.clip.flex.selectOp(-2 to 8){ (c, i) => sIf(c.value == 'i'); c.value + i } =**= "ci0&p4".map(_.toInt)
    T ~ car.clip.flex.selectOp(biv    ){ (c, i) => sIf(c.value == 'i'); c.value + i } =**= "ci0&p4".map(_.toInt)

    T ~ car.clip.flex.selectOp(8, 10 ){ (c, i) => sIf(!c.l); qIf(c.n > 99); c.value + i } =**= "".map(_.toInt)
    T ~ car.clip.flex.selectOp(8 to 9){ (c, i) => sIf(!c.l); qIf(c.n > 99); c.value + i } =**= "".map(_.toInt)
    T ~ car.clip.flex.selectOp(niv   ){ (c, i) => sIf(!c.l); qIf(c.n > 99); c.value + i } =**= "".map(_.toInt)
    T ~ car.clip.flex.selectOp(npv   ){ (c, i) => sIf(!c.l); qIf(c.n > 99); c.value + i } =**= "".map(_.toInt)

    T ~ car.clip.flex.selectOp(ex){ (c, i) => qIf(i == 3); c.value + i } =**= "0c".map(_.toInt)
    T ~ car.clip.flex.selectOp(et){ (c, i) => qIf(i == 3); c.value + i } =**= "0c".map(_.toInt)

    T ~ car.clip.flex.selectOp(ex){ (c, i) => sIf(i == 3); c.value + i } =**= "0c4".map(_.toInt)
    T ~ car.clip.flex.selectOp(et){ (c, i) => sIf(i == 3); c.value + i } =**= "0c4".map(_.toInt)


  def arrayPrimitiveDataTest(): Unit =
    import java.lang.Float.{intBitsToFloat => i2f}
    import java.lang.Double.{longBitsToDouble => l2d}

    T ~ Iv(5, 8)                    ==== 0x800000005L  --: typed[Iv]
    T ~ Iv(5, 8).i0                 ==== 5
    T ~ Iv(5, 8).iN                 ==== 8

    val atf = Array(false, false, true, true)
    val aip = Array(2, 3, -3)
    val air = Array(3, 4, 3)
    val piv1Em2 = 1 to End-2
    val piv0Em2 = 0 to End-2

    object NuZ extends NewType[Boolean] {}
    val az = Array[Boolean](true, true, false)
    val naz = Array[NuZ.Type](NuZ(true), NuZ(true), NuZ(false))
    T ~ az.copy                            =**= az
    T ~ (az.copy eq az)                    ==== false
    T ~ naz.copy                           =**= naz
    T ~ az.copyToSize(4)                   =**= Array(true, true, false, false)
    T ~ az.copyToSize(2)                   =**= Array(true, true)
    T ~ az.copyToSize(End+1)               =**= Array(true, true, false, false)
    T ~ az.copyToSize(End-1)               =**= Array(true, true)
    T ~ az.shrinkCopy(4)                   =**= az
    T ~ az.shrinkCopy(2)                   =**= az.copyToSize(2)
    T ~ az.copyOfRange(1, 3)               =**= Array(true, false)
    T ~ az.copyOfRange(1 to 2)             =**= Array(true, false)
    T ~ az.copyOfRange(Iv(1, 3))           =**= Array(true, false)
    T ~ az.copyOfRange(1 to End-1)         =**= Array(true)
    T ~ {az.fill(false); az}               =**= Array(false, false, false)
    T ~ {az.fillRange(1 to End)(true); az} =**= Array(false, true, true)
    T ~ {az.fillRange(1, 3)(false); az}    =**= Array(false, false, false)
    T ~ {az.fillRange(Iv(1, 3))(true); az} =**= Array(false, true, true)
    T ~ {az.fillRange(1 to 2)(false); az}  =**= Array(false, false, false)

    object NuB extends NewType[Byte] {}
    val ab = Array[Byte](1, 2, 3)
    val bb = Array[Byte](2, 0, 3, 2, 3)
    val nab = Array[NuB.Type](NuB(1), NuB(2), NuB(3))
    T ~ ab.copy                     =**= ab
    T ~ (ab.copy eq ab)             ==== false
    T ~ nab.copy                    =**= nab
    T ~ ab.copy.tap(_(0) = 4).toSeq =!!= ab.toSeq
    T ~ ab.copyToSize(2).length     ==== 2
    T ~ ab.copyToSize(2)            =**= ab.take(2)
    T ~ ab.copyToSize(4)            =**= Array[Byte](1, 2, 3, 0)
    T ~ ab.copyToSize(End-1)        =**= ab.take(2)
    T ~ ab.copyToSize(End+1)        =**= Array[Byte](1, 2, 3, 0)
    T ~ ab.shrinkCopy(2)            =**= ab.take(2)
    T ~ (ab.shrinkCopy(4) eq ab)    ==== true
    T ~ ab.copyOfRange(1, 3)        =**= ab.drop(1)
    T ~ ab.copyOfRange(Iv(1, 3))    =**= ab.drop(1)
    T ~ ab.copyOfRange(1 to End)    =**= ab.drop(1)
    T ~ ab.copyOfRange(1 to 2)      =**= ab.drop(1)
    T ~ (ab ++ bb).packInts         =**= Array[Int](0x02030201, 0x03020300)
    T ~ (ab ++ bb).packFloats       =**= Array[Float](i2f(0x02030201), i2f(0x03020300))
    T ~ (ab ++ bb).packLongs        =**= Array[Long](0x0302030002030201L)
    T ~ (ab ++ bb).packDoubles      =**= Array[Double](l2d(0x0302030002030201L))
    T ~ bb.isSorted                 ==== false
    T ~ bb.isSortedRange(1, 3)      ==== true
    T ~ bb.isSortedRange(Iv(1, 3))  ==== true
    T ~ bb.isSortedRange(piv1Em2)   ==== true
    T ~ bb.isSortedRange(1 to 2)    ==== true
    T ~ ab.search(2)                ==== 1
    T ~ ab.search(0)                ==== -1
    T ~ bb.searchRange(1, 3)(3)     ==== 2
    T ~ bb.searchRange(1, 3)(2)     ==== -3
    T ~ bb.searchRange(Iv(1, 3))(3) ==== 2
    T ~ bb.searchRange(Iv(1, 3))(2) ==== -3
    T ~ bb.searchRange(piv1Em2)(3)  ==== 2
    T ~ bb.searchRange(piv1Em2)(2)  ==== -3
    T ~ bb.searchRange(1 to 2)(3)   ==== 2
    T ~ bb.searchRange(1 to 2)(2)   ==== -3
    /*
    T ~ bb.copy.sortRange(0 to 2)   =**= Array[Byte](0, 2, 3, 2, 3)
    T ~ bb.copy.sortRange(Iv(0,3))  =**= Array[Byte](0, 2, 3, 2, 3)
    T ~ bb.copy.sortRange(piv0Em2)  =**= Array[Byte](0, 2, 3, 2, 3)
    T ~ bb.sortRange(0, 3)          =**= Array[Byte](0, 2, 3, 2, 3)
    T ~ bb.sort()                   =**= Array[Byte](0, 2, 2, 3, 3)
    T ~ bb.fillRange(2, 4)(1)       =**= Array[Byte](0, 2, 1, 1, 3)
    T ~ bb.fillRange(Iv(2, 4))(5)   =**= Array[Byte](0, 2, 5, 5, 3)
    T ~ bb.fillRange(2 to End-1)(6) =**= Array[Byte](0, 2, 6, 6, 3)
    T ~ bb.fillRange(2 to 3)(7)     =**= Array[Byte](0, 2, 7, 7, 3)
    T ~ bb.fill(4)                  =**= Array[Byte](4, 4, 4, 4, 4)
    */

    object NuS extends NewType[Short] {}
    val as = Array[Short](1, 2, 3)
    val bs = Array[Short](2, 0, 3, 2, 3)
    val nas = Array[NuS.Type](NuS(1), NuS(2), NuS(3))
    T ~ as.copy                     =**= as
    T ~ (as.copy eq as)             ==== false
    T ~ nas.copy                    =**= nas
    T ~ as.copy.tap(_(0) = 4).toSeq =!!= as.toSeq
    T ~ as.copyToSize(2).length     ==== 2
    T ~ as.copyToSize(2)            =**= as.take(2)
    T ~ as.copyToSize(4)            =**= Array[Short](1, 2, 3, 0)
    T ~ as.copyToSize(End-1)        =**= as.take(2)
    T ~ as.copyToSize(End+1)        =**= Array[Short](1, 2, 3, 0)
    T ~ as.shrinkCopy(2)            =**= as.take(2)
    T ~ (as.shrinkCopy(4) eq as)    ==== true
    T ~ as.copyOfRange(1, 3)        =**= as.drop(1)
    T ~ as.copyOfRange(Iv(1, 3))    =**= as.drop(1)
    T ~ as.copyOfRange(1 to End)    =**= as.drop(1)
    T ~ as.copyOfRange(1 to 2)      =**= as.drop(1)
    T ~ bs.isSorted                 ==== false
    T ~ bs.isSortedRange(1, 3)      ==== true
    T ~ bs.isSortedRange(Iv(1, 3))  ==== true
    T ~ bs.isSortedRange(piv1Em2)   ==== true
    T ~ bs.isSortedRange(1 to 2)    ==== true
    T ~ as.search(2)                ==== 1
    T ~ as.search(0)                ==== -1
    T ~ bs.searchRange(1, 3)(3)     ==== 2
    T ~ bs.searchRange(1, 3)(2)     ==== -3
    T ~ bs.searchRange(Iv(1, 3))(3) ==== 2
    T ~ bs.searchRange(Iv(1, 3))(2) ==== -3
    T ~ bs.searchRange(piv1Em2)(3)  ==== 2
    T ~ bs.searchRange(piv1Em2)(2)  ==== -3
    T ~ bs.searchRange(1 to 2)(3)   ==== 2
    T ~ bs.searchRange(1 to 2)(2)   ==== -3
    /*
    T ~ bs.sortRange(0, 3)          =**= Array[Short](0, 2, 3, 2, 3)
    T ~ bs.copy.sortRange(0 to 2)   =**= Array[Short](0, 2, 3, 2, 3)
    T ~ bs.copy.sortRange(Iv(0,3))  =**= Array[Short](0, 2, 3, 2, 3)
    T ~ bs.copy.sortRange(piv0Em2)  =**= Array[Short](0, 2, 3, 2, 3)
    T ~ bs.sort()                   =**= Array[Short](0, 2, 2, 3, 3)
    T ~ bs.fillRange(2, 4)(1)       =**= Array[Short](0, 2, 1, 1, 3)
    T ~ bs.fillRange(Iv(2, 4))(5)   =**= Array[Short](0, 2, 5, 5, 3)
    T ~ bs.fillRange(2 to End-1)(6) =**= Array[Short](0, 2, 6, 6, 3)
    T ~ bs.fillRange(2 to 3)(7)     =**= Array[Short](0, 2, 7, 7, 3)
    T ~ bs.fill(4)                  =**= Array[Short](4, 4, 4, 4, 4)
    */

    val ac = Array[Char]('1', '2', '3')
    val bc = Array[Char]('2', '0', '3', '2', '3')
    T ~ ac.copy                       =**= ac
    T ~ (ac.copy eq ac)               ==== false
    T ~ ac.copy.tap(_(0) = '4').toSeq =!!= ac.toSeq
    T ~ ac.copyToSize(2).length       ==== 2
    T ~ ac.copyToSize(2)              =**= ac.take(2)
    T ~ ac.copyToSize(4)              =**= Array[Char]('1', '2', '3', '\u0000')
    T ~ ac.copyToSize(End-1)          =**= ac.take(2)
    T ~ ac.copyToSize(End+1)          =**= Array[Char]('1', '2', '3', '\u0000')
    T ~ ac.shrinkCopy(2)              =**= ac.take(2)
    T ~ (ac.shrinkCopy(4) eq ac)      ==== true
    T ~ ac.copyOfRange(1, 3)          =**= ac.drop(1)
    T ~ ac.copyOfRange(Iv(1, 3))      =**= ac.drop(1)
    T ~ ac.copyOfRange(1 to End)      =**= ac.drop(1)
    T ~ ac.copyOfRange(1 to 2)        =**= ac.drop(1)
    T ~ bc.isSorted                   ==== false
    T ~ bc.isSortedRange(1, 3)        ==== true
    T ~ bc.isSortedRange(Iv(1, 3))    ==== true
    T ~ bc.isSortedRange(piv1Em2)     ==== true
    T ~ bc.isSortedRange(1 to 2)      ==== true
    T ~ ac.search('2')                ==== 1
    T ~ ac.search('0')                ==== -1
    T ~ bc.searchRange(1, 3)('3')     ==== 2
    T ~ bc.searchRange(1, 3)('2')     ==== -3
    T ~ bc.searchRange(Iv(1,3))('3')  ==== 2
    T ~ bc.searchRange(Iv(1,3))('2')  ==== -3
    T ~ bc.searchRange(piv1Em2)('3')  ==== 2
    T ~ bc.searchRange(piv1Em2)('2')  ==== -3
    T ~ bc.searchRange(1 to 2)('3')   ==== 2
    T ~ bc.searchRange(1 to 2)('2')   ==== -3
    /*
    T ~ bc.copy.sortRange(0 to 2)     =**= Array[Char]('0', '2', '3', '2', '3')
    T ~ bc.copy.sortRange(Iv(0,3))    =**= Array[Char]('0', '2', '3', '2', '3')
    T ~ bc.copy.sortRange(piv0Em2)    =**= Array[Char]('0', '2', '3', '2', '3')
    T ~ bc.sortRange(0, 3)            =**= Array[Char]('0', '2', '3', '2', '3')
    T ~ bc.sort()                     =**= Array[Char]('0', '2', '2', '3', '3')
    T ~ bc.fillRange(2, 4)('e')       =**= Array[Char]('0', '2', 'e', 'e', '3')
    T ~ bc.fillRange(Iv(2, 4))('d')   =**= Array[Char]('0', '2', 'd', 'd', '3')
    T ~ bc.fillRange(2 to End-1)('c') =**= Array[Char]('0', '2', 'c', 'c', '3')
    T ~ bc.fillRange(2 to 3)('b')     =**= Array[Char]('0', '2', 'b', 'b', '3')
    T ~ bc.fill('4')                  =**= Array[Char]('4', '4', '4', '4', '4')
    */

    val ai = Array[Int](1, 2, 3)
    val bi = Array[Int](2, 0, 3, 2, 3)
    T ~ ai.copy                     =**= ai
    T ~ (ai.copy eq ai)             ==== false
    T ~ ai.copy.tap(_(0) = 4).toSeq =!!= ai.toSeq
    T ~ ai.copyToSize(2).length     ==== 2
    T ~ ai.copyToSize(2)            =**= ai.take(2)
    T ~ ai.copyToSize(4)            =**= Array[Int](1, 2, 3, 0)
    T ~ ai.copyToSize(End-1)        =**= ai.take(2)
    T ~ ai.copyToSize(End+1)        =**= Array[Int](1, 2, 3, 0)
    T ~ ai.shrinkCopy(2)            =**= ai.take(2)
    T ~ (ai.shrinkCopy(4) eq ai)    ==== true
    T ~ ai.copyOfRange(1, 3)        =**= ai.drop(1)
    T ~ ai.copyOfRange(Iv(1, 3))    =**= ai.drop(1)
    T ~ ai.copyOfRange(1 to End)    =**= ai.drop(1)
    T ~ ai.copyOfRange(1 to 2)      =**= ai.drop(1)
    T ~ Array(0x05030107).unpackBytes =**= Array[Byte](7, 1, 3, 5)
    T ~ bi.isSorted                 ==== false
    T ~ bi.isSortedRange(1, 3)      ==== true
    T ~ bi.isSortedRange(Iv(1, 3))  ==== true
    T ~ bi.isSortedRange(1 to End-2)==== true
    T ~ bi.isSortedRange(1 to 2)    ==== true
    T ~ ai.search(2)                ==== 1
    T ~ ai.search(0)                ==== -1
    T ~ bi.searchRange(1, 3)(3)     ==== 2
    T ~ bi.searchRange(1, 3)(2)     ==== -3
    T ~ bi.searchRange(Iv(1, 3))(3) ==== 2
    T ~ bi.searchRange(Iv(1, 3))(2) ==== -3
    T ~ bi.searchRange(piv1Em2)(3)  ==== 2
    T ~ bi.searchRange(piv1Em2)(2)  ==== -3
    T ~ bi.searchRange(1 to 2)(3)   ==== 2
    T ~ bi.searchRange(1 to 2)(2)   ==== -3
    /*
    T ~ bi.copy.sortRange(0 to 2)   =**= Array[Int](0, 2, 3, 2, 3)
    T ~ bi.copy.sortRange(Iv(0,3))  =**= Array[Int](0, 2, 3, 2, 3)
    T ~ bi.copy.sortRange(piv0Em2)  =**= Array[Int](0, 2, 3, 2, 3)
    T ~ bi.sortRange(0, 3)          =**= Array[Int](0, 2, 3, 2, 3)
    T ~ ai.copyOfRange(1, 3)        =**= ai.drop(1)
    T ~ bi.sort()                   =**= Array[Int](0, 2, 2, 3, 3)
    T ~ bi.fillRange(2, 4)(1)       =**= Array[Int](0, 2, 1, 1, 3)
    T ~ bi.fillRange(Iv(2, 4))(5)   =**= Array[Int](0, 2, 5, 5, 3)
    T ~ bi.fillRange(2 to End-1)(6) =**= Array[Int](0, 2, 6, 6, 3)
    T ~ bi.fillRange(2 to 3)(7)     =**= Array[Int](0, 2, 7, 7, 3)
    T ~ bi.fill(4)                  =**= Array[Int](4, 4, 4, 4, 4)
    */

    val al = Array[Long](1, 2, 3)
    val bl = Array[Long](2, 0, 3, 2, 3)
    T ~ al.copy                     =**= al
    T ~ (al.copy eq al)             ==== false
    T ~ al.copy.tap(_(0) = 4).toSeq =!!= al.toSeq
    T ~ al.copyToSize(2).length     ==== 2
    T ~ al.copyToSize(2)            =**= al.take(2)
    T ~ al.copyToSize(4)            =**= Array[Long](1, 2, 3, 0)
    T ~ al.copyToSize(End-1)        =**= al.take(2)
    T ~ al.copyToSize(End+1)        =**= Array[Long](1, 2, 3, 0)
    T ~ al.shrinkCopy(2)            =**= al.take(2)
    T ~ (al.shrinkCopy(4) eq al)    ==== true
    T ~ al.copyOfRange(1, 3)        =**= al.drop(1)
    T ~ al.copyOfRange(Iv(1, 3))    =**= al.drop(1)
    T ~ al.copyOfRange(1 to End)    =**= al.drop(1)
    T ~ al.copyOfRange(1 to 2)      =**= al.drop(1)
    T ~ Array(0x0102030405060708L).unpackBytes =**= Array[Byte](8, 7, 6, 5, 4, 3, 2, 1)
    T ~ bl.isSorted                 ==== false
    T ~ bl.isSortedRange(1, 3)      ==== true
    T ~ bl.isSortedRange(Iv(1, 3))  ==== true
    T ~ bl.isSortedRange(1 to End-2)==== true
    T ~ bl.isSortedRange(1 to 2)    ==== true
    T ~ al.search(2)                ==== 1
    T ~ al.search(0)                ==== -1
    T ~ bl.searchRange(1, 3)(3)     ==== 2
    T ~ bl.searchRange(1, 3)(2)     ==== -3
    T ~ bl.searchRange(Iv(1, 3))(3) ==== 2
    T ~ bl.searchRange(Iv(1, 3))(2) ==== -3
    T ~ bl.searchRange(piv1Em2)(3)  ==== 2
    T ~ bl.searchRange(piv1Em2)(2)  ==== -3
    T ~ bl.searchRange(1 to 2)(3)   ==== 2
    T ~ bl.searchRange(1 to 2)(2)   ==== -3
    /*
    T ~ bl.copy.sortRange(0 to 2)   =**= Array[Long](0, 2, 3, 2, 3)
    T ~ bl.copy.sortRange(Iv(0,3))  =**= Array[Long](0, 2, 3, 2, 3)
    T ~ bl.copy.sortRange(piv0Em2)  =**= Array[Long](0, 2, 3, 2, 3)
    T ~ bl.sortRange(0, 3)          =**= Array[Long](0, 2, 3, 2, 3)
    T ~ bl.sort()                   =**= Array[Long](0, 2, 2, 3, 3)
    T ~ bl.fillRange(2, 4)(1)       =**= Array[Long](0, 2, 1, 1, 3)
    T ~ bl.fillRange(Iv(2, 4))(5)   =**= Array[Long](0, 2, 5, 5, 3)
    T ~ bl.fillRange(2 to End-1)(6) =**= Array[Long](0, 2, 6, 6, 3)
    T ~ bl.fillRange(2 to 3)(7)     =**= Array[Long](0, 2, 7, 7, 3)
    T ~ bl.fill(4)                  =**= Array[Long](4, 4, 4, 4, 4)
    */

    val af = Array[Float](1, 2, 3)
    val bf = Array[Float](2, 0, 3, 2, 3)
    T ~ af.copy                     =**= af
    T ~ (af.copy eq af)             ==== false
    T ~ af.copy.tap(_(0) = 4).toSeq =!!= af.toSeq
    T ~ af.copyToSize(2).length     ==== 2
    T ~ af.copyToSize(2)            =**= af.take(2)
    T ~ af.copyToSize(4)            =**= Array[Float](1, 2, 3, 0)
    T ~ af.copyToSize(End-1)        =**= af.take(2)
    T ~ af.copyToSize(End+1)        =**= Array[Float](1, 2, 3, 0)
    T ~ af.shrinkCopy(2)            =**= af.take(2)
    T ~ (af.shrinkCopy(4) eq af)    ==== true
    T ~ af.copyOfRange(1, 3)        =**= af.drop(1)
    T ~ af.copyOfRange(Iv(1, 3))    =**= af.drop(1)
    T ~ af.copyOfRange(1 to End)    =**= af.drop(1)
    T ~ af.copyOfRange(1 to 2)      =**= af.drop(1)
    T ~ Array(1.4f).unpackBytes     =**= Array[Byte](51, 51, -77, 63)
    T ~ bf.isSorted                 ==== false
    T ~ bf.isSortedRange(1, 3)      ==== true
    T ~ bf.isSortedRange(Iv(1, 3))  ==== true
    T ~ bf.isSortedRange(1 to End-2)==== true
    T ~ bf.isSortedRange(1 to 2)    ==== true
    T ~ af.search(2)                ==== 1
    T ~ af.search(0)                ==== -1
    T ~ bf.searchRange(1, 3)(3)     ==== 2
    T ~ bf.searchRange(1, 3)(2)     ==== -3
    T ~ bf.searchRange(Iv(1, 3))(3) ==== 2
    T ~ bf.searchRange(Iv(1, 3))(2) ==== -3
    T ~ bf.searchRange(piv1Em2)(3)  ==== 2
    T ~ bf.searchRange(piv1Em2)(2)  ==== -3
    T ~ bf.searchRange(1 to 2)(3)   ==== 2
    T ~ bf.searchRange(1 to 2)(2)   ==== -3
    /*
    T ~ bf.copy.sortRange(0 to 2)   =**= Array[Float](0, 2, 3, 2, 3)
    T ~ bf.copy.sortRange(Iv(0,3))  =**= Array[Float](0, 2, 3, 2, 3)
    T ~ bf.copy.sortRange(piv0Em2)  =**= Array[Float](0, 2, 3, 2, 3)
    T ~ bf.sortRange(0, 3)          =**= Array[Float](0, 2, 3, 2, 3)
    T ~ bf.sort()                   =**= Array[Float](0, 2, 2, 3, 3)
    T ~ bf.fillRange(2, 4)(1)       =**= Array[Float](0, 2, 1, 1, 3)
    T ~ bf.fillRange(Iv(2, 4))(5)   =**= Array[Float](0, 2, 5, 5, 3)
    T ~ bf.fillRange(2 to End-1)(6) =**= Array[Float](0, 2, 6, 6, 3)
    T ~ bf.fillRange(2 to 3)(7)     =**= Array[Float](0, 2, 7, 7, 3)
    T ~ bf.fill(4)                  =**= Array[Float](4, 4, 4, 4, 4)
    */

    val ad = Array[Double](1, 2, 3)
    val bd = Array[Double](2, 0, 3, 2, 3)
    T ~ ad.copy                     =**= ad
    T ~ (ad.copy eq ad)             ==== false
    T ~ ad.copy.tap(_(0) = 4).toSeq =!!= ad.toSeq
    T ~ ad.copyToSize(2).length     ==== 2
    T ~ ad.copyToSize(2)            =**= ad.take(2)
    T ~ ad.copyToSize(4)            =**= Array[Double](1, 2, 3, 0)
    T ~ ad.copyToSize(End-1)        =**= ad.take(2)
    T ~ ad.copyToSize(End+1)        =**= Array[Double](1, 2, 3, 0)
    T ~ ad.shrinkCopy(2)            =**= ad.take(2)
    T ~ (ad.shrinkCopy(4) eq ad)    ==== true
    T ~ ad.copyOfRange(1, 3)        =**= ad.drop(1)
    T ~ ad.copyOfRange(Iv(1, 3))    =**= ad.drop(1)
    T ~ ad.copyOfRange(1 to End)    =**= ad.drop(1)
    T ~ ad.copyOfRange(1 to 2)      =**= ad.drop(1)
    T ~ Array(1.41).unpackBytes     =**= Array[Byte](-113, -62, -11, 40, 92, -113, -10, 63)
    T ~ bd.isSorted                 ==== false
    T ~ bd.isSortedRange(1, 3)      ==== true
    T ~ bd.isSortedRange(Iv(1, 3))  ==== true
    T ~ bd.isSortedRange(1 to End-2)==== true
    T ~ bd.isSortedRange(1 to 2)    ==== true
    T ~ ad.search(2)                ==== 1
    T ~ ad.search(0)                ==== -1
    T ~ bd.searchRange(1, 3)(3)     ==== 2
    T ~ bd.searchRange(1, 3)(2)     ==== -3
    T ~ bd.searchRange(Iv(1, 3))(3) ==== 2
    T ~ bd.searchRange(Iv(1, 3))(2) ==== -3
    T ~ bd.searchRange(piv1Em2)(3)  ==== 2
    T ~ bd.searchRange(piv1Em2)(2)  ==== -3
    T ~ bd.searchRange(1 to 2)(3)   ==== 2
    T ~ bd.searchRange(1 to 2)(2)   ==== -3
    /*
    T ~ bd.copy.sortRange(0 to 2)   =**= Array[Double](0, 2, 3, 2, 3)
    T ~ bd.copy.sortRange(Iv(0,3))  =**= Array[Double](0, 2, 3, 2, 3)
    T ~ bd.copy.sortRange(piv0Em2)  =**= Array[Double](0, 2, 3, 2, 3)
    T ~ bd.sortRange(0, 3)          =**= Array[Double](0, 2, 3, 2, 3)
    T ~ bd.sort()                   =**= Array[Double](0, 2, 2, 3, 3)
    T ~ bd.fillRange(2, 4)(1)       =**= Array[Double](0, 2, 1, 1, 3)
    T ~ bd.fillRange(Iv(2, 4))(5)   =**= Array[Double](0, 2, 5, 5, 3)
    T ~ bd.fillRange(2 to End-1)(6) =**= Array[Double](0, 2, 6, 6, 3)
    T ~ bd.fillRange(2 to 3)(7)     =**= Array[Double](0, 2, 7, 7, 3)
    T ~ bd.fill(4)                  =**= Array[Double](4, 4, 4, 4, 4)
    */

    object NuA extends NewType[String] {}
    val aa = Array[String]("1", "2", "3")
    val ba = Array[String]("2", "0", "3", "2", "3")
    val naa = Array[NuA.Type](NuA("1"), NuA("2"), NuA("3"))
    T ~ aa.copy                       =**= aa
    T ~ (aa.copy eq aa)               ==== false
    T ~ naa.copy                      =**= naa
    T ~ aa.copy.tap(_(0) = "4").toSeq =!!= aa.toSeq
    T ~ aa.copyToSize(2).length       ==== 2
    T ~ aa.copyToSize(2)              =**= aa.take(2)
    T ~ aa.copyToSize(4)              =**= Array[String]("1", "2", "3", null)
    T ~ aa.copyToSize(End-1)          =**= aa.take(2)
    T ~ aa.copyToSize(End+1)          =**= Array[String]("1", "2", "3", null)
    T ~ aa.shrinkCopy(2)              =**= aa.take(2)
    T ~ (aa.shrinkCopy(4) eq aa)      ==== true
    T ~ aa.copyOfRange(1, 3)          =**= aa.drop(1)
    T ~ aa.copyOfRange(Iv(1, 3))      =**= aa.drop(1)
    T ~ aa.copyOfRange(1 to End)      =**= aa.drop(1)
    T ~ aa.copyOfRange(1 to 2)        =**= aa.drop(1)
    T ~ ba.isSorted                   ==== false
    T ~ ba.isSortedRange(1, 3)        ==== true
    T ~ ba.isSortedRange(Iv(1, 3))    ==== true
    T ~ ba.isSortedRange(1 to End-2)  ==== true
    T ~ ba.isSortedRange(1 to 2)      ==== true
    T ~ aa.search("2")                ==== 1
    T ~ aa.search("0")                ==== -1
    T ~ ba.searchRange(1, 3)("3")     ==== 2
    T ~ ba.searchRange(1, 3)("2")     ==== -3
    T ~ ba.searchRange(Iv(1, 3))("3") ==== 2
    T ~ ba.searchRange(Iv(1, 3))("2") ==== -3
    T ~ ba.searchRange(piv1Em2)("3")  ==== 2
    T ~ ba.searchRange(piv1Em2)("2")  ==== -3
    T ~ ba.searchRange(1 to 2)("3")   ==== 2
    T ~ ba.searchRange(1 to 2)("2")   ==== -3
    /*
    T ~ ba.copy.sortRange(0 to 2)     =**= Array[String]("0", "2", "3", "2", "3")
    T ~ ba.copy.sortRange(Iv(0,3))    =**= Array[String]("0", "2", "3", "2", "3")
    T ~ ba.copy.sortRange(piv0Em2)    =**= Array[String]("0", "2", "3", "2", "3")
    T ~ ba.sortRange(0, 3)            =**= Array[String]("0", "2", "3", "2", "3")
    T ~ ba.sort()                     =**= Array[String]("0", "2", "2", "3", "3")
    T ~ ba.fillRange(2, 4)("e")       =**= Array[String]("0", "2", "e", "e", "3")
    T ~ ba.fillRange(Iv(2, 4))("d")   =**= Array[String]("0", "2", "d", "d", "3")
    T ~ ba.fillRange(2 to End-1)("c") =**= Array[String]("0", "2", "c", "c", "3")
    T ~ ba.fillRange(2 to 3)("b")     =**= Array[String]("0", "2", "b", "b", "3")
    T ~ ba.fill("4")                  =**= Array[String]("4", "4", "4", "4", "4")
    */


  def stringInlinedDataTest(): Unit =
    var cuml = 0
    val str = "ch.ix.#n."
    val arr = Array('c', 'h', '.', 'i', 'x', '.', '#', 'n', '.')
    val ix = Array(2, 3, 1, 1, 3)

    val civ = Iv(3, 5)
    val cpv = 3 to End-4

    def st = ix.stepper

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f: Unit
      cuml

    T ~ str.arr                             =**= arr
    T ~ arr.str                             ==== str
    T ~ str.maker().tap(_ += "##").toString ==== str + "##"
    T ~ str.make(_ += "##")                 ==== str + "##"

    T ~ str(1)     ==== 'h'
    T ~ str(End)   ==== '.'
    T ~ str(End-1) ==== 'n'

    T ~ z{ str.tap(_.use()(cuml += _)) }       ==== str
    T ~ cuml                                   ==== str.map(_.toInt).sum  
    T ~ z{ str.tap(_.use(3, 5)(cuml += _)) }   ==== str
    T ~ cuml                                   ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.tap(_.use(civ)(cuml += _)) }    ==== str
    T ~ cuml                                   ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.tap(_.use(3 to 4)(cuml += _)) } ==== str
    T ~ cuml                                   ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.tap(_.use(cpv)(cuml += _)) }    ==== str
    T ~ cuml                                   ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.tap(_.use(ix)(cuml += _)) }     ==== str
    T ~ cuml                                   ==== ".ihhi".map(_.toInt).sum
    T ~ z{ str.tap(_.use(st)(cuml += _)) }     ==== str
    T ~ cuml                                   ==== ".ihhi".map(_.toInt).sum

    T ~ n{ str.visit()(cuml += _ + _) }       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ n{ str.visit(3, 5)(cuml += _ + _) }   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.visit(civ)(cuml += _ + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.visit(3 to 4)(cuml += _ + _) } ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.visit(cpv)(cuml += _ + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.visit(ix)(cuml += _ + _) }     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ n{ str.visit(st)(cuml += _ + _) }     ==== ix.map(i => str(i).toInt).sum + ix.sum

    T ~ n{ str.pairs((x, y) => if x.isLetter && y.isLetter then cuml += 1) }                   ==== 2
    T ~ n{ str.trios((x, y, z) => if x.isLetter && !y.isLetter && z.isLetter then cuml += 1) } ==== 1

    T ~ n{ str.together(str)((s, t, i) => if s > 'c' && t < 'x' then cuml += 1+i) } ==== 14
    T ~ n{ str.together(arr)((s, a, i) => if s > 'c' && a < 'x' then cuml += 1+i) } ==== 14
    T ~ n{ str.together(str, str)((s, t, u, i) => if s > 'c' && t < 'x' && u != 'i' then cuml += 1+i) } ==== 10
    T ~ n{ str.together(str, arr)((s, t, a, i) => if s > 'c' && t < 'x' && a != 'i' then cuml += 1+i) } ==== 10
    T ~ n{ str.together(arr, str)((s, a, t, i) => if s > 'c' && t < 'x' && a != 'i' then cuml += 1+i) } ==== 10
    T ~ n{ str.together(arr, arr)((s, a, b, i) => if s > 'c' && a < 'x' && b != 'i' then cuml += 1+i) } ==== 10

    T ~ n{ str.wander(){  (c, i) => cuml += c; i+2 } } ==== str.grouped(2).map(_(0).toInt).sum
    T ~ n{ str.wander(1){ (c, i) => cuml += c; i+2 } } ==== str.grouped(2).filter(_.length == 2).map(_(1).toInt).sum
    T ~    str.wander(){  (_, i) =>            i+2 }   ==== str.grouped(2).map(_(0).toInt).length
    T ~    str.wander(1){ (_, i) =>            i+2 }   ==== str.grouped(2).filter(_.length == 2).map(_(1).toInt).length

    T ~ str.gather(0)()(_ + _ + _)       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ str.gather(0)(3, 5)(_ + _ + _)   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.gather(0)(civ)(_ + _ + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.gather(0)(3 to 4)(_ + _ + _) ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.gather(0)(cpv)(_ + _ + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.gather(0)(ix)(_ + _ + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ str.gather(0)(st)(_ + _ + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum

    T ~ str.copyWith(_.toUpper)                           ==== str.toUpperCase
    T ~ str.copyOp((c, i) => if i%2 == 0 then '-' else c) =**= "-h-i-.-n-".arr

    T ~ str.where()                   =**= str.arr.indices
    T ~ str.where(_.isLetter)         =**= str.zipWithIndex.collect{ case (c, i) if c.isLetter => i }
    T ~ str.whereIn(3, 5  )(_ == 'x') =**= Array(4)
    T ~ str.whereIn(3 to 4)(_ == 'x') =**= Array(4)
    T ~ str.whereIn(civ   )(_ == 'x') =**= Array(4)
    T ~ str.whereIn(cpv   )(_ == 'x') =**= Array(4)
    T ~ str.whereFrom(ix)(_ != 'i')   =**= Array(2, 1, 1)

    def linc(c: Char, i: Int) = if c.isLetter then i+7 else -1
    T ~ str.whereOp(linc)               =**= str.where(_.isLetter).copyWith(_ + 7)
    T ~ str.whereInOp(3, 8)(linc)       =**= str.whereIn(3, 8)(_.isLetter).copyWith(_ + 7)
    T ~ str.whereInOp(Iv(3, 8))(linc)   =**= str.whereIn(Iv(3, 8))(_.isLetter).copyWith(_ + 7)
    T ~ str.whereInOp(3 to End-2)(linc) =**= str.whereIn(3 to End-2)(_.isLetter).copyWith(_ + 7)
    T ~ str.whereInOp(3 to 7)(linc)     =**= str.whereIn(3 to 8)(_.isLetter).copyWith(_ + 7)
    T ~ str.whereFromOp(ix)(linc)       =**= str.whereFrom(ix)(_.isLetter).copyWith(_ + 7)

    val cx = "___________".toCharArray
    var ninja = 0
    T ~ cx.dup(a => ninja = str.inject(a)).str                ==== "ch.ix.#n.__"
    T ~ { val x = ninja; ninja = 0; x }                       ==== str.length
    T ~ cx.dup(a => ninja = str.inject(a, 2)).str             ==== "__ch.ix.#n."
    T ~ { val x = ninja; ninja = 0; x }                       ==== str.length
    T ~ cx.dup(a => ninja = str.inject(a)(3, 5)).str          ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(3, 5)).str       ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(3 to 4)).str        ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(3 to 4)).str     ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(3, 5)).str          ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(3, 5)).str       ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(civ)).str           ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(civ)).str        ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(cpv)).str           ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(cpv)).str        ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(ix)).str            ==== ".ihhi______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 5
    T ~ cx.dup(a => ninja = str.inject(a, 1)(ix)).str         ==== "_.ihhi_____"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 5
    T ~ cx.dup(a => ninja = str.inject(a)(st)).str            ==== ".ihhi______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 5
    T ~ cx.dup(a => ninja = str.inject(a, 1)(st)).str         ==== "_.ihhi_____"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 5
    T ~ cx.dup(a => ninja = str.inject(a)(_.isLetter)).str    ==== "chixn______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== str.count(_.isLetter)
    T ~ cx.dup(a => ninja = str.inject(a, 2)(_.isLetter)).str ==== "__chixn____"
    T ~ { val x = ninja; ninja = 0; x }                       ==== str.count(_.isLetter)

    val ax = "___________".arr.map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a)()((c, i) => c + i))              =**= "ci0l|3)u6__".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a, 2)()((c, i) => c + i))           =**= "__ci0l|3)u6".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a)(3, 5)((c, i) => c + i))          =**= "l|_________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a, 2)(3, 5)((c, i) => c + i))       =**= "__l|_______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a)(3 to 4)((c, i) => c + i))        =**= "l|_________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a, 2)(3 to 4)((c, i) => c + i))     =**= "__l|_______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a)(3, 5)((c, i) => c + i))          =**= "l|_________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a, 2)(3, 5)((c, i) => c + i))       =**= "__l|_______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a)(civ)((c, i) => c + i))           =**= "l|_________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a, 2)(civ)((c, i) => c + i))        =**= "__l|_______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a)(cpv)((c, i) => c + i))           =**= "l|_________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a, 2)(cpv)((c, i) => c + i))        =**= "__l|_______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a)(ix)((c, i) => c + i))            =**= "0liil______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a, 1)(ix)((c, i) => c + i))         =**= "_0liil_____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a)(st)((c, i) => c + i))            =**= "0liil______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a, 1)(st)((c, i) => c + i))         =**= "_0liil_____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a)(_.isLetter)((c, i) => c + i))    =**= "cil|u______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.injectOp(a, 2)(_.isLetter)((c, i) => c + i)) =**= "__cil|u____".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x } ==== 2*str.length + 10*2 + 4*5 + 2*str.count(_.isLetter)

    T ~ str.select(3, 5)       ==== "ix"
    T ~ str.select(3 to 4)     ==== "ix"
    T ~ str.select(civ)        ==== "ix"
    T ~ str.select(cpv)        ==== "ix"
    T ~ str.select(ix)         ==== ".ihhi"
    T ~ str.select(st)         ==== ".ihhi"
    T ~ str.select(_.isLetter) ==== "chixn"

    T ~ str.selectOp(3, 5)(   (c, i) => if i%2 == 0 then '-' else c) =**= "i-".arr
    T ~ str.selectOp(3 to 4)( (c, i) => if i%2 == 0 then '-' else c) =**= "i-".arr
    T ~ str.selectOp(civ)(    (c, i) => if i%2 == 0 then '-' else c) =**= "i-".arr
    T ~ str.selectOp(cpv)(    (c, i) => if i%2 == 0 then '-' else c) =**= "i-".arr
    T ~ str.selectOp(ix)(     (c, i) => if i%2 == 0 then '-' else c) =**= "-ihhi".arr
    T ~ str.selectOp(st)(     (c, i) => if i%2 == 0 then '-' else c) =**= "-ihhi".arr
    T ~ str.selectOp(_.isLetter)((c,i)=>if i%2 == 0 then '-' else c) =**= "-hi-n".arr

    T ~ str.fuse[Int]((c, i, add) => if !c.isLetter then add(i) else Array(O(Some(c.toString))).os.foreach(c => add(c.toInt))) =**= Array(99, 46, 104, 46, 2, 105, 46, 120, 46, 5, 6, 110, 46, 8)

    val test = "cheesefactories"
    val tidx = test.where(_ == 'e')
    val qidx = Array(9, 4, 3, 3, 4)
    T ~ test.diced(tidx)                       =**= Array("ch", "s", "factori", "s")
    T ~ test.diced(tidx, "", "endpoints")      =**= Array("ch", "s", "factori", "s")
    T ~ test.diced(tidx, "no endpoints")       =**= Array("s", "factori")
    T ~ test.diced(tidx, "", "no endpoints")   =**= Array("s", "factori")
    T ~ test.diced(tidx, "()")                 =**= Array("ch", "", "s", "factori", "s")
    T ~ test.diced(tidx, "()", "endpoints")    =**= Array("ch", "", "s", "factori", "s")
    T ~ test.diced(tidx, "()", "no endpoints") =**= Array("", "s", "factori")
    T ~ test.diced(tidx, "(]")                 =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.diced(tidx, "(]", "endpoints")    =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.diced(tidx, "(]", "no endpoints") =**= Array("e", "se", "factorie")
    T ~ test.diced(tidx, "[)")                 =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.diced(tidx, "[)", "endpoints")    =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.diced(tidx, "[)", "no endpoints") =**= Array("e", "es", "efactori")
    T ~ test.diced(tidx, "[]")                 =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.diced(tidx, "[]", "endpoints")    =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.diced(tidx, "[]", "no endpoints") =**= Array("ee", "ese", "efactorie")
    T ~ test.diced(qidx)                       =**= Array("cheesefac", "cafe", "efactories")
    T ~ test.diced(qidx, "()")                 =**= Array("cheesefac", "cafe", "", "", "", "efactories")
    T ~ test.diced(qidx, "(]")                 =**= Array("cheesefact", "cafes", "e", "", "s", "efactories")
    T ~ test.diced(qidx, "[)")                 =**= Array("cheesefac", "tcafe", "s", "", "e", "sefactories")
    T ~ test.diced(qidx, "[]")                 =**= Array("cheesefact", "tcafes", "se", "e", "es", "sefactories")
    T ~ "".diced(Array.empty[Int]).length      ==== 0
    T ~ "".diced(Array.empty[Int], "()")       =**= Array("")
    T ~ "".diced(Array.empty[Int], "(]")       =**= Array("")
    T ~ "".diced(Array.empty[Int], "[)")       =**= Array("")
    T ~ "".diced(Array.empty[Int], "[]")       =**= Array("")
    T ~ test.diced(_ == 'e')                   =**= Array("ch", "s", "factori", "s")
    T ~ test.diced(_ == 'e', "")               =**= Array("ch", "s", "factori", "s")
    T ~ test.diced(_ == 'e', "()")             =**= Array("ch", "", "s", "factori", "s")
    T ~ test.diced(_ == 'e', "(]")             =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.diced(_ == 'e', "[)")             =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.diced(_ == 'e', "[]")             =**= Array("che", "ee", "ese", "efactorie", "es")


  def stringClippedInlinedDataTest(): Unit =
    var cuml = 0
    val str = "ch.#ik."

    val ix = Array(2, 3, 1, 1, 3)
    val ex = Array(2, 0, -1, 3, 6, 7, -9, 400) 
    def st = ix.stepper
    def et = ex.stepper

    val civ = Iv(3, 5)
    val cpv = 3 to End-2
    val eiv = Iv(3, 9)
    val fiv = Iv(-2, 5)
    val fpv = End-9 to End-2
    val biv = Iv(-2, 9)
    val niv = Iv(8, 9)
    val npv = End-9 to End-8

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f: Unit
      cuml

    T ~ z{ str.tap(_.clip.use(3, 5)(cuml += _)) }    ==== str
    T ~ cuml                                         ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.tap(_.clip.use(3 to 4)(cuml += _)) }  ==== str
    T ~ cuml                                         ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.tap(_.clip.use(civ)(cuml += _)) }     ==== str
    T ~ cuml                                         ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.tap(_.clip.use(cpv)(cuml += _)) }     ==== str
    T ~ cuml                                         ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.tap(_.clip.use(ix)(cuml += _)) }      ==== str
    T ~ cuml                                         ==== ".#hh#".map(_.toInt).sum
    T ~ z{ str.tap(_.clip.use(st)(cuml += _)) }      ==== str
    T ~ cuml                                         ==== ".#hh#".map(_.toInt).sum

    T ~ n{ str.tap(_.clip.use(3, 9)(cuml += _)) }    ==== str.substring(3).map(_.toInt).sum
    T ~ n{ str.tap(_.clip.use(3 to 8)(cuml += _)) }  ==== str.substring(3).map(_.toInt).sum
    T ~ n{ str.tap(_.clip.use(eiv)(cuml += _)) }     ==== str.substring(3).map(_.toInt).sum

    T ~ n{ str.tap(_.clip.use(-2, 5)(cuml += _)) }   ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ str.tap(_.clip.use(-2 to 4)(cuml += _)) } ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ str.tap(_.clip.use(fiv)(cuml += _)) }     ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ str.tap(_.clip.use(fpv)(cuml += _)) }     ==== str.substring(0, 5).map(_.toInt).sum

    T ~ n{ str.tap(_.clip.use(-2, 9)(cuml += _)) }   ==== str.map(_.toInt).sum
    T ~ n{ str.tap(_.clip.use(-2 to 9)(cuml += _)) } ==== str.map(_.toInt).sum
    T ~ n{ str.tap(_.clip.use(biv)(cuml += _)) }     ==== str.map(_.toInt).sum

    T ~ n{ str.tap(_.clip.use(8, 9)(cuml += _)) }    ==== 0
    T ~ n{ str.tap(_.clip.use(8 to 9)(cuml += _)) }  ==== 0
    T ~ n{ str.tap(_.clip.use(niv)(cuml += _)) }     ==== 0
    T ~ n{ str.tap(_.clip.use(npv)(cuml += _)) }     ==== 0

    T ~ n{ str.tap(_.clip.use(ex)(cuml += _)) }      ==== ".c#.".map(_.toInt).sum
    T ~ n{ str.tap(_.clip.use(et)(cuml += _)) }      ==== ".c#.".map(_.toInt).sum

    def sm(i: Int, j: Int) = j*(j+1)/2 - i*(i-1)/2
    T ~ n{ str.clip.visit(3, 5)(cuml += _ + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.clip.visit(3 to 4)(cuml += _ + _) }  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.clip.visit(civ)(cuml += _ + _) }     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.clip.visit(cpv)(cuml += _ + _) }     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.clip.visit(ix)(cuml += _ + _) }      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ n{ str.clip.visit(st)(cuml += _ + _) }      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ n{ str.clip.visit(3, 9)(cuml += _ + _) }    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ n{ str.clip.visit(3 to 8)(cuml += _ + _) }  ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ n{ str.clip.visit(eiv)(cuml += _ + _) }     ==== str.substring(3).map(_.toInt).sum + sm(3, 6)

    T ~ n{ str.clip.visit(-2, 5)(cuml += _ + _) }   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ str.clip.visit(-2 to 4)(cuml += _ + _) } ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ str.clip.visit(fiv)(cuml += _ + _) }     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ str.clip.visit(fpv)(cuml += _ + _) }     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ str.clip.visit(-2, 9)(cuml += _ + _) }   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ n{ str.clip.visit(-2 to 9)(cuml += _ + _) } ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ n{ str.clip.visit(biv)(cuml += _ + _) }     ==== str.map(_.toInt).sum + sm(0, 6)

    T ~ n{ str.clip.visit(8, 9)(cuml += _ + _) }    ==== 0
    T ~ n{ str.clip.visit(8 to 9)(cuml += _ + _) }  ==== 0
    T ~ n{ str.clip.visit(niv)(cuml += _ + _) }     ==== 0
    T ~ n{ str.clip.visit(npv)(cuml += _ + _) }     ==== 0

    T ~ n{ str.clip.visit(ex)(cuml += _ + _) }      ==== ".c#.".map(_.toInt).sum + 11
    T ~ n{ str.clip.visit(et)(cuml += _ + _) }      ==== ".c#.".map(_.toInt).sum + 11

    T ~ str.clip.gather(0)(3, 5)(_ + _ + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.clip.gather(0)(3 to 4)(_ + _ + _)  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.clip.gather(0)(civ)(_ + _ + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.clip.gather(0)(cpv)(_ + _ + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.clip.gather(0)(ix)(_ + _ + _)      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ str.clip.gather(0)(st)(_ + _ + _)      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ str.clip.gather(0)(3, 9)(_ + _ + _)    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ str.clip.gather(0)(3 to 8)(_ + _ + _)  ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ str.clip.gather(0)(eiv)(_ + _ + _)     ==== str.substring(3).map(_.toInt).sum + sm(3, 6)

    T ~ str.clip.gather(0)(-2, 5)(_ + _ + _)   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ str.clip.gather(0)(-2 to 4)(_ + _ + _) ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ str.clip.gather(0)(fiv)(_ + _ + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ str.clip.gather(0)(fpv)(_ + _ + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)

    T ~ str.clip.gather(0)(-2, 9)(_ + _ + _)   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ str.clip.gather(0)(-2 to 9)(_ + _ + _) ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ str.clip.gather(0)(biv)(_ + _ + _)     ==== str.map(_.toInt).sum + sm(0, 6)

    T ~ str.clip.gather(0)(8, 9)(_ + _ + _)    ==== 0
    T ~ str.clip.gather(0)(8 to 9)(_ + _ + _)  ==== 0
    T ~ str.clip.gather(0)(niv)(_ + _ + _)     ==== 0
    T ~ str.clip.gather(0)(npv)(_ + _ + _)     ==== 0

    T ~ str.clip.gather(0)(ex)(_ + _ + _)      ==== ".c#.".map(_.toInt).sum + 11
    T ~ str.clip.gather(0)(et)(_ + _ + _)      ==== ".c#.".map(_.toInt).sum + 11

    T ~ str.clip.whereIn(3, 9)(_.isLetter)          =**= Array(4, 5)
    T ~ str.clip.whereIn(3 to 8)(_.isLetter)        =**= Array(4, 5)
    T ~ str.clip.whereIn(eiv)(_.isLetter)           =**= Array(4, 5)
    T ~ str.clip.whereIn(-2, 5)(_.isLetter)         =**= Array(0, 1, 4)
    T ~ str.clip.whereIn(-2 to 4)(_.isLetter)       =**= Array(0, 1, 4)
    T ~ str.clip.whereIn(fiv)(_.isLetter)           =**= Array(0, 1, 4)
    T ~ str.clip.whereIn(fpv)(_.isLetter)           =**= Array(0, 1, 4)
    T ~ str.clip.whereIn(-2, 9)(_.isLetter)         =**= Array(0, 1, 4, 5)
    T ~ str.clip.whereIn(-2 to 8)(_.isLetter)       =**= Array(0, 1, 4, 5)
    T ~ str.clip.whereIn(biv)(_.isLetter)           =**= Array(0, 1, 4, 5)
    T ~ str.clip.whereIn(8, 9)(_.isLetter).length   ==== 0
    T ~ str.clip.whereIn(8 to 9)(_.isLetter).length ==== 0
    T ~ str.clip.whereIn(niv)(_.isLetter).length    ==== 0
    T ~ str.clip.whereIn(npv)(_.isLetter).length    ==== 0
    T ~ str.clip.whereFrom(ex)(! _.isLetter)        =**= Array(2, 3, 6)

    def linc(c: Char, i: Int) = if c.isLetter then i+7 else -1
    T ~ str.clip.whereInOp(3, 9)(linc)           =**= str.clip.whereIn(3, 9)(_.isLetter).copyWith(_ + 7)
    T ~ str.clip.whereInOp(Iv(3, 9))(linc)       =**= str.clip.whereIn(Iv(3, 9))(_.isLetter).copyWith(_ + 7)
    T ~ str.clip.whereInOp(3 to 8)(linc)         =**= str.clip.whereIn(3 to 8)(_.isLetter).copyWith(_ + 7)
    T ~ str.clip.whereInOp(-2, 5)(linc)          =**= str.clip.whereIn(-2, 5)(_.isLetter).copyWith(_ + 7)
    T ~ str.clip.whereInOp(Iv(-2, 5))(linc)      =**= str.clip.whereIn(Iv(-2, 5))(_.isLetter).copyWith(_ + 7)
    T ~ str.clip.whereInOp(End-9 to End-2)(linc) =**= str.clip.whereIn(End-9 to End-2)(_.isLetter).copyWith(_ + 7)
    T ~ str.clip.whereFromOp(ex)(linc)           =**= str.clip.whereFrom(ex)(_.isLetter).copyWith(_ + 7)

    val ca9 = "ABCDEFGHI".arr
    val ca7 = "1234567".arr
    val ca3 = "890".arr
    val ca1 = "%".arr

    var ninja = 0
    T ~ ca9.dup(a => ninja += str.clip.inject(a)).str            ==== "ch.#ik.HI"
    T ~ ca9.dup(a => ninja += str.clip.inject(a, 2)).str         ==== "ABch.#ik."
    T ~ { val x = ninja; ninja = 0; x }                          ==== 2*str.length
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(3, 5)).str      ==== "#i34567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(3, 5)).str   ==== "12#i567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(3 to 4)).str    ==== "#i34567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(3 to 4)).str ==== "12#i567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(civ)).str       ==== "#i34567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(civ)).str    ==== "12#i567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(cpv)).str       ==== "#i34567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(cpv)).str    ==== "12#i567"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 8*2
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(ix)).str        ==== ".#hh#67"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(ix)).str     ==== "12.#hh#"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(st)).str        ==== ".#hh#67"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(st)).str     ==== "12.#hh#"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 4*5

    T ~ ca3.dup(a => ninja += str.clip.inject(a)).str    ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)).str ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)).str ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 3+5+0

    T ~ ca1.dup(a => ninja += str.clip.inject(a)(3, 5)).str      ==== "#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(3, 5)).str   ==== "89#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(3, 5)).str   ==== "890"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(3 to 4)).str    ==== "#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(3 to 4)).str ==== "89#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(3 to 4)).str ==== "890"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(civ)).str       ==== "#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(civ)).str    ==== "89#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(civ)).str    ==== "890"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(cpv)).str       ==== "#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(cpv)).str    ==== "89#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(cpv)).str    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 4*(1+1+0)

    T ~ ca3.dup(a => ninja += str.clip.inject(a)(3, 9)).str      ==== "#ik"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 5)(3, 9)).str   ==== "12345#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(3, 9)).str   ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(3 to 8)).str    ==== "#ik"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 5)(3 to 8)).str ==== "12345#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(3 to 8)).str ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(eiv)).str       ==== "#ik"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 5)(eiv)).str    ==== "12345#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(eiv)).str    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 3*(3+2+0)

    T ~ ca3.dup(a => ninja += str.clip.inject(a)(-2, 5)).str      ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 3)(-2, 5)).str   ==== "123ch.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(-2, 5)).str   ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(-2 to 4)).str    ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 3)(-2 to 4)).str ==== "123ch.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(-2 to 4)).str ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(fiv)).str        ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 3)(fiv)).str     ==== "123ch.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(fiv)).str     ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(fpv)).str        ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 3)(fpv)).str     ==== "123ch.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(fpv)).str     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                           ==== 4*(3+4+0)

    T ~ ca3.dup(a => ninja += str.clip.inject(a)(-2, 9)).str      ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(-2, 9)).str   ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(-2, 9)).str   ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(-2 to 8)).str    ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(-2 to 8)).str ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(-2 to 8)).str ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(biv)).str        ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(biv)).str     ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(biv)).str     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                           ==== 3*(3+5+0)

    T ~ ca1.dup(a => ninja += str.clip.inject(a)(8, 10)).str     ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a, 2)(8, 10)).str  ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(8 to 9)).str    ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a, 2)(8 to 9)).str ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(niv)).str       ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a, 2)(niv)).str    ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(npv)).str       ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a, 2)(npv)).str    ==== "%"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 0

    T ~ ca3.dup(a => ninja += str.clip.inject(a)(ix)).str     ==== ".#h"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 1)(ix)).str  ==== "8.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(ix)).str  ==== "890"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, -1)(ix)).str ==== ".#hh#67"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(st)).str     ==== ".#h"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 1)(st)).str  ==== "8.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(st)).str  ==== "890"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, -1)(st)).str ==== ".#hh#67"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2*(3+2+0+5)    
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(ex)).str     ==== ".c#.567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 4)(ex)).str  ==== "1234.c#"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 9)(ex)).str  ==== "1234567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, -1)(ex)).str ==== ".c#.567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(et)).str     ==== ".c#.567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 4)(et)).str  ==== "1234.c#"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 9)(et)).str  ==== "1234567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, -1)(et)).str ==== ".c#.567"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2*(4+3+0+4)

    T ~ ca7.dup(a => ninja += str.clip.inject(a)(_.isLetter)).str    ==== "chik567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(_.isLetter)).str ==== "12chik7"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(_.isLetter)).str    ==== "chi"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(_.isLetter)).str ==== "89c"
    T ~ { val x = ninja; ninja = 0; x }                              ==== 2*4 + 3 + 1

    val aa9 = "ABCDEFGHI".arr.map(_.toInt)
    val aa7 = "1234567".arr.map(_.toInt)
    val aa3 = "890".arr.map(_.toInt)
    val aa1 = "%".arr.map(_.toInt)
    T ~ aa9.dup(a => ninja += str.clip.injectOp(a)()((c, i) => c + i))              =**= "ci0&mp4HI".map(_.toInt)
    T ~ aa9.dup(a => ninja += str.clip.injectOp(a, 2)()((c, i) => c + i))           =**= "ABci0&mp4".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 2*str.length
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a)(3, 5)((c, i) => c + i))          =**= "&m34567".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(3, 5)((c, i) => c + i))       =**= "12&m567".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a)(3 to 4)((c, i) => c + i))        =**= "&m34567".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(3 to 4)((c, i) => c + i))     =**= "12&m567".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a)(civ)((c, i) => c + i))           =**= "&m34567".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(civ)((c, i) => c + i))        =**= "12&m567".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a)(cpv)((c, i) => c + i))           =**= "&m34567".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(cpv)((c, i) => c + i))        =**= "12&m567".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 8*2
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a)(ix)((c, i) => c + i))            =**= "0&ii&67".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(ix)((c, i) => c + i))         =**= "120&ii&".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a)(st)((c, i) => c + i))            =**= "0&ii&67".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(st)((c, i) => c + i))         =**= "120&ii&".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 4*5

    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)()((c, i) => c + i))              =**= "ci0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)()((c, i) => c + i))           =**= "12ci0&m".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)()((c, i) => c + i))           =**= "890".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 3+5+0

    T ~ aa1.dup(a => ninja += str.clip.injectOp(a)(3, 5)((c, i) => c + i))          =**= "&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 2)(3, 5)((c, i) => c + i))       =**= "89&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(3, 5)((c, i) => c + i))       =**= "890".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a)(3 to 4)((c, i) => c + i))        =**= "&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 2)(3 to 4)((c, i) => c + i))     =**= "89&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(3 to 4)((c, i) => c + i))     =**= "890".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a)(civ)((c, i) => c + i))           =**= "&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 2)(civ)((c, i) => c + i))        =**= "89&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(civ)((c, i) => c + i))        =**= "890".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a)(cpv)((c, i) => c + i))           =**= "&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 2)(cpv)((c, i) => c + i))        =**= "89&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(cpv)((c, i) => c + i))        =**= "890".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 4*(1+1+0)

    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(3, 9)((c, i) => c + i))          =**= "&mp".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 5)(3, 9)((c, i) => c + i))       =**= "12345&m".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(3, 9)((c, i) => c + i))       =**= "890".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(3 to 8)((c, i) => c + i))        =**= "&mp".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 5)(3 to 8)((c, i) => c + i))     =**= "12345&m".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(3 to 8)((c, i) => c + i))     =**= "890".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(eiv)((c, i) => c + i))           =**= "&mp".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 5)(eiv)((c, i) => c + i))        =**= "12345&m".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(eiv)((c, i) => c + i))        =**= "890".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 3*(3+2+0)

    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(-2, 5)((c, i) => c + i))          =**= "ci0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 3)(-2, 5)((c, i) => c + i))       =**= "123ci0&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(-2, 5)((c, i) => c + i))       =**= "890".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(-2 to 4)((c, i) => c + i))        =**= "ci0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 3)(-2 to 4)((c, i) => c + i))     =**= "123ci0&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(-2 to 4)((c, i) => c + i))     =**= "890".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(fiv)((c, i) => c + i))            =**= "ci0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 3)(fiv)((c, i) => c + i))         =**= "123ci0&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(fiv)((c, i) => c + i))         =**= "890".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(fpv)((c, i) => c + i))            =**= "ci0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 3)(fpv)((c, i) => c + i))         =**= "123ci0&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(fpv)((c, i) => c + i))         =**= "890".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                              ==== 4*(3+4+0)

    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(-2, 9)((c, i) => c + i))          =**= "ci0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(-2, 9)((c, i) => c + i))       =**= "12ci0&m".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(-2, 9)((c, i) => c + i))       =**= "890".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(-2 to 8)((c, i) => c + i))        =**= "ci0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(-2 to 8)((c, i) => c + i))     =**= "12ci0&m".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(-2 to 8)((c, i) => c + i))     =**= "890".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(biv)((c, i) => c + i))            =**= "ci0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(biv)((c, i) => c + i))         =**= "12ci0&m".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(biv)((c, i) => c + i))         =**= "890".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                              ==== 3*(3+5+0)

    T ~ aa1.dup(a => ninja += str.clip.injectOp(a)(8, 10)((c, i) => c + i))         =**= "%".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a, 2)(8, 10)((c, i) => c + i))      =**= "%".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a)(8 to 9)((c, i) => c + i))        =**= "%".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a, 2)(8 to 9)((c, i) => c + i))     =**= "%".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a)(niv)((c, i) => c + i))           =**= "%".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a, 2)(niv)((c, i) => c + i))        =**= "%".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a)(npv)((c, i) => c + i))           =**= "%".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.injectOp(a, 2)(npv)((c, i) => c + i))        =**= "%".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 0

    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(ix)((c, i) => c + i))            =**= "0&i".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 1)(ix)((c, i) => c + i))         =**= "80&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(ix)((c, i) => c + i))         =**= "890".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, -1)(ix)((c, i) => c + i))        =**= "0&ii&67".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(st)((c, i) => c + i))            =**= "0&i".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 1)(st)((c, i) => c + i))         =**= "80&".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 4)(st)((c, i) => c + i))         =**= "890".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, -1)(st)((c, i) => c + i))        =**= "0&ii&67".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 2*(3+2+0+5)    
    val ab7 = "abcdefg".arr.map(_.toInt)
    T ~ ab7.dup(a => ninja += str.clip.injectOp(a)(ex)((c, i) => c + i))            =**= "0c&4efg".map(_.toInt)
    T ~ ab7.dup(a => ninja += str.clip.injectOp(a, 4)(ex)((c, i) => c + i))         =**= "abcd0c&".map(_.toInt)
    T ~ ab7.dup(a => ninja += str.clip.injectOp(a, 9)(ex)((c, i) => c + i))         =**= "abcdefg".map(_.toInt)
    T ~ ab7.dup(a => ninja += str.clip.injectOp(a, -1)(ex)((c, i) => c + i))        =**= "0c&4efg".map(_.toInt)
    T ~ ab7.dup(a => ninja += str.clip.injectOp(a)(et)((c, i) => c + i))            =**= "0c&4efg".map(_.toInt)
    T ~ ab7.dup(a => ninja += str.clip.injectOp(a, 4)(et)((c, i) => c + i))         =**= "abcd0c&".map(_.toInt)
    T ~ ab7.dup(a => ninja += str.clip.injectOp(a, 9)(et)((c, i) => c + i))         =**= "abcdefg".map(_.toInt)
    T ~ ab7.dup(a => ninja += str.clip.injectOp(a, -1)(et)((c, i) => c + i))        =**= "0c&4efg".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 2*(4+3+0+4)

    T ~ aa7.dup(a => ninja += str.clip.injectOp(a)(_.isLetter)((c, i) => c + i))    =**= "cimp567".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.injectOp(a, 2)(_.isLetter)((c, i) => c + i)) =**= "12cimp7".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a)(_.isLetter)((c, i) => c + i))    =**= "cim".map(_.toInt)
    T ~ aa3.dup(a => ninja += str.clip.injectOp(a, 2)(_.isLetter)((c, i) => c + i)) =**= "89c".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x }                                             ==== 2*4 + 3 + 1

    T ~ str.clip.select(3, 5)   ==== "#i"
    T ~ str.clip.select(3 to 4) ==== "#i"
    T ~ str.clip.select(civ)    ==== "#i"
    T ~ str.clip.select(cpv)    ==== "#i"
    T ~ str.clip.select(ix)     ==== ".#hh#"
    T ~ str.clip.select(st)     ==== ".#hh#"

    T ~ str.clip.select(3, 9)   ==== "#ik."
    T ~ str.clip.select(3 to 8) ==== "#ik."
    T ~ str.clip.select(eiv)    ==== "#ik."

    T ~ str.clip.select(-2, 5)   ==== "ch.#i"
    T ~ str.clip.select(-2 to 4) ==== "ch.#i"
    T ~ str.clip.select(fiv)     ==== "ch.#i"
    T ~ str.clip.select(fpv)     ==== "ch.#i"

    T ~ str.clip.select(-2, 9)   ==== "ch.#ik."
    T ~ str.clip.select(-2 to 8) ==== "ch.#ik."
    T ~ str.clip.select(biv)     ==== "ch.#ik."

    T ~ str.clip.select(8, 10)  ==== ""
    T ~ str.clip.select(8 to 9) ==== ""
    T ~ str.clip.select(niv)    ==== ""
    T ~ str.clip.select(npv)    ==== ""

    T ~ str.clip.select(et)    ==== ".c#."
    T ~ str.clip.select(et)    ==== ".c#."

    T ~ str.clip.selectOp(3, 5  )((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.selectOp(3 to 4)((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.selectOp(civ   )((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.selectOp(cpv   )((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.selectOp(ix    )((c, i) => c + i) =**= "0&ii&".map(_.toInt)
    T ~ str.clip.selectOp(st    )((c, i) => c + i) =**= "0&ii&".map(_.toInt)
    T ~ str.clip.selectOp(3, 5  )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(3 to 4)((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(civ   )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(cpv   )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(ix    )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(st    )((c, i) => c + i) ==== typed[Array[Int]]

    T ~ str.clip.selectOp(3, 9  )((c, i) => c + i) =**= "&mp4".map(_.toInt)
    T ~ str.clip.selectOp(3 to 8)((c, i) => c + i) =**= "&mp4".map(_.toInt)
    T ~ str.clip.selectOp(eiv   )((c, i) => c + i) =**= "&mp4".map(_.toInt)

    T ~ str.clip.selectOp(-2, 5  )((c, i) => c + i) =**= "ci0&m".map(_.toInt)
    T ~ str.clip.selectOp(-2 to 4)((c, i) => c + i) =**= "ci0&m".map(_.toInt)
    T ~ str.clip.selectOp(fiv    )((c, i) => c + i) =**= "ci0&m".map(_.toInt)
    T ~ str.clip.selectOp(fpv    )((c, i) => c + i) =**= "ci0&m".map(_.toInt)

    T ~ str.clip.selectOp(-2, 9  )((c, i) => c + i) =**= "ci0&mp4".map(_.toInt)
    T ~ str.clip.selectOp(-2 to 8)((c, i) => c + i) =**= "ci0&mp4".map(_.toInt)
    T ~ str.clip.selectOp(biv    )((c, i) => c + i) =**= "ci0&mp4".map(_.toInt)

    T ~ str.clip.selectOp(8, 10 )((c, i) => c + i) =**= "".map(_.toInt)
    T ~ str.clip.selectOp(8 to 9)((c, i) => c + i) =**= "".map(_.toInt)
    T ~ str.clip.selectOp(niv   )((c, i) => c + i) =**= "".map(_.toInt)
    T ~ str.clip.selectOp(npv   )((c, i) => c + i) =**= "".map(_.toInt)

    T ~ str.clip.selectOp(ex)((c, i) => c + i) =**= "0c&4".map(_.toInt)
    T ~ str.clip.selectOp(et)((c, i) => c + i) =**= "0c&4".map(_.toInt)

    val test = "cheesefactories"
    val tidx = test.where(_ == 'e')
    val qidx = Array(
      9, 4, 3, 3, 4,
      20, 20, 21, Int.MaxValue-1, Int.MaxValue, Int.MaxValue, Int.MaxValue-1, Int.MaxValue,
      Int.MinValue, Int.MinValue+1, Int.MinValue, Int.MinValue, 
      0, -1, 0, -1, 1, -1000,
      Int.MaxValue, test.length-1, test.length, test.length-1, test.length, test.length-2
    )
    T ~ test.clip.diced(tidx)                       =**= Array("ch", "s", "factori", "s")
    T ~ test.clip.diced(tidx, "", "endpoints")      =**= Array("ch", "s", "factori", "s")
    T ~ test.clip.diced(tidx, "no endpoints")       =**= Array("s", "factori")
    T ~ test.clip.diced(tidx, "", "no endpoints")   =**= Array("s", "factori")
    T ~ test.clip.diced(tidx, "()")                 =**= Array("ch", "", "s", "factori", "s")
    T ~ test.clip.diced(tidx, "()", "endpoints")    =**= Array("ch", "", "s", "factori", "s")
    T ~ test.clip.diced(tidx, "()", "no endpoints") =**= Array("", "s", "factori")
    T ~ test.clip.diced(tidx, "(]")                 =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.clip.diced(tidx, "(]", "endpoints")    =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.clip.diced(tidx, "(]", "no endpoints") =**= Array("e", "se", "factorie")
    T ~ test.clip.diced(tidx, "[)")                 =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.clip.diced(tidx, "[)", "endpoints")    =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.clip.diced(tidx, "[)", "no endpoints") =**= Array("e", "es", "efactori")
    T ~ test.clip.diced(tidx, "[]")                 =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.clip.diced(tidx, "[]", "endpoints")    =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.clip.diced(tidx, "[]", "no endpoints") =**= Array("ee", "ese", "efactorie")
    T ~ test.clip.diced(qidx)                       =**= Array("cheesefac", "cafe", "efactories", "seirotcafeseehc", "c", "c", "cheesefactories", "s", "s")
    T ~ test.clip.diced(qidx, "()")                 =**= Array("cheesefac", "cafe", "", "", "", "efactories", "seirotcafeseehc", "", "", "", "", "c", "c", "cheesefactories", "", "", "", "", "s", "s")
    T ~ test.clip.diced(qidx, "(]")                 =**= Array("cheesefact", "cafes", "e", "", "s", "efactories", "seirotcafeseehc", "c", "", "c", "", "ch", "c", "cheesefactories", "s", "", "s", "", "se", "s")
    T ~ test.clip.diced(qidx, "[)")                 =**= Array("cheesefac", "tcafe", "s", "", "e", "sefactories", "seirotcafeseehc", "", "c", "", "c", "c", "hc", "cheesefactories", "", "s", "", "s", "s", "es")
    T ~ test.clip.diced(qidx, "[]")                 =**= Array("cheesefact", "tcafes", "se", "e", "es", "sefactories", "seirotcafeseehc", "c", "c", "c", "c", "ch", "hc", "cheesefactories", "s", "s", "s", "s", "se", "es")
    T ~ "".clip.diced(Array.empty[Int]).length      ==== 0
    T ~ "".clip.diced(Array.empty[Int], "()")       =**= Array("")
    T ~ "".clip.diced(Array.empty[Int], "(]")       =**= Array("")
    T ~ "".clip.diced(Array.empty[Int], "[)")       =**= Array("")
    T ~ "".clip.diced(Array.empty[Int], "[]")       =**= Array("")


  def stringBreakInlinedDataTest(): Unit =
    import shortcut.{ quittable => qt }

    inline def qIf[Q >: shortcut.Quits.type <: shortcut.Type](p: Boolean)(using boundary.Label[Q]) =
      shortcut.quit(p).?

    inline def sIf[S >: shortcut.Skips.type <: shortcut.Type](p: Boolean)(using boundary.Label[S]) =
      shortcut.skip(p).?

    var cuml = 0
    val str = "ch.#ik."

    val ix = Array(2, 3, 1, 1, 3)
    def st = ix.stepper
    val civ = Iv(3, 5)
    val cpv = 3 to End-2
    val div = Iv(1, 5)
    val dpv = 1 to End-2

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f: Unit
      cuml

    T ~ z{ str.tap(_.flex.use(){ c => qIf(!c.isLetter); cuml += c }) }      ==== str
    T ~ cuml                                                               ==== str.take(2).map(_.toInt).sum
    T ~ z{ str.tap(_.flex.use(3, 5){ c => qIf(c.isLetter); cuml += c }) }   ==== str
    T ~ cuml                                                               ==== str(3).toInt
    T ~ z{ str.tap(_.flex.use(3 to 4){ c => qIf(c.isLetter); cuml += c }) } ==== str
    T ~ cuml                                                               ==== str(3).toInt
    T ~ z{ str.tap(_.flex.use(civ){ c => qIf(c.isLetter); cuml += c }) }    ==== str
    T ~ cuml                                                               ==== str(3).toInt
    T ~ z{ str.tap(_.flex.use(cpv){ c => qIf(c.isLetter); cuml += c }) }    ==== str
    T ~ cuml                                                               ==== str(3).toInt
    T ~ z{ str.tap(_.flex.use(ix){ c => qIf(c.isLetter); cuml += c }) }     ==== str
    T ~ cuml                                                               ==== ".#".map(_.toInt).sum
    T ~ z{ str.tap(_.flex.use(st){ c => qIf(c.isLetter); cuml += c }) }     ==== str
    T ~ cuml                                                               ==== ".#".map(_.toInt).sum

    T ~ str.flex.gather(0)(){ (a, c, i) => qIf(!c.isLetter); a + c + i }      ==== str.take(2).map(_.toInt).sum + 1
    T ~ str.flex.gather(0)(3, 5){ (a, c, i) => qIf(c.isLetter); a + c + i }   ==== str(3).toInt + 3
    T ~ str.flex.gather(0)(civ){ (a, c, i) => qIf(c.isLetter); a + c + i }    ==== str(3).toInt + 3
    T ~ str.flex.gather(0)(3 to 4){ (a, c, i) => qIf(c.isLetter); a + c + i } ==== str(3).toInt + 3
    T ~ str.flex.gather(0)(cpv){ (a, c, i) => qIf(c.isLetter); a + c + i }    ==== str(3).toInt + 3
    T ~ str.flex.gather(0)(ix){ (a, c, i) => qIf(c.isLetter); a + c + i }     ==== ".#".map(_.toInt).sum + 5
    T ~ str.flex.gather(0)(st){ (a, c, i) => qIf(c.isLetter); a + c + i }     ==== ".#".map(_.toInt).sum + 5

    T ~ str.flex.copyWith{ c => qIf(!c.isLetter); c }          ==== str.take(2)
    T ~ str.flex.copyOp((c, i) => if i%2 == 0 then '-' else c) =**= "-h-#-k-".arr
    T ~ str.flex.copyOp((c, i) => if i%2 == 0 then '-' else c) ==== typed[Array[Char]]
    T ~ str.flex.copyOp{ (c, i) => sIf(i%2 == 0); c }          =**= "h#k".arr
    T ~ str.flex.copyOp{ (c, i) => qIf(i == 1 || i == 4); c }  =**= "c".arr

    T ~ str.flex.where{ c => qIf(c > 'i'); c.isLetter } =**= str.zipWithIndex.takeWhile(_._1 <= 'i').collect{ case (c, i) if c.isLetter => i }

    T ~ str.flex.whereIn(1, 5  ){ c =>              !c.isLetter } =**= Array(2, 3)
    T ~ str.flex.whereIn(1 to 4){ c =>              !c.isLetter } =**= Array(2, 3)
    T ~ str.flex.whereIn(div   ){ c =>              !c.isLetter } =**= Array(2, 3)
    T ~ str.flex.whereIn(dpv   ){ c =>              !c.isLetter } =**= Array(2, 3)
    T ~ str.flex.whereIn(1, 5  ){ c => qIf(c=='#'); !c.isLetter } =**= Array(2)
    T ~ str.flex.whereIn(1 to 4){ c => qIf(c=='#'); !c.isLetter } =**= Array(2)
    T ~ str.flex.whereIn(div   ){ c => qIf(c=='#'); !c.isLetter } =**= Array(2)
    T ~ str.flex.whereIn(dpv   ){ c => qIf(c=='#'); !c.isLetter } =**= Array(2)

    T ~ str.flex.whereFrom(Array(2, 0, 3, 6)){ c =>              !c.isLetter } =**= Array(2, 3, 6)
    T ~ str.flex.whereFrom(Array(2, 0, 3, 6)){ c => qIf(c=='#'); !c.isLetter } =**= Array(2)

    def ninc(c: Char, i: Int) = if c.isLetter then -1 else i+7
    def nxnc[Q >: shortcut.Quits.type <: shortcut.Type](c: Char, i: Int)(using lb: boundary.Label[Q]) =
      qIf(c == '#')
      if c.isLetter then -1 else i+7
    T ~ str.flex.whereInOp(1, 5)(ninc)       =**= str.flex.whereIn(1, 5)(! _.isLetter).copyWith(_ + 7)
    T ~ str.flex.whereInOp(Iv(1, 5))(ninc)   =**= str.flex.whereIn(Iv(1, 5))(! _.isLetter).copyWith(_ + 7)
    T ~ str.flex.whereInOp(1 to End-2)(ninc) =**= str.flex.whereIn(1 to End-2)(! _.isLetter).copyWith(_ + 7)
    T ~ str.flex.whereInOp(1 to 4)(ninc)     =**= str.flex.whereIn(1 to 4)(! _.isLetter).copyWith(_ + 7)
    T ~ str.flex.whereInOp(1, 5)(nxnc)       =**= str.flex.whereIn(1, 5){ c => qIf(c=='#'); !c.isLetter }.copyWith(_ + 7)
    T ~ str.flex.whereInOp(Iv(1, 5))(nxnc)   =**= str.flex.whereIn(Iv(1, 5)){ c => qIf(c=='#'); !c.isLetter }.copyWith(_ + 7)
    T ~ str.flex.whereInOp(1 to End-2)(nxnc) =**= str.flex.whereIn(1 to End-2){ c => qIf(c=='#'); !c.isLetter }.copyWith(_ + 7)
    T ~ str.flex.whereInOp(1 to 4)(nxnc)     =**= str.flex.whereIn(1 to 4){ c => qIf(c=='#'); !c.isLetter }.copyWith(_ + 7)

    T ~ str.flex.whereFromOp(Array(2, 0, 3, 6))(ninc) =**= str.flex.whereFrom(Array(2, 0, 3, 6))(! _.isLetter).copyWith(_ + 7)
    T ~ str.flex.whereFromOp(Array(2, 0, 3, 6))(nxnc) =**= str.flex.whereFrom(Array(2, 0, 3, 6)){ c => qIf(c=='#'); !c.isLetter }.copyWith(_ + 7)

    val cx = "_________".arr
    var ninja = 0
    T ~ cx.dup(a => ninja = str.flex.inject(a)(_.isLetter)).str                          ==== "chik_____"
    T ~ { val x = ninja; ninja = 0; x }                                                       ==== str.count(_.isLetter)
    T ~ cx.dup(a => ninja = str.flex.inject(a, 2)(_.isLetter)) .str                      ==== "__chik___"
    T ~ { val x = ninja; ninja = 0; x }                                                       ==== str.count(_.isLetter)
    T ~ cx.dup(a => ninja = str.flex.inject(a){ c => qIf(c == '#'); c.isLetter }).str    ==== "ch_______"
    T ~ { val x = ninja; ninja = 0; x }                                                       ==== str.takeWhile(_ != '#').count(_.isLetter)
    T ~ cx.dup(a => ninja = str.flex.inject(a, 2){ c => qIf(c == '#'); c.isLetter }).str ==== "__ch_____"
    T ~ { val x = ninja; ninja = 0; x }                                                       ==== str.takeWhile(_ != '#').count(_.isLetter)

    val ax = "_________".arr.map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(      ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "cim______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(      ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "___cim___".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(1, 5  ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "i________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(1, 5  ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "___i_____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(1, 5  ){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "im_______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(1, 5  ){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "___im____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(1 to 4){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "i________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(1 to 4){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "___i_____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(1 to 4){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "im_______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(1 to 4){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "___im____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(div   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "i________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(div   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "___i_____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(div   ){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "im_______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(div   ){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "___im____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(dpv   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "i________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(dpv   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "___i_____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(dpv   ){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "im_______".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(dpv   ){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "___im____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(ix    ){ (c, i) => sIf(i==2); qIf( c.isLetter); c + i }) =**= "&________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(ix    ){ (c, i) => sIf(i==2); qIf( c.isLetter); c + i }) =**= "___&_____".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a   )(st    ){ (c, i) => sIf(i==2); qIf( c.isLetter); c + i }) =**= "&________".map(_.toInt)
    T ~ ax.dup(a => ninja += str.flex.injectOp(a, 3)(st    ){ (c, i) => sIf(i==2); qIf( c.isLetter); c + i }) =**= "___&_____".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x } ==== 2*3 + 2*4*(1+2) + 2*(1+1)

    T ~ str.flex.select(_.isLetter)                       ==== "chik"
    T ~ str.flex.select{ c => qIf(c == '#'); c.isLetter } ==== "ch"

    T ~ str.flex.selectOp(3, 5)(  (c, i) => if i%2 == 0 then '-' else c) =**= "#-".arr
    T ~ str.flex.selectOp(3 to 4)((c, i) => if i%2 == 0 then '-' else c) =**= "#-".arr
    T ~ str.flex.selectOp(civ)(   (c, i) => if i%2 == 0 then '-' else c) =**= "#-".arr
    T ~ str.flex.selectOp(cpv)(   (c, i) => if i%2 == 0 then '-' else c) =**= "#-".arr
    T ~ str.flex.selectOp(ix)(    (c, i) => if i%2 == 0 then '-' else c) =**= "-#hh#".arr
    T ~ str.flex.selectOp(st)(    (c, i) => if i%2 == 0 then '-' else c) =**= "-#hh#".arr
    T ~ str.flex.selectOp(3, 5)(  (c, i) => if i%2 == 0 then '-' else c) ==== typed[Array[Char]]
    T ~ str.flex.selectOp(3 to 4)((c, i) => if i%2 == 0 then '-' else c) ==== typed[Array[Char]]
    T ~ str.flex.selectOp(civ)(   (c, i) => if i%2 == 0 then '-' else c) ==== typed[Array[Char]]
    T ~ str.flex.selectOp(cpv)(   (c, i) => if i%2 == 0 then '-' else c) ==== typed[Array[Char]]
    T ~ str.flex.selectOp(ix)(    (c, i) => if i%2 == 0 then '-' else c) ==== typed[Array[Char]]
    T ~ str.flex.selectOp(st)(    (c, i) => if i%2 == 0 then '-' else c) ==== typed[Array[Char]]

    T ~ str.flex.selectOp(3, 5){  (c, i) => sIf(i%2 == 0); c } =**= "#".arr
    T ~ str.flex.selectOp(3 to 4){(c, i) => sIf(i%2 == 0); c } =**= "#".arr
    T ~ str.flex.selectOp(civ){   (c, i) => sIf(i%2 == 0); c } =**= "#".arr
    T ~ str.flex.selectOp(cpv){   (c, i) => sIf(i%2 == 0); c } =**= "#".arr
    T ~ str.flex.selectOp(ix){    (c, i) => sIf(i%2 == 0); c } =**= "#hh#".arr
    T ~ str.flex.selectOp(st){    (c, i) => sIf(i%2 == 0); c } =**= "#hh#".arr

    T ~ str.flex.selectOp(3, 5){  (c, i) => qIf(i == 1 || i == 4); c } =**= "#".arr
    T ~ str.flex.selectOp(3 to 4){(c, i) => qIf(i == 1 || i == 4); c } =**= "#".arr
    T ~ str.flex.selectOp(civ){   (c, i) => qIf(i == 1 || i == 4); c } =**= "#".arr
    T ~ str.flex.selectOp(cpv){   (c, i) => qIf(i == 1 || i == 4); c } =**= "#".arr
    T ~ str.flex.selectOp(ix){    (c, i) => qIf(i == 1 || i == 4); c } =**= ".#".arr
    T ~ str.flex.selectOp(st){    (c, i) => qIf(i == 1 || i == 4); c } =**= ".#".arr

    val lar = "ch.ix.#n."
    T ~ lar.flex.fuse[Int]((c, i, add) => if !c.isLetter then { qIf(i>5); add(i) } else Array(O(Some(c.toString))).os.foreach(c => add(c.toInt))) =**= Array(99, 46, 104, 46, 2, 105, 46, 120, 46, 5)

    val test = "cheesefactories"
    T ~ test.flex.diced(_ == 'e')                               =**= Array("ch", "s", "factori", "s")
    T ~ test.flex.diced(_ == 'e', "")                           =**= Array("ch", "s", "factori", "s")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "")   =**= Array("ch", "s", "facto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "")   =**= Array("ch", "s")
    T ~ test.flex.diced(_ == 'e', "()")                         =**= Array("ch", "", "s", "factori", "s")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "()") =**= Array("ch", "", "s", "facto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "()") =**= Array("ch", "", "s", "")
    T ~ test.flex.diced(_ == 'e', "(]")                         =**= Array("che", "e", "se", "factorie", "s")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "(]") =**= Array("che", "e", "se", "facto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "(]") =**= Array("che", "e", "se", "")
    T ~ test.flex.diced(_ == 'e', "[)")                         =**= Array("ch", "e", "es", "efactori", "es")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "[)") =**= Array("ch", "e", "es", "efacto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "[)") =**= Array("ch", "e", "es", "e")
    T ~ test.flex.diced(_ == 'e', "[]")                         =**= Array("che", "ee", "ese", "efactorie", "es")
    T ~ test.flex.diced(c => { qIf(c == 'r'); c == 'e' }, "[]") =**= Array("che", "ee", "ese", "efacto")
    T ~ test.flex.diced(c => { qIf(c == 'f'); c == 'e' }, "[]") =**= Array("che", "ee", "ese", "e")


  def stringFancyIntervalTest(): Unit =
    import shortcut.{ quittable => qt }

    inline def qIf[Q >: shortcut.Quits.type <: shortcut.Type](p: Boolean)(using boundary.Label[Q]) =
      shortcut.quit(p).?

    inline def sIf[S >: shortcut.Skips.type <: shortcut.Type](p: Boolean)(using boundary.Label[S]) =
      shortcut.skip(p).?

    var cuml = 0
    val str = "ch.#ik."

    val ix = Array(2, 3, 1, 1, 3)
    val ex = Array(2, 0, -1, 3, 6, 7, -9, 400) 
    def st = ix.stepper
    def et = ex.stepper

    val civ = Iv(3, 5)
    val cpv = 3 to End-2
    val eiv = Iv(3, 9)
    val fiv = Iv(-2, 5)
    val fpv = End-9 to End-2
    val biv = Iv(-2, 9)
    val niv = Iv(8, 9)
    val npv = End-9 to End-8

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f: Unit
      cuml

    T ~ str.clip.flex ==== typed[FancyString]
    T ~ str.flex.clip ==== typed[FancyString]
    T ~ str.fancy      ==== typed[FancyString]

    T ~ z{ str.tap(_.flex.clip.use(3, 5){ c => qIf(c.isLetter); cuml += c }) }   ==== str
    T ~ cuml                                                                     ==== str(3).toInt
    T ~ z{ str.tap(_.flex.clip.use(3 to 4){ c => qIf(c.isLetter); cuml += c }) } ==== str
    T ~ cuml                                                                     ==== str(3).toInt
    T ~ z{ str.tap(_.flex.clip.use(civ){ c => qIf(c.isLetter); cuml += c }) }    ==== str
    T ~ cuml                                                                     ==== str(3).toInt
    T ~ z{ str.tap(_.flex.clip.use(cpv){ c => qIf(c.isLetter); cuml += c }) }    ==== str
    T ~ cuml                                                                     ==== str(3).toInt
    T ~ z{ str.tap(_.flex.clip.use(ix){ c => qIf(c.isLetter); cuml += c }) }     ==== str
    T ~ cuml                                                                     ==== ".#".map(_.toInt).sum
    T ~ z{ str.tap(_.flex.clip.use(st){ c => qIf(c.isLetter); cuml += c }) }     ==== str
    T ~ cuml                                                                     ==== ".#".map(_.toInt).sum

    T ~ n{ str.tap(_.flex.clip.use(3, 9){ c => qIf(c.isLetter); cuml += c }) }   ==== str(3).toInt
    T ~ n{ str.tap(_.flex.clip.use(3 to 8){ c => qIf(c.isLetter); cuml += c }) } ==== str(3).toInt
    T ~ n{ str.tap(_.flex.clip.use(eiv){ c => qIf(c.isLetter); cuml += c }) }    ==== str(3).toInt

    T ~ n{ str.tap(_.flex.clip.use(-2, 5){ c => qIf(!c.isLetter); cuml += c }) }   ==== str.takeWhile(_.isLetter).map(_.toInt).sum
    T ~ n{ str.tap(_.flex.clip.use(-2 to 4){ c => qIf(!c.isLetter); cuml += c }) } ==== str.takeWhile(_.isLetter).map(_.toInt).sum
    T ~ n{ str.tap(_.flex.clip.use(fiv){ c => qIf(!c.isLetter); cuml += c }) }     ==== str.takeWhile(_.isLetter).map(_.toInt).sum
    T ~ n{ str.tap(_.flex.clip.use(fpv){ c => qIf(!c.isLetter); cuml += c }) }     ==== str.takeWhile(_.isLetter).map(_.toInt).sum

    T ~ n{ str.tap(_.flex.clip.use(-2, 9){ c => qIf(!c.isLetter); cuml += c }) }   ==== str.takeWhile(_.isLetter).map(_.toInt).sum
    T ~ n{ str.tap(_.flex.clip.use(-2 to 9){ c => qIf(!c.isLetter); cuml += c }) } ==== str.takeWhile(_.isLetter).map(_.toInt).sum
    T ~ n{ str.tap(_.flex.clip.use(biv){ c => qIf(!c.isLetter); cuml += c }) }     ==== str.takeWhile(_.isLetter).map(_.toInt).sum

    T ~ n{ str.tap(_.flex.clip.use(8, 9){ c => qIf(c.isLetter); cuml += c }) }    ==== 0
    T ~ n{ str.tap(_.flex.clip.use(8 to 9){ c => qIf(c.isLetter); cuml += c }) }  ==== 0
    T ~ n{ str.tap(_.flex.clip.use(niv){ c => qIf(c.isLetter); cuml += c }) }     ==== 0
    T ~ n{ str.tap(_.flex.clip.use(npv){ c => qIf(c.isLetter); cuml += c }) }     ==== 0

    T ~ n{ str.tap(_.flex.clip.use(ex){ c => qIf(c == '#'); cuml += c }) } ==== ".c".map(_.toInt).sum
    T ~ n{ str.tap(_.flex.clip.use(et){ c => qIf(c == '#'); cuml += c }) } ==== ".c".map(_.toInt).sum

    def sm(i: Int, j: Int) = j*(j+1)/2 - i*(i-1)/2
    T ~ str.clip.flex.gather(0)(3, 5){ (a, c, i) => qIf(c.isLetter); a + c + i }   ==== str(3).toInt + 3
    T ~ str.clip.flex.gather(0)(civ){ (a, c, i) => qIf(c.isLetter); a + c + i }    ==== str(3).toInt + 3
    T ~ str.clip.flex.gather(0)(3 to 4){ (a, c, i) => qIf(c.isLetter); a + c + i } ==== str(3).toInt + 3
    T ~ str.clip.flex.gather(0)(cpv){ (a, c, i) => qIf(c.isLetter); a + c + i }    ==== str(3).toInt + 3
    T ~ str.clip.flex.gather(0)(ix){ (a, c, i) => qIf(c.isLetter); a + c + i }     ==== ".#".map(_.toInt).sum + 5
    T ~ str.clip.flex.gather(0)(st){ (a, c, i) => qIf(c.isLetter); a + c + i }     ==== ".#".map(_.toInt).sum + 5

    T ~ str.clip.flex.gather(0)(3, 9){ (a, c, i) => qIf(c.isLetter); a + c + i }   ==== str(3).toInt + 3
    T ~ str.clip.flex.gather(0)(3 to 8){ (a, c, i) => qIf(c.isLetter); a + c + i } ==== str(3).toInt + 3
    T ~ str.clip.flex.gather(0)(eiv){ (a, c, i) => qIf(c.isLetter); a + c + i }    ==== str(3).toInt + 3

    T ~ str.clip.flex.gather(0)(-2, 5){ (a, c, i) => qIf(!c.isLetter); a + c + i }   ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)
    T ~ str.clip.flex.gather(0)(-2 to 4){ (a, c, i) => qIf(!c.isLetter); a + c + i } ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)
    T ~ str.clip.flex.gather(0)(fiv){ (a, c, i) => qIf(!c.isLetter); a + c + i }     ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)
    T ~ str.clip.flex.gather(0)(fpv){ (a, c, i) => qIf(!c.isLetter); a + c + i }     ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)

    T ~ str.clip.flex.gather(0)(-2, 9){ (a, c, i) => qIf(!c.isLetter); a + c + i }   ==== str.take(2).map(_.toInt).sum + sm(0, 1)
    T ~ str.clip.flex.gather(0)(-2 to 9){ (a, c, i) => qIf(!c.isLetter); a + c + i } ==== str.take(2).map(_.toInt).sum + sm(0, 1)
    T ~ str.clip.flex.gather(0)(biv){ (a, c, i) => qIf(!c.isLetter); a + c + i }     ==== str.take(2).map(_.toInt).sum + sm(0, 1)

    T ~ str.clip.flex.gather(0)(8, 9){ (a, c, i) => qIf(c.isLetter); a + c + i }   ==== 0
    T ~ str.clip.flex.gather(0)(8 to 9){ (a, c, i) => qIf(c.isLetter); a + c + i } ==== 0
    T ~ str.clip.flex.gather(0)(niv){ (a, c, i) => qIf(c.isLetter); a + c + i }    ==== 0
    T ~ str.clip.flex.gather(0)(npv){ (a, c, i) => qIf(c.isLetter); a + c + i }    ==== 0

    T ~ str.clip.flex.gather(0)(ex){ (a, c, i) => qIf(c == '#'); a + c + i } ==== ".c".map(_.toInt).sum + 2
    T ~ str.clip.flex.gather(0)(et){ (a, c, i) => qIf(c == '#'); a + c + i } ==== ".c".map(_.toInt).sum + 2

    T ~ str.clip.flex.whereIn(3, 9   ){ c => qIf(c=='k'); c.isLetter } =**= Array(4)
    T ~ str.clip.flex.whereIn(3 to 8 ){ c => qIf(c=='k'); c.isLetter } =**= Array(4)
    T ~ str.clip.flex.whereIn(eiv    ){ c => qIf(c=='k'); c.isLetter } =**= Array(4)
    T ~ str.clip.flex.whereIn(-2, 5  ){ c => qIf(c=='#'); c.isLetter } =**= Array(0, 1)
    T ~ str.clip.flex.whereIn(-2 to 4){ c => qIf(c=='#'); c.isLetter } =**= Array(0, 1)
    T ~ str.clip.flex.whereIn(fiv    ){ c => qIf(c=='#'); c.isLetter } =**= Array(0, 1)
    T ~ str.clip.flex.whereIn(fpv    ){ c => qIf(c=='#'); c.isLetter } =**= Array(0, 1)

    T ~ str.fancy.whereFrom(Array(2, 1, -3, 9, 5, 3, 4)){ c => qIf(c=='#'); c.isLetter } =**= Array(1, 5)

    def lxnc[Q >: shortcut.Quits.type <: shortcut.Type](c: Char, i: Int)(using lb: boundary.Label[Q]) =
      qIf(c == '#')
      if c.isLetter then i+7 else -1
    T ~ str.fancy.whereInOp(3, 9)(lxnc)           =**= str.fancy.whereIn(3, 9){ c => qIf(c=='#'); c.isLetter }.copyWith(_ + 7)
    T ~ str.fancy.whereInOp(Iv(3, 9))(lxnc)       =**= str.fancy.whereIn(Iv(3, 9)){ c => qIf(c=='#'); c.isLetter }.copyWith(_ + 7)
    T ~ str.fancy.whereInOp(3 to 8)(lxnc)         =**= str.fancy.whereIn(3 to 8){ c => qIf(c=='#'); c.isLetter }.copyWith(_ + 7)
    T ~ str.fancy.whereInOp(-2, 5)(lxnc)          =**= str.fancy.whereIn(-2, 5){ c => qIf(c=='#'); c.isLetter }.copyWith(_ + 7)
    T ~ str.fancy.whereInOp(Iv(-2, 5))(lxnc)      =**= str.fancy.whereIn(Iv(-2, 5)){ c => qIf(c=='#'); c.isLetter }.copyWith(_ + 7)
    T ~ str.fancy.whereInOp(End-9 to End-2)(lxnc) =**= str.fancy.whereIn(End-9 to End-2){ c => qIf(c=='#'); c.isLetter }.copyWith(_ + 7)

    T ~ str.fancy.whereFromOp(Array(2, 1, -3, 9, 5, 3, 4))(lxnc) =**= str.fancy.whereFrom(Array(2, 1, -3, 9, 5, 3, 4)){ c => qIf(c=='#'); c.isLetter }.copyWith(_ + 7)

    val ca7 = "1234567".arr
    val ca3 = "890".arr
    var ninja = 0
    T ~ ca7.dup(a => ninja += str.flex.clip.inject(a){ c => qIf(c == 'i'); c.isLetter })    =**= "ch34567".arr
    T ~ ca7.dup(a => ninja += str.flex.clip.inject(a, 2){ c => qIf(c == 'i'); c.isLetter }) =**= "12ch567".arr
    T ~ ca7.dup(a => ninja += str.flex.clip.inject(a, 6){ c => qIf(c == 'i'); c.isLetter }) =**= "123456c".arr
    T ~ ca3.dup(a => ninja += str.flex.clip.inject(a){ c => qIf(c == 'i'); c.isLetter })    =**= "ch0".arr
    T ~ ca3.dup(a => ninja += str.flex.clip.inject(a, 2){ c => qIf(c == 'i'); c.isLetter }) =**= "89c".arr
    T ~ { val x = ninja; ninja = 0; x }                                                          ==== 2+2+1+2+1

    val aa7 = "ABCDEFG".arr.map(_.toInt)
    val aa1 = "H".arr.map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ciCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(){ (c, i) => sIf(!c.isLetter); qIf(i==9); c + i }) =**= "cimpEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "cimDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 5 )(){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "ABCDEci".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 9 )(){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x } ==== 2+4+3+2+0
    val ejv = Iv(1, 9)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(1, 9  ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "imCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(1, 9  ){ (c, i) => sIf(!c.isLetter); qIf(i==7); c + i }) =**= "impDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 6 )(1, 9  ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "ABCDEFi".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(1, 9  ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "imCDEFG".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.flex.injectOp(a, -2)(1, 9  ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "i".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(1 to 8){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "imCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(1 to 8){ (c, i) => sIf(!c.isLetter); qIf(i==7); c + i }) =**= "impDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 6 )(1 to 8){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "ABCDEFi".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(1 to 8){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "imCDEFG".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.flex.injectOp(a, -2)(1 to 8){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "i".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(ejv   ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "imCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(ejv   ){ (c, i) => sIf(!c.isLetter); qIf(i==7); c + i }) =**= "impDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 6 )(ejv   ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "ABCDEFi".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(ejv   ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "imCDEFG".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.flex.injectOp(a, -2)(ejv   ){ (c, i) => sIf(!c.isLetter); qIf(i==5); c + i }) =**= "i".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x } ==== 3*(2+3+1+2+1)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(-2, 5  ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ciCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(-2, 5  ){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "cimDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 6 )(-2, 5  ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFc".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(-2, 5  ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ciCDEFG".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.flex.injectOp(a, -2)(-2, 5  ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "c".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(-2 to 4){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ciCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(-2 to 4){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "cimDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 6 )(-2 to 4){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFc".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(-2 to 4){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ciCDEFG".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.flex.injectOp(a, -2)(-2 to 4){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "c".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(fiv    ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ciCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(fiv    ){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "cimDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 6 )(fiv    ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFc".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(fiv    ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ciCDEFG".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.flex.injectOp(a, -2)(fiv    ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "c".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(fpv    ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ciCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(fpv    ){ (c, i) => sIf(!c.isLetter); qIf(i==6); c + i }) =**= "cimDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 6 )(fpv    ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFc".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(fpv    ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ciCDEFG".map(_.toInt)
    T ~ aa1.dup(a => ninja += str.clip.flex.injectOp(a, -2)(fpv    ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "c".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x } ==== 4*(2+3+1+2+1)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(8, 10 ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -7)(8, 10 ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 8 )(8, 10 ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(8 to 9){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -7)(8 to 9){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 8 )(8 to 9){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(niv   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -7)(niv   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 8 )(niv   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(npv   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -7)(npv   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 8 )(npv   ){ (c, i) => sIf(!c.isLetter); qIf(i==4); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x } ==== 0
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(ex){ (c, i) => sIf(c.isLetter); qIf(i==6); c + i }) =**= "0&CDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(ex){ (c, i) => sIf(c.isLetter); qIf(i==8); c + i }) =**= "0&4DEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(ex){ (c, i) => sIf(c.isLetter); qIf(i==6); c + i }) =**= "0&CDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 6 )(ex){ (c, i) => sIf(c.isLetter); qIf(i==6); c + i }) =**= "ABCDEF0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 9 )(ex){ (c, i) => sIf(c.isLetter); qIf(i==6); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(et){ (c, i) => sIf(c.isLetter); qIf(i==6); c + i }) =**= "0&CDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a    )(et){ (c, i) => sIf(c.isLetter); qIf(i==8); c + i }) =**= "0&4DEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, -2)(et){ (c, i) => sIf(c.isLetter); qIf(i==6); c + i }) =**= "0&CDEFG".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 6 )(et){ (c, i) => sIf(c.isLetter); qIf(i==6); c + i }) =**= "ABCDEF0".map(_.toInt)
    T ~ aa7.dup(a => ninja += str.clip.flex.injectOp(a, 9 )(et){ (c, i) => sIf(c.isLetter); qIf(i==6); c + i }) =**= "ABCDEFG".map(_.toInt)
    T ~ { val x = ninja; ninja = 0; x } ==== 2*(2+3+2+1+0)

    T ~ str.clip.flex.selectOp(3, 5  )((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.flex.selectOp(3 to 4)((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.flex.selectOp(civ   )((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.flex.selectOp(cpv   )((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.flex.selectOp(ix    )((c, i) => c + i) =**= "0&ii&".map(_.toInt)
    T ~ str.clip.flex.selectOp(st    )((c, i) => c + i) =**= "0&ii&".map(_.toInt)
    T ~ str.clip.flex.selectOp(3, 5  )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.flex.selectOp(3 to 4)((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.flex.selectOp(civ   )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.flex.selectOp(cpv   )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.flex.selectOp(ix    )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.flex.selectOp(st    )((c, i) => c + i) ==== typed[Array[Int]]

    T ~ str.clip.flex.selectOp(3, 5){  (c, i) => qIf(i == 1 || i == 4); c } =**= "#".arr
    T ~ str.clip.flex.selectOp(3 to 4){(c, i) => qIf(i == 1 || i == 4); c } =**= "#".arr
    T ~ str.clip.flex.selectOp(civ){   (c, i) => qIf(i == 1 || i == 4); c } =**= "#".arr
    T ~ str.clip.flex.selectOp(cpv){   (c, i) => qIf(i == 1 || i == 4); c } =**= "#".arr
    T ~ str.clip.flex.selectOp(ix){    (c, i) => qIf(i == 1 || i == 4); c } =**= ".#".arr
    T ~ str.clip.flex.selectOp(st){    (c, i) => qIf(i == 1 || i == 4); c } =**= ".#".arr

    T ~ str.clip.flex.selectOp(3, 9  ){ (c, i) => qIf(c == 'k'); c + i } =**= "&m".map(_.toInt)
    T ~ str.clip.flex.selectOp(3 to 8){ (c, i) => qIf(c == 'k'); c + i } =**= "&m".map(_.toInt)
    T ~ str.clip.flex.selectOp(eiv   ){ (c, i) => qIf(c == 'k'); c + i } =**= "&m".map(_.toInt)

    T ~ str.clip.flex.selectOp(3, 9  ){ (c, i) => sIf(c == 'k'); c + i } =**= "&m4".map(_.toInt)
    T ~ str.clip.flex.selectOp(3 to 8){ (c, i) => sIf(c == 'k'); c + i } =**= "&m4".map(_.toInt)
    T ~ str.clip.flex.selectOp(eiv   ){ (c, i) => sIf(c == 'k'); c + i } =**= "&m4".map(_.toInt)

    T ~ str.clip.flex.selectOp(-2, 5  ){ (c, i) => qIf(c == '#'); c + i } =**= "ci0".map(_.toInt)
    T ~ str.clip.flex.selectOp(-2 to 4){ (c, i) => qIf(c == '#'); c + i } =**= "ci0".map(_.toInt)
    T ~ str.clip.flex.selectOp(fiv    ){ (c, i) => qIf(c == '#'); c + i } =**= "ci0".map(_.toInt)
    T ~ str.clip.flex.selectOp(fpv    ){ (c, i) => qIf(c == '#'); c + i } =**= "ci0".map(_.toInt)

    T ~ str.clip.flex.selectOp(-2, 5  ){ (c, i) => sIf(c == '#'); c + i } =**= "ci0m".map(_.toInt)
    T ~ str.clip.flex.selectOp(-2 to 4){ (c, i) => sIf(c == '#'); c + i } =**= "ci0m".map(_.toInt)
    T ~ str.clip.flex.selectOp(fiv    ){ (c, i) => sIf(c == '#'); c + i } =**= "ci0m".map(_.toInt)
    T ~ str.clip.flex.selectOp(fpv    ){ (c, i) => sIf(c == '#'); c + i } =**= "ci0m".map(_.toInt)

    T ~ str.clip.flex.selectOp(-2, 9  ){ (c, i) => qIf(c == 'i'); c + i } =**= "ci0&".map(_.toInt)
    T ~ str.clip.flex.selectOp(-2 to 8){ (c, i) => qIf(c == 'i'); c + i } =**= "ci0&".map(_.toInt)
    T ~ str.clip.flex.selectOp(biv    ){ (c, i) => qIf(c == 'i'); c + i } =**= "ci0&".map(_.toInt)

    T ~ str.clip.flex.selectOp(-2, 9  ){ (c, i) => sIf(c == 'i'); c + i } =**= "ci0&p4".map(_.toInt)
    T ~ str.clip.flex.selectOp(-2 to 8){ (c, i) => sIf(c == 'i'); c + i } =**= "ci0&p4".map(_.toInt)
    T ~ str.clip.flex.selectOp(biv    ){ (c, i) => sIf(c == 'i'); c + i } =**= "ci0&p4".map(_.toInt)

    T ~ str.clip.flex.selectOp(8, 10 ){ (c, i) => sIf(!c.isLetter); qIf(c > 99); c + i } =**= "".map(_.toInt)
    T ~ str.clip.flex.selectOp(8 to 9){ (c, i) => sIf(!c.isLetter); qIf(c > 99); c + i } =**= "".map(_.toInt)
    T ~ str.clip.flex.selectOp(niv   ){ (c, i) => sIf(!c.isLetter); qIf(c > 99); c + i } =**= "".map(_.toInt)
    T ~ str.clip.flex.selectOp(npv   ){ (c, i) => sIf(!c.isLetter); qIf(c > 99); c + i } =**= "".map(_.toInt)

    T ~ str.clip.flex.selectOp(ex){ (c, i) => qIf(i == 3); c + i } =**= "0c".map(_.toInt)
    T ~ str.clip.flex.selectOp(et){ (c, i) => qIf(i == 3); c + i } =**= "0c".map(_.toInt)

    T ~ str.clip.flex.selectOp(ex){ (c, i) => sIf(i == 3); c + i } =**= "0c4".map(_.toInt)
    T ~ str.clip.flex.selectOp(et){ (c, i) => sIf(i == 3); c + i } =**= "0c4".map(_.toInt)
}
