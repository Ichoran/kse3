// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2015, 2021 Rex Kerr, UCSF, and Calico Life Sciences LLC.


package kse.flow

import scala.util.control.NonFatal

import scala.util.{Try, Success, Failure}


//////////////////////////////////////
/// Early returns with ? a la Rust ///
//////////////////////////////////////

extension [N, Y](ok: Ok[N, Y])
  inline def ? : Y = ok match
    case Yes(y) => y
    case n      => throw new UntransformedFlowException(n)

extension [L, R](either: Either[L, R])
  inline def ? : R = either match
    case Right(r) => r
    case l => throw new UntransformedFlowException(l)

extension [A](option: Option[A])
  inline def ? : A = option match
    case Some(a) => a
    case _ => throw new UntransformedFlowException(None)

extension [A](`try`: Try[A])
  inline def ? : A = `try` match
    case Success(a) => a
    case f => throw new UntransformedFlowException(f)

extension (double: Double)
  inline def ? : Double = double match
    case x if java.lang.Double.isNaN(x) => throw new UntransformedFlowException(x)
    case y => y

extension (float: Float)
  inline def ? : Float = float match
    case x if java.lang.Float.isNaN(x) => throw new UntransformedFlowException(x)
    case y => y

inline def Ret[A](inline a: A) = ${ EarlyReturnMacro.transform('a) }

extension (objectOk: Ok.type)
  inline def Ret[N, Y](inline y: Y): Ok[N, Y] =
    ${ EarlyReturnMacro.transform('{ val ok: Ok[N, Y] = Yes(y); ok }) }

extension (objectEither: Either.type)
  inline def Ret[L, R](inline r: R): Either[L, R] = ${ EarlyReturnMacro.transform('{ val either: Either[L, R] = Right(r); either }) }

extension (objectOption: Option.type)
  inline def Ret[A](inline a: A): Option[A] = ${ EarlyReturnMacro.transform('{ val option: Option[A] = Some(a); option }) }

extension (tryObject: Try.type)
  inline def Ret[A](inline a: A): Try[A] = ${ EarlyReturnMacro.transform('{ val tri: Try[A] = Success(a); tri }) }



/////////////////////////////////////////
/// Validation and exception handling ///
/////////////////////////////////////////

extension (throwable: Throwable) {
  def explainAsArray(lines: Int = Int.MaxValue): Array[String] =
    import scala.collection.mutable.LongMap
    val seen = new LongMap[List[Throwable]]
      val sab = Array.newBuilder[String]
      var t = ((throwable, "", lines, false)) :: Nil
      while (t.nonEmpty) {
        val (ti, si, ni, cb) = t.head
        t = t.tail
        val notYetSeen = {
          val ihc = System.identityHashCode(ti)
          val entry = seen.getOrNull(ihc)
          if (entry eq null) { seen(ihc) = ti :: Nil; true }
          else if (!entry.exists(_ eq t)) { seen(ihc) = ti :: entry; true }
          else false
        }
        if (notYetSeen) {
          sab += (if (cb) si + "CAUSED BY " else si) + ti.getClass.getName + ": " + ti.getMessage
          val st = ti.getStackTrace
          sab ++= st.take(ni).map(_.toString)
          if (st.length > ni && ni > 0) sab += si + "...[" + (st.length - ni).toString + " lines elided]"
          val tj = ti.getCause
          if (tj ne null) t = ((tj, si, lines, true)) :: t
          val sup = ti.getSuppressed
          if (sup.length > 0) t = sup.reverse.map(s => (s, si + "> ", lines/2, false)) ++: t
        }
        else sab += si + "(Circular reference to " + ti.getClass.getName + ": " + ti.getMessage + ")"
      }
      sab.result

  def explainAsVector(lines: Int = Int.MaxValue): Vector[String] = throwable.explainAsArray(lines).toVector

  def explain(lines: Int = Int.MaxValue): String = explainAsArray(lines).mkString("\n")
}

trait NotNice[N] {
  def fromThrowable(t: Throwable): N
}
object NotNice {
  given NotNice[String] = new NotNice[String] { def fromThrowable(t: Throwable) = t.explain(12) }
}

inline def safe[Y](inline y: => Y): Ok[Throwable, Y] =
  try Yes(y)
  catch
    case e if NonFatal(e) => No(e)

inline def nice[N, Y](inline y: => Y)(using NotNice[N]): Ok[N, Y] = 
  try Yes(y)
  catch
    case e if NonFatal(e) => No(summon[NotNice[N]] fromThrowable e)


extension (objectOk: Ok.type) {
  inline def Safer[N, Y](erf: Throwable => N)(inline y: Y): Ok[N, Y] =
    ${ EarlyReturnMacro.transform('{ val ok: Ok[N, Y] = try { Yes(y) } catch { case t if scala.util.control.NonFatal(t) => No(erf(t)) }; ok }) }
  inline def Nicer[N, Y](inline y: Y)(using nn: NotNice[N]): Ok[N, Y] =
    ${ EarlyReturnMacro.transform('{ try{ Yes(y) } catch { case t if NonFatal(t) => No(nn fromThrowable t) }}) }
}


//////////////////////////////////////////////////////////////////
/// Interconversions between Ok and standard library sum types ///
//////////////////////////////////////////////////////////////////


final case class LeftBranchException[+L](left: L) extends Exception {
  override def getMessage: String = left.toString
}

extension [L, R](either: Either[L, R]) {
  inline def toOk: Ok[L, R] = either match
    case Right(r) => Yes(r)
    case Left(l)  => No(l)
}

extension [A](`try`: Try[A]) {
  inline def toOk: Ok[Throwable, A] = `try` match
    case Success(s) => Yes(s)
    case Failure(t) => No(t)

  inline def okOr[B](inline f: Throwable => B): Ok[B, A] = `try` match
    case Success(s) => Yes(s)
    case Failure(t) => No(f(t))
}

extension [A](option: Option[A]) {
  inline def toOk: Ok[Unit, A] = option match
    case Some(a) => Yes(a)
    case _       => Ok.UnitNo

  inline def okOr[B](inline b: => B) = option match
    case Some(a) => Yes(a)
    case _       => No(b)
}



////////////////////////////////////////////////////////
/// Empowering sum types and others to work with Hop ///
////////////////////////////////////////////////////////

extension [A](a: A) {
  inline def hop(using ch: CanHop[A]): Nothing = ch hop a
  inline def hopMap[B](inline f: A => B)(using ch: CanHop[B]): Nothing = ch hop f(a)
  inline def hopIf(inline p: A => Boolean)(using ch: CanHop[A]): A = if (p(a)) ch hop a else a
  inline def hopOrMap[B](pf: PartialFunction[A, B])(using ch: CanHop[A]): B =
    if pf isDefinedAt a then pf(a)
    else ch hop a
}

extension [N, Y](ok: Ok[N, Y]) {
  inline def good[M >: N](using ch: CanHop[M]): Y = ok match
    case Yes(y) => y
    case No(n)  => throw ch hop n
  inline def bad[Z >: Y](using ch: CanHop[Z]): N = ok match
    case No(n)  => n
    case Yes(y) => throw ch hop y
}

extension (objectOk: Ok.type) {
  inline def hops[N, Y](f: CanHop[N] ?=> Y) =
    given ch: Hop.AnyImpl[N] = new Hop.AnyImpl[N]
    try { Yes(f) }
    catch { case h: Hop[_] if ch eq h => No(ch.value) }
  inline def hopsUnit[Y](f: CanHop.Unit ?=> Y) =
    given ch: Hop.UnitImpl = new Hop.UnitImpl
    try { Yes(f) }
    catch { case h: Hop[_] if ch eq h => Ok.UnitNo }
}

extension [L, R](either: Either[L, R]) {
  inline def good[K >: L](using ch: CanHop[K]): R = either match
    case Right(r) => r 
    case Left(l)  => ch hop l
  inline def bad[S >: R](using ch: CanHop[S]): L = either match
    case Left(l)  => l
    case Right(r) => ch hop r
}

extension (objectEither: Either.type)
  inline def hops[L, R](f: CanHop[L] ?=> R) =
    given ch: Hop.AnyImpl[L] = new Hop.AnyImpl[L]
    try { Right(f) }
    catch { case h: Hop[_] if ch eq h => Left(ch.value) }

extension [A](option: Option[A])
  inline def good(using ch: CanHop[Unit]): A = option match
    case Some(a) => a
    case _ => ch hop None

extension (objectOption: Option.type)
  inline def hops[A](f: CanHop.Unit ?=> A) =
    given ch: Hop.UnitImpl = new Hop.UnitImpl
    try { Some(f) }
    catch { case h: Hop[_] if ch eq h => None }
