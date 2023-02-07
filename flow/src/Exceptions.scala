// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2021-23 Rex Kerr, UCSF, and Calico Life Sciences LLC.

package kse.flow

import scala.util.control.ControlThrowable
import scala.util.boundary.Break


extension (t: Throwable) {
  /** Indicates that a throwable is meant to be caught as a normal part of control flow. */
  inline def catchable: Boolean = t match
    case _: VirtualMachineError | _: ThreadDeath | _: InterruptedException | _: LinkageError | _: ControlThrowable | _: Break[_] => false
    case _ => true

  /** Indicates that a throwable should be caught rather than letting it kill the current thread. */
  inline def threadCatchable: Boolean = t match
    case _: VirtualMachineError | _: ThreadDeath | _: InterruptedException | _: LinkageError => false
    case _ => true
}


object ExceptionExplainer {
  private val emptyExplanation = new Array[String](0)

  /** Converts a Throwable into an array of strings by gathering
    * the Throwable's messsage, stack trace, and any sub-Throwables
    * line-by-line (one line per stack trace element).
    * 
    * `lines` is the maximum number of lines available for all output;
    * if truncated, the output will say what is missing in the last line.
    * 
    * If `showSuppressed` is false, the existence of suppressed Throwables
    * will be noted, but their stack traces will not be printed, nor will
    * they be followed to see what, if any, children they have.
    * 
    * `childLines` is the maximum number of stack trace lines
    * for each child's stack trace (either cause or suppressed).  
    * 
    * Causes are indented with `"| "`.  Suppressed Throwables are indented
    * with `"# "` or `"% "` (alternating, for easy visual delimitation).
    */
  def explainAsArray(throwable: Throwable, lines: Int = Int.MaxValue, showSuppressed: Boolean = false, childLines: Int = Int.MaxValue): Array[String] =
    if lines <= 0 then return emptyExplanation

    val seen = new scala.collection.mutable.LongMap[List[Throwable]]
    def novel(t: Throwable): Boolean =
      val h = System.identityHashCode(t)
      if seen.contains(h) then
        val ts = seen(h)
        if ts.exists(_ eq t) then false
        else
          seen(h) = t :: ts
          true
      else
        seen(h) = t :: Nil
        true

    val ans = Array.newBuilder[String]
    var n = lines
    def storeStackLines(lines: Array[StackTraceElement], prefix: String): Unit =
      ans ++= (if prefix.isEmpty then lines.map(_.toString) else lines.map(l => s"$prefix$l"))
      n -= lines.length
    def loadStackTrace(trace: Array[StackTraceElement], willBeMore: Boolean, prefix: String, extraLimit: Int = Int.MaxValue): Int =
      val available = if willBeMore then n-1 else n
      if trace.length <= available && trace.length <= extraLimit then  // Full trace fits
        storeStackLines(trace, prefix)
        0
      else if trace.length <= extraLimit || available <= extraLimit then  // Full trace does not fit because overall limit exceeded
        val missing = trace.length - (n - 1)
        storeStackLines(trace take (n - 1), prefix)
        missing
      else
        val missing = trace.length - (extraLimit-1)
        storeStackLines(trace take (extraLimit-1), prefix)
        ans += s"${prefix}. . .(+$missing lines)"
        n -= 1
        0

    var missed = 0
    var missedLines = 0
    var writing = true
    var active = ((throwable, "")) :: Nil
    while active.nonEmpty do
      val (t, prefix) = active.head
      active = active.tail

      val sups = t.getSuppressed
      val c = t.getCause
      val hasChildren = (c ne null) || sups.nonEmpty
      val hasMore = ((c ne null) && childLines > 0) || (sups.nonEmpty && showSuppressed)

      if n > 0 && writing then
        val trace = t.getStackTrace()

        if n > 1 || (n == 1 && trace.isEmpty && !hasMore) then
          val who =
            if prefix.isEmpty then ""
            else if prefix.endsWith("| ") then "CAUSE: "
            else "SUPPRESSES: "
          ans += s"$prefix$who${t.toString}"
          n -= 1
        else
          writing = false
          missed += 1

        if writing && hasChildren && (n > 1 || (n == 1 && trace.isEmpty && !hasMore)) then
          ans += (
            if (c eq null) then s"${prefix}SUPPRESSED ${sups.length}"
            else if sups.length == 0 then s"${prefix}CAUSED BY ${c.toString}"
            else s"${prefix}SUPPRESSED ${sups.length} and CAUSED BY ${c.toString}"
          )
          n -= 1

        if writing && trace.nonEmpty && n > 0 then
          missedLines = loadStackTrace(trace, hasMore, prefix, if prefix.isEmpty then Int.MaxValue else childLines)
          if missedLines > 0 then
            writing = false
            if !hasMore && n > 0 then
              ans += s"${prefix}. . . (+$missedLines lines)"
              missedLines = 0
      else
        missed += 1

      // Since we're using a stack but want sub-exceptions in forward order, we need to accumulate and restack backwards
      var more: List[(Throwable, String)] = Nil
      if (c ne null) && childLines > 0 && novel(c) then
        more = ((c, prefix + "| ")) :: more
      if sups.nonEmpty && showSuppressed then
        sups.zipWithIndex.foreach{ case (s, i) =>
          if novel(s) then
            more = ((s, prefix + (if i % 2 == 0 then "# " else "% "))) :: more
        }
      while more.nonEmpty do
        active = more.head :: active
        more = more.tail

    def plural(i: Int): String = if i == 1 then "" else "s"

    if missedLines > 0 && missed > 0 then
      ans += s". . . (+$missedLines line${plural(missedLines)} and $missed more exception${plural(missed)})"
    else if missed > 0 then
      ans += s". . . (+$missed more exception${plural(missed)})"
    else if missedLines > 0 then
      ans += s". . . (+$missedLines lines)"

    ans.result

  /** Converts a Throwable into a Vector of strings by gathering
    * the Throwable's messsage, stack trace, and any sub-Throwables
    * line-by-line.
    * 
    * See `explainAsArray` for details.
    */
  def explainAsVector(throwable: Throwable, lines: Int = Int.MaxValue, seeSuppressed: Boolean = false, childLines: Int = Int.MaxValue): Vector[String] = 
    explainAsArray(throwable, lines, seeSuppressed, childLines).toVector

  /** Converts a Throwable into a String by gathering
    * the Throwable's messsage, stack trace, and any sub-Throwables
    * line-by-line, then concatenating them with interspersed `"\n"`s.
    * 
    * See `explainAsArray` for details.
    */
  def explain(throwable: Throwable, lines: Int = Int.MaxValue, seeSuppressed: Boolean = false, childLines: Int = Int.MaxValue): String = 
    explainAsArray(throwable, lines, seeSuppressed, childLines).mkString("\n")
}

extension (throwable: Throwable) {
  /** Converts this Throwable into an Array of strings by listing out
    * the stack trace, plus any causes if `childLines` is positive.
    * 
    * See `ExceptionExplainer.explainAsArray`.
    */
  def explainAsArray(lines: Int = Int.MaxValue, childLines: Int = Int.MaxValue): Array[String] =
    ExceptionExplainer.explainAsArray(throwable, lines, false, childLines)

  /** Converts this Throwable into a Vector of strings by listing out
    * the stack trace, plus any causes if `childLines` is positive.
    * 
    * See `ExceptionExplainer.explainAsVector`.
    */
  def explainAsVector(lines: Int = Int.MaxValue, childLines: Int = Int.MaxValue): Vector[String] = 
    ExceptionExplainer.explainAsVector(throwable, lines, false, childLines)

  /** Converts this Throwable into a String by listing out the
    * stack trace, plus any causes if `childLines` is positive;
    * lines are separated by newlines.
    * 
    * See `ExceptionExplainer.explain`
    */
  def explain(lines: Int = Int.MaxValue, childLines: Int = Int.MaxValue): String =
    ExceptionExplainer.explain(throwable, lines, false, childLines)


  /** Converts this Throwable into an Array of strings by listing out
    * the stack trace, plus any causes or suppressed Throwables
    * if `childLines` is positive.
    * 
    * See `ExceptionExplainer.explainAsArray`.
    */
  def explainSuppressedAsArray(lines: Int = Int.MaxValue, childLines: Int = Int.MaxValue): Array[String] =
    ExceptionExplainer.explainAsArray(throwable, lines, true, childLines)

  /** Converts this Throwable into an Vector of strings by listing out
    * the stack trace, plus any causes or suppressed Throwables
    * if `childLines` is positive.
    * 
    * See `ExceptionExplainer.explainAsVector`.
    */
  def explainSuppressedAsVector(lines: Int = Int.MaxValue, childLines: Int = Int.MaxValue): Vector[String] = 
    ExceptionExplainer.explainAsVector(throwable, lines, true, childLines)

  /** Converts this Throwable into a String by listing out the
    * stack trace, plus any causes or suppressed Throwables
    * if `childLines` is positive; lines are separated by newlines.
    * 
    * See `ExceptionExplainer.explain`
    */
  def explainSuppressed(lines: Int = Int.MaxValue, childLines: Int = Int.MaxValue): String =
    ExceptionExplainer.explain(throwable, lines, true, childLines)
}

/** Provides the ability to cope with errors, either Throwable or a String message,
  * by converting to an error type `E` that can, for instance, be stored and passed
  * as a disfavored branch in an `Or`.
  */
trait Cope[E] {
  def fromThrowable(t: Throwable): E
  def fromThrowable(t: Throwable, cause: E): E = fromCope(fromThrowable(t), cause)
  def fromString(s: String): E
  def fromString(s: String, cause: E): E = fromCope(fromString(s), cause)
  def fromCope(error: E, cause: E): E
}
object Cope {
  /** Conversion from one method of coping with errors to another. */
  trait From[F, E] extends Cope[E] {
    def from(previous: F): E
    def from(previous: F, cause: E): E = fromCope(from(previous), cause)
  }
  object From {
    def apply[F, E](f: F => E, merge: (E, E) => E)(using cope: Cope[F]): Cope.From[F, E] = new Cope.From[F, E] {
      def fromThrowable(t: Throwable): E = f(cope fromThrowable t)
      def fromString(s: String): E = f(cope fromString s)
      def fromCope(error: E, cause: E): E = merge(error, cause)
      def from(previous: F): E = f(previous)
    }
  }

  final class StoredMessageException(message: String)
  extends Exception(message) {
    override def fillInStackTrace(): Throwable = this
  }

  final class MultipleExceptionsException(val exceptions: List[Throwable])
  extends Exception(
    if exceptions.nonEmpty then exceptions.head.getMessage else null,
    if exceptions.nonEmpty then
      if exceptions.head.isInstanceOf[StoredMessageException] then
        if exceptions.tail.nonEmpty then exceptions.tail.head
        else null
      else exceptions.head
    else null
  ) {
    exceptions match
      case (sme: StoredMessageException) :: t :: more => more.reverse.foreach(this addSuppressed _)
      case (sme: StoredMessageException) :: Nil =>
      case t :: more => more.reverse.foreach(this addSuppressed _)
      case _ =>

    override def fillInStackTrace(): Throwable = this
  }

  /** The default coping strategy: errors are Strings, with Throwables printed out. */
  given Cope[String] = new Cope[String] { 
    def fromThrowable(t: Throwable)          = t.explainSuppressed(60, 12)
    def fromString(s: String)                = s
    def fromCope(error: String, why: String) = s"$error\n$why"
  }

  val fullTrace = new Cope[Array[String]] {
    def fromThrowable(t: Throwable)                        = t.explainSuppressedAsArray()
    def fromString(s: String)                              = s.linesIterator.toArray
    def fromCope(error: Array[String], why: Array[String]) = error ++ why
  }

  val asException = new Cope[Throwable] {
    def fromThrowable(t: Throwable) = t
    def fromString(s: String) = new StoredMessageException(s)
    def fromCope(t: Throwable, u: Throwable) = t match
      case mee: MultipleExceptionsException => u match
        case yuu: MultipleExceptionsException =>
          if mee.exceptions.isEmpty then yuu
          else if yuu.exceptions.isEmpty then mee
          else new MultipleExceptionsException(mee.exceptions ++ yuu.exceptions)
        case _ => new MultipleExceptionsException(mee.exceptions :+ u)
      case _ => u.match
        case yuu: MultipleExceptionsException =>
          new MultipleExceptionsException(t :: yuu.exceptions)
        case _ =>
          new MultipleExceptionsException(t :: u :: Nil)
  }

  val stored = new Cope[List[String | Throwable]] {
    def fromThrowable(t: Throwable): List[String | Throwable] = t :: Nil
    def fromString(s: String):       List[String | Throwable] = s :: Nil
    def fromCope(a: List[String | Throwable], b: List[String | Throwable]) = a ::: b
  }

  val fullText = From[Array[String], String](_.mkString("\n"), (a: String, b: String) => s"$a\n$b")(using fullTrace)
}

/** An exception specifically to reify the idea of the disfavored branch of a sum type
  * (`Or`, `Either`, etc.) being packed into a `Try`: the disfavored branch of `Try`
  * _must_ be a `Throwable`.
  * 
  * The exception does compute a stack trace because if you didn't need a stack trace
  * you probably wouldn't be using `Try` to begin with.
  */
final case class WrongBranchException[+W](value: W) extends Exception {
  override def getMessage: String = value.toString
}
