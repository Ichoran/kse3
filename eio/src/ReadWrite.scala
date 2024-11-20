// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020, 2021 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.io._
import java.nio._
import java.nio.channels._
import java.nio.file._

import scala.annotation.targetName

import kse.basics.{given, _}
import kse.flow.{given, _}
import kse.maths.{given, _}


trait Send[A, -B] {
  inline final def apply(in: A, out: B): Ask[Long] = limited(Long.MaxValue)(in, out)
  def limited(count: Long)(in: A, out: B): Ask[Long]
}
object Send {
  val emptyByteArray = Array.empty[Byte]

  final class StreamToStream(maxBuffer: Int = 0x1000000)
  extends Send[InputStream, OutputStream] {
    private val maxb = maxBuffer.clamp(256, Int.MaxValue - 7)
    def limited(count: Long)(in: InputStream, out: OutputStream): Ask[Long] = nice:
      var buffer: Array[Byte] = new Array[Byte]( in.available.clamp(if maxb < 4096 then maxb else 4096, maxb) )
      var n = 0L
      var m = 0L max count
      while m-n > 0L do
        val k =
          if m-n >= buffer.length then in.read(buffer)
          else in.read(buffer, 0, (m-n).toInt)
        if k < 0 then m = 0L
        else
          out.write(buffer, 0, k)
          n += k
          if k == buffer.length && buffer.length < maxb && n - m > 2L*buffer.length then
            var bigger = (buffer.length *# 2) min maxb
            if (maxb - bigger).in(1, maxb >>> 3) then bigger = maxb
            buffer = new Array[Byte](bigger)
      out.flush
      n
  }

  final class StreamToChannel(maxBuffer: Int = 0x1000000, maxRetries: Int = 7, msDelayRetry: UByte = UByte(0), allowFullTarget: Boolean = false)
  extends Send[InputStream, WritableByteChannel] {
    private val maxb = maxBuffer.clamp(256, Int.MaxValue - 7)
    private val maxr = maxRetries.clamp(0, Int.MaxValue)
    private val msdr = msDelayRetry.toInt
    def limited(count: Long)(in: InputStream, out: WritableByteChannel): Ask[Long] = Ask:
      var data: Array[Byte] = new Array[Byte]( in.available.clamp(if maxb < 4096 then maxb else 4096, maxb) )
      var buffer: ByteBuffer = data.buffer()
      var n = 0L
      var m = 0L max count
      var k = 0
      while m - n > 0L do
        if k == 0 then
          k = if m-n >= data.length then in.read(data) else in.read(data, 0, (m-n).toInt)
        if k < 0 then m = 0L
        else
          buffer.position(0).limit(k)
          var nr = 0
          var r = out.write(buffer)
          while r < k && nr < maxr && r >= 0 do
            if msdr > 0 then Thread.sleep(msdr)
            nr += 1
            val w = out.write(buffer)
            if w < 0 then r = w else r += w 
          if r < 0 then
            n += buffer.position
            if allowFullTarget then m = 0
            else Err ?# s"Full output after $n bytes with at least ${buffer.remaining} remaining"
          else if r < k then
            Err ?# s"After $maxr tries, write $n bytes but at least ${buffer.remaining} remaining"
          else
            n += k
            if k == data.length && data.length < maxb && n - m > 2L*data.length then
              var bigger = (data.length *# 2) min maxb
              if (maxb - bigger).in(1, maxb >>> 3) then bigger = maxb
              data = new Array[Byte](bigger)
              buffer = data.buffer()
            k = 0
      n
  }

  final class ChannelToStream(maxBuffer: Int = 0x1000000, maxRetries: Int = 7, msDelayRetry: UByte = UByte(0))
  extends Send[ReadableByteChannel, OutputStream] {
    private val maxb = maxBuffer.clamp(256, Int.MaxValue - 7)
    private val maxr = maxRetries.clamp(0, Int.MaxValue)
    private val msdr = msDelayRetry.toInt
    def limited(count: Long)(in: ReadableByteChannel, out: OutputStream): Ask[Long] = Ask:
      val speculativeSize = in match
        case sbc: SeekableByteChannel => (sbc.size - sbc.position).clamp(0L, Int.MaxValue - 7).toInt
        case _ => 4096
      var data: Array[Byte] = new Array[Byte]( speculativeSize.clamp(if maxb < 4096 then maxb else 4096, maxb) )
      var buffer: ByteBuffer = data.buffer()
      var n = 0L
      var m = 0L max count
      var nz = 0
      while m - n > 0L do
        if m - n < data.length then buffer.limit((m - n).toInt)
        val k = in.read(buffer)
        if k < 0 then m = 0L
        else if k == 0 then
          nz += 1
          if nz > maxr then Err ?# s"Read $n bytes but failed to make further progress after $nz tries"
          else if msdr > 0 then Thread.sleep(msdr)
        else
          nz = 0
          out.write(data, 0, k)
          n += k
          if k == data.length && data.length < maxb && n - m > 2L*data.length then
            var bigger = (data.length *# 2) min maxb
            if (maxb - bigger).in(1, maxb >>> 3) then bigger = maxb
            data = new Array[Byte](bigger)
            buffer = data.buffer()
          else buffer.clear
      n
  }

  final class ChannelToChannel(maxBuffer: Int = 0x1000000, maxRetries: Int = 7, msDelayRetry: UByte = UByte(0), allowFullTarget: Boolean = false)
  extends Send[ReadableByteChannel, WritableByteChannel] {
    private val maxb = maxBuffer.clamp(256, Int.MaxValue - 7)
    private val maxr = maxRetries.clamp(0, Int.MaxValue)
    private val msdr = msDelayRetry.toInt
    def limited(count: Long)(in: ReadableByteChannel, out: WritableByteChannel): Ask[Long] = Ask:
      val speculativeSize = in match
        case sbc: SeekableByteChannel => (sbc.size - sbc.position).clamp(0L, Int.MaxValue - 7).toInt
        case _ => 4096
      var buffer = ByteBuffer.allocate( speculativeSize.clamp(if maxb < 4096 then maxb else 4096, maxb) )
      var n = 0L
      var m = 0L max count
      var nz = 0
      var k = 0
      var r = 0
      while m - n > 0L do
        if k == 0 then
          if m - n < buffer.capacity then buffer.limit((m - n).toInt)
          k = in.read(buffer)
          if k > 0 then
            buffer.flip
            nz = 0
        if k < 0 then
          m = 0L
        else if k == 0 then
          nz += 1
          if nz > maxr then Err ?# s"Read $n bytes but failed to make further progress after $nz tries"
          else if msdr > 0 then Thread.sleep(msdr)
        else
          val w = out.write(buffer)
          if w < 0 then
            n += buffer.position
            if allowFullTarget then m = 0
            else Err ?# s"Full output after $n bytes with at least ${buffer.remaining} remaining"
          else if w == 0 then
            nz += 1
            if nz > maxr then
              n += buffer.position
              Err ?# s"Read $n bytes but failed to make further progress after $nz tries"
            else if msdr > 0 then Thread.sleep(msdr)
          else
            nz = 0
            r += w
            if r >= k then
              n += k
              if k == buffer.capacity && buffer.capacity < maxb && n - m > 2L*buffer.capacity then
                var bigger = (buffer.capacity *# 2) min maxb
                if (maxb - bigger).in(1, maxb >>> 3) then bigger = maxb
                buffer = ByteBuffer.allocate(bigger)
              else buffer.clear
              k = 0
              r = 0
      n
  }

  final class MultiToStream()
  extends Send[MultiArrayChannel, OutputStream] {
    def limited(count: Long)(in: MultiArrayChannel, out: OutputStream): Ask[Long] = nice:
      if count == Long.MaxValue then in.read(out)
      else if count <= 0 then 0L
      else
        val nchunk = ((count - 1) / 0x20000000L) + 1
        val chunk = (count / nchunk).toInt
        var m = count
        var n = 0L
        while m - n > 0L do
          val h = if m - n < chunk then (m - n).toInt else chunk
          val k = in.read(out)(h)
          if k > 0 then n += k
          if k < h then m = 0
        n
  }

  final class MultiToChannel(maxRetries: Int = 7, msDelayRetry: UByte = UByte(0), allowFullTarget: Boolean = false)
  extends Send[MultiArrayChannel, WritableByteChannel] {
    private val maxr = maxRetries.clamp(0, Int.MaxValue)
    private val msdr = msDelayRetry.toInt
    def limited(count: Long)(in: MultiArrayChannel, out: WritableByteChannel): Ask[Long] = Ask:
      if count <= 0 then 0L
      else
        val nchunk = ((count - 1) / 0x20000000L) + 1
        val chunk = (count / nchunk).toInt
        var m = count
        var n = 0L
        var nz = 0
        while m - n > 0L do
          val h = if m - n < chunk then (m - n).toInt else chunk
          val k = in.read(out)(h)
          if k > 0 then
            nz = 0
            n += k
          else if k < 0 then
            if in.availableToRead == 0 || allowFullTarget then m = 0
            else Err ?# s"Output full with ${in.availableToRead min (m-n)} bytes remaining"
          else
            nz += 1
            if nz > maxr then
              Err ?# s"Made no progress writing after $nz tries with ${in.availableToRead min (m-n)} bytes remaining"
            else
              if msdr > 0 then Thread.sleep(msdr)
        n
  }

  final class IterBytesToStream(endline: Array[Byte] = emptyByteArray)
  extends Send[Iterator[Array[Byte]], OutputStream] {
    def limited(count: Long)(in: Iterator[Array[Byte]], out: OutputStream): Ask[Long] = nice:
      var m = 0L max count
      var n = 0L
      while in.hasNext && m - n > 0 do
        val ab = in.next
        if ab.length > 0 then
          out.write(ab)
          n += ab.length
        if endline.length > 0 then
          out.write(endline)
          n += endline.length
      n
  }

  final class IterBytesToChannel(endline: Array[Byte] = emptyByteArray, maxRetries: Int = 7, msDelayRetry: UByte = UByte(0), allowFullTarget: Boolean = false)
  extends Send[Iterator[Array[Byte]], WritableByteChannel] {
    private val maxr = maxRetries.clamp(0, Int.MaxValue)
    private val msdr = msDelayRetry.toInt
    private val ebuf = endline.buffer()
    def limited(count: Long)(in: Iterator[Array[Byte]], out: WritableByteChannel): Ask[Long] = Ask:
      var m = 0L max count
      var n = 0L
      while in.hasNext && m - n > 0 do
        val ab = in.next
        val passes = if endline.length == 0 then 1 else 2
        var i = if ab.length > 0 then 0 else 1
        while i < passes do
          val bb = if i == 0 then ab.buffer() else { ebuf.clear; ebuf }
          i += 1
          if m - n < bb.limit then bb.limit((m - n).toInt)
          var nr = 0
          while bb.remaining > 0 && nr <= maxr do
            val w = out.write(bb)
            if w < 0 then
              if allowFullTarget then Is.break(n + bb.position)
              else Err ?# s"Output channel full with at least ${bb.remaining} bytes remaining"
            if w == 0 then
              nr += 1
              if msdr > 0 then Thread.sleep(msdr)
            else nr = 0
            nr += 1
          if bb.remaining > 0 then Err ?# s"Made no progress writing after $nr attempts with at least ${bb.remaining} bytes remaining"
          n += bb.limit
      n
  }

  final class IterBytesToMulti(endline: Array[Byte] = emptyByteArray, allowFullTarget: Boolean = false)
  extends Send[Iterator[Array[Byte]], MultiArrayChannel] {
    def limited(count: Long)(in: Iterator[Array[Byte]], out: MultiArrayChannel): Ask[Long] = Ask:
      var m = 0L max count
      var n = 0L
      while in.hasNext & m - n > 0 do
        val ab = in.next
        val h = if m - n < ab.length then (m - n).toInt else ab.length
        val k = out.write(ab, 0)(h)
        if k < h then
          if allowFullTarget then Is.break(n + (k max 0))
          else Err ?# s"Output channel full with at least ${h - k} bytes remaining"
        n += h
        if endline.length > 0 then
          val g = if m - n < endline.length then (m - n).toInt else endline.length
          val j = out.write(endline, 0)(g)
          if j < g then
            if allowFullTarget then Is.break(n + (j max 0))
            else Err ?# s"Output channelf ull with at least ${g - j} bytes remaining"
          n += g
      n
  }

  final class IterStringToStream(endline: String = "\n")
  extends Send[Iterator[String], OutputStream] {
    private val actual = new IterBytesToStream(if endline.isEmpty then emptyByteArray else endline.bytes)
    def limited(count: Long)(in: Iterator[String], out: OutputStream): Ask[Long] =
      actual.limited(count)(in.map(_.bytes), out)
  }

  final class IterStringToChannel(endline: String = "\n", maxRetries: Int = 7, msDelayRetry: UByte = UByte(0), allowFullTarget: Boolean = false)
  extends Send[Iterator[String], WritableByteChannel] {
    private val actual = new IterBytesToChannel(if endline.isEmpty then emptyByteArray else endline.bytes, maxRetries, msDelayRetry, allowFullTarget)
    def limited(count: Long)(in: Iterator[String], out: WritableByteChannel): Ask[Long] =
      actual.limited(count)(in.map(_.bytes), out)
  }

  final class IterStringToMulti(endline: String = "\n", allowFullTarget: Boolean = false)
  extends Send[Iterator[String], MultiArrayChannel] {
    private val actual = new IterBytesToMulti(if endline.isEmpty then emptyByteArray else endline.bytes, allowFullTarget)
    def limited(count: Long)(in: Iterator[String], out: MultiArrayChannel): Ask[Long] =
      actual.limited(count)(in.map(_.bytes), out)
  }

  given stream2stream: Send[InputStream, OutputStream] = StreamToStream()
  given stream2channel: Send[InputStream, WritableByteChannel] = StreamToChannel()
  given channel2stream: Send[ReadableByteChannel, OutputStream] = ChannelToStream()
  given channel2channel: Send[ReadableByteChannel, WritableByteChannel] = ChannelToChannel()
  given multi2stream: Send[MultiArrayChannel, OutputStream] = MultiToStream()
  given multi2channel: Send[MultiArrayChannel, WritableByteChannel] = MultiToChannel()
  given iterbytes2stream: Send[Iterator[Array[Byte]], OutputStream] = IterBytesToStream()
  given iterbytes2channel: Send[Iterator[Array[Byte]], WritableByteChannel] = IterBytesToChannel()
  given iterbytes2multi: Send[Iterator[Array[Byte]], MultiArrayChannel] = IterBytesToMulti()
  given iterstring2stream: Send[Iterator[String], OutputStream] = IterStringToStream()
  given iterstring2channel: Send[Iterator[String], WritableByteChannel] = IterStringToChannel()
  given iterstring2multi: Send[Iterator[String], MultiArrayChannel] = IterStringToMulti()

  final class IterateInputStream(input: InputStream, initialSize: Int = 256, maxSize: Int = 4194304, reloadUnderfilled: Boolean = true)
  extends Iterator[Array[Byte]] {
    private var buffer: Array[Byte] = new Array[Byte](initialSize max 4)
    private var n = 0
    private var k = 0
    private var mightBeMore = true
    def hasNext: Boolean = mightBeMore
    def next: Array[Byte] =
      if n < 0 then throw new IOException("next on empty InputStream")
      else
        if (k & 3) == 3 then
          val h = buffer.length max ((buffer.length * 4L) min (maxSize.toLong min (Int.MaxValue - 7))).toInt
          if h > buffer.length then buffer = new Array[Byte](h)
        n = input.read(buffer)
        if reloadUnderfilled then
          if n < buffer.length then
            if n < 0 then mightBeMore = false
            else
              val m = input.read(buffer, n, buffer.length - n)
              if m < 0 then mightBeMore = false
              else if m > 0 then n += m
        else
          if n < 0 then mightBeMore = false
        if n == buffer.length && buffer.length < Int.MaxValue - 7 then k += 1
        buffer.copyToSize(n max 0)
  }

  final class IterateByteChannel(input: ReadableByteChannel, initialSize: Int = 256, maxSize: Int = 4194304, reloadUnderfilled: Boolean = true)
  extends Iterator[Array[Byte]] {
    private var buffer: Array[Byte] = new Array[Byte](initialSize max 4)
    private var bb: ByteBuffer = ByteBuffer.wrap(buffer)
    private var n = 0
    private var k = 0
    private var mightBeMore = true
    def hasNext: Boolean = mightBeMore
    def next: Array[Byte] =
      if n < 0 then throw new IOException("next on empty InputStream")
      else
        if (k & 3) == 3 then
          val h = buffer.length max ((buffer.length * 4L) min (maxSize.toLong min (Int.MaxValue - 7))).toInt
          if h > buffer.length then
            buffer = new Array[Byte](h)
            bb = ByteBuffer.wrap(buffer)
          else bb.clear()
        else bb.clear()
        n = input.read(bb)
        if reloadUnderfilled then
          if n < buffer.length then
            if n < 0 then mightBeMore = false
            else
              val m = input.read(bb)
              if m < 0 then mightBeMore = false
              else if m > 0 then n += m
        else
          if n < 0 then mightBeMore = false
        if n == buffer.length && buffer.length < Int.MaxValue - 7 then k += 1
        buffer.copyToSize(n max 0)
  }
}

extension (in: InputStream)
  def sendTo[B](out: B)(using tr: Send[InputStream, B]): Ask[Long] = tr(in, out)

extension (rbc: ReadableByteChannel)
  def sendTo[B](out: B)(using tr: Send[ReadableByteChannel, B]): Ask[Long] = tr(rbc, out)

extension (mac: MultiArrayChannel)
  def sendTo[B](out: B)(using tr: Send[MultiArrayChannel, B]): Ask[Long] = tr(mac, out)

extension (iter: IterableOnce[Array[Byte]]) {
  @targetName("ioArrayByteSendTo")
  def sendTo[B](out: B)(using tr: Send[Iterator[Array[Byte]], B]): Ask[Long] = tr(iter.iterator, out)

  @targetName("ioArrayByteWriteAt")
  def writeTo(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Resource.Nice(p.openWrite())(_.close): out =>
      var m = 0L
      val n = iter.iterator
        .map{ a => m += a.length; a }
        .fn(it => tr(it, out)).?
      if m != n then Err ?# s"Tried to write $m bytes but wrote $n to $p"

  @targetName("ioArrayByteAppendTo")
  def appendTo(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Resource.Nice(p.openAppend())(_.close): out =>
      var m = 0L
      val n = iter.iterator
        .map{ a => m += a.length; a }
        .fn(it => tr(it, out)).?
      if m != n then Err ?# s"Tried to write $m bytes but wrote $n to $p"

  @targetName("ioArrayByteCreateAt")
  def createAt(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Resource.Nice(p.openCreate())(_.close): out =>
      var m = 0L
      val n = iter.iterator
        .map{ a => m += a.length; a }
        .fn(it => tr(it, out)).?
      if m != n then Err ?# s"Tried to write $m bytes but wrote $n to $p"
}

extension (iter: IterableOnce[String]) {
  @targetName("ioStringSendTo")
  def sendTo[B](out: B)(using tr: Send[Iterator[String], B]): Ask[Long] = tr(iter.iterator, out)

  @targetName("ioStringWriteTo")
  def writeTo(p: Path)(using tr: Send[Iterator[String], OutputStream]): Ask[Unit] =
    Resource.Nice(p.openWrite())(_.close): out =>
      val it = iter.iterator
      tr(it, out)
      if it.hasNext then Err ?# s"Tried to write to $p but some content was not consumed"

  @targetName("ioStringAppendTo")
  def appendTo(p: Path)(using tr: Send[Iterator[String], OutputStream]): Ask[Unit] =
    Resource.Nice(p.openAppend())(_.close): out =>
      val it = iter.iterator
      tr(it, out)
      if it.hasNext then Err ?# s"Tried to write to $p but some content was not consumed"

  @targetName("ioStringCreateAt")
  def createAt(p: Path)(using tr: Send[Iterator[String], OutputStream]): Ask[Unit] =
    Resource.Nice(p.openCreate())(_.close): out =>
      val it = iter.iterator
      tr(it, out)
      if it.hasNext then Err ?# s"Tried to write to $p but some content was not consumed"
}

/*
object LineOutput {
  private def outputTo(pw: PrintWriter, separator: String, lines: IterableOnce[String]): Unit =
    try { if (separator eq null) lines.foreach(p.println) else lines.foreach(x => p.print(x + lineEnding)) }
    finally { pw.close() }

  private def compareLines(existing: Option[Array[String]], novel: Iterator[String]): Option[Iterator[String]] = existing match
    case Some(old) =>
      var same = true
      var line: String = null
      var i = 0
      while (same && i < old.length && novel.hasNext) {
        line = novel.next
        same = old(i) == line
        i += 1
      }
      if (same && i >= old.length && !novel.hasNext) None
      else if (same) Some(if (novel.hasNext) old.iterator ++ novel else old.iterator.take(i))
      else {
        val l = Iterator(line)
        if (i > 1) Some(if (novel.hasNext) old.iterator.take(i-1) ++ l ++ novel else old.iterator.take(i-1) ++ l)
        else       Some(if (novel.hasNext) l ++ novel else l)
      }
    case _ => Some(novel)

  def writeTo(f: File                   )(lines: IterableOnce[String]): Unit = writeTo(f, null)(lines)
  def writeTo(p: Path                   )(lines: IterableOnce[String]): Unit = writeTo(p, null)(lines)
  def writeTo(f: File, separator: String)(lines: IterableOnce[String]): Unit = outputTo(new PrintWriter(f), separator, lines)
  def writeTo(p: Path, separator: String)(lines: IterableOnce[String]): Unit = outputTo(new PrintWriter(Files newOutputStream p), separator, lines)

  def appendTo(f: File)(lines: IterableOnce[String]): Unit = appendTo(f, null)(lines)
  def appendTo(f: File, separator: String)(lines: IterableOnce[String]): Unit =
    if (!f.exists) writeTo(f, separator)(lines)
    else {
      val sepBytes = (if (separator ne null) separator else System.lineSeparator).getBytes
      val endBytes = new Array[Byte](sepBytes.length)
      val rw = new RandomAccessFile(f, "rw")
      try {
        val L = rw.length
        val endsNL = (sepBytes.length <= L) && {
          rw.seek(L - sepBytes.length)
          (rw.read(endBytes) == endBytes.length) && (0 until sepBytes.length).forall(i => endBytes(i) == sepBytes(i))
        }
        rw.seek(L)
        val p = new PrintWriter(new RandomAccessFileOutputStream(rw))
        if (!endsNL) { if (separator ne null) p print separator else p.println() }
        outputTo(pw, separator)(lines)
      }
      finally { rw.close() }     
    }

  def appendTo(p: Path)(lines: IterableOnce[String]): Unit = appendTo(p, null)(lines)
  def appendTo(p: Path, separator: String)(lines: IterableOnce[String]): Unit =
    if (!(Files exists p)) writeTo(p, separator)(lines)
    else {
      val sepBuf = ByteBuffer wrap (if (separator ne null) separator else System.lineSeparator).getBytes
      val endBuf = ByteBuffer wrap (new Array[Byte](sepBuf.capacity))
      val sbc = Files.newByteChannel(p, StandardOpenOption.READ, StandardOpenOption.WRITE)
      try {
        val L = sbc.size
        val endsNL = (sepBytes.length <= L) && {
          sbc.position(L-sepBytes.length)
          (sbc.read(endBuf) == endBytes.length) && { 
            endBuf.flip
            var same = true
            while (same && endBuf.hasRemaining && sepBuf.hasRemaining) same = endBuf.get == sepBbuf.get
            same && !endBuf.hasRemaining && !sepBuf.hasRemaining
          }
        }
        sbc.position(L)
        if (!endsNL) { sepBuf.clear; if (sbc.write(sepBuf) != sepBuf.capacity) throw new IOException("Could not add newline") }
        outputTo(new PrintWriter(new SeekableByteChannelOutputStream(sbc)), separator, lines)
      }
      finally { sbc.close() }
    }

  def updateTo(f: File)(lines: IterableOnce[String]): Boolean =
    compareLines(if (f.exists) f.slurp.toOption else None)(lines) match
      case Some(i) => writeTo(f)(i); true
      case _ => false

  def updateTo(p: Path)(lines: IterableOnce[String]): Boolean =
    compareLines(if (Files exists p) p.slurp.toOption else None)(lines) match
      case Some(i) => writeTo(p)(i); true
      case _ => false

  def atomicWrite(f: File)(lines: IterableOnce[String]): Unit = atomicWrite(f, null)(lines)
  def atomicWrite(f: File, separator: String)(lines: IterableOnce[String]): Unit =
    val a = f.nameFn(_ + ".atomic")
    writeTo(a, separator)(lines)
    Files.move(a.toPath, f.toPath, StandardCopyOption.ATOMIC_MOVE)

  def atomicWrite(p: Path)(lines: IterableOnce[String]): Unit = atomicWrite(p, null)(lines)
  def atomicWrite(p: Path, separator: String)(lines: IterableOnce[String]): Unit =
    val a = p.nameFn(_ + ".atomic")
    writeTo(a, separator)(lines)
    Files.move(a, p, StandardCopyOption.ATOMIC_MOVE)

  def atomicUpdate(f: File)(lines: IterableOnce[String]): Boolean =
    compareLines(if (f.exists) f.slurp.toOption else None)(lines) match
      case Some(i) =>
        val a = f.nameFn(_ + ".atomic")
        writeTo(a)(i)
        Files.move(a.toPath, f.toPath, StandardCopyOption.ATOMIC_MOVE)
        true
      case _ => false

  def atomicUpdate(p: Path)(lines: IterableOnce[String]): Boolean =
    compareLines(if (Files exists p) p.slurp.toOption else None)(lines) match
      case Some(i) =>
        val a = p.nameFn(_ + ".atomic")
        writeTo(a)(i)
        Files.move(a, p, StandardCopyOption.ATOMIC_MOVE)
        true
      case _ => false
}

extension (lines: IterableOnce[String])
  def writeTo(f: File): Unit                    = LineOutput.writeTo(f           )(lines)
  def writeTo(f: File, separator: String): Unit = LineOutput.writeTo(f, separator)(lines)
  def writeTo(p: Path): Unit                    = LineOutput.writeTo(p           )(lines)
  def writeTo(p: Path, separator: String): Unit = LineOutput.writeTo(p, separator)(lines)
  def appendTo(f: File): Unit                    = LineOutput.appendTo(f           )(lines)
  def appendTo(f: File, separator: String): Unit = LineOutput.appendTo(f, separator)(lines)
  def appendTo(p: Path): Unit                    = LineOutput.appendTo(p           )(lines)
  def appendTo(p: Path, separator: String): Unit = LineOutput.appendTo(p, separator)(lines)
  def updateTo(f: File): Boolean = LineOutput.updateTo(f)(lines)
  def updateTo(p: Path): Boolean = LineOutput.updateTo(p)(lines)
  def atomicWrite(f: File): Unit                    = LineOutput.atomicWrite(f           )(lines)
  def atomicWrite(f: File, separator: String): Unit = LineOutput.atomicWrite(f, separator)(lines)
  def atomicWrite(p: Path): Unit                    = LineOutput.atomicWrite(p           )(lines)
  def atomicWrite(p: Path, separator: String): Unit = LineOutput.atomicWrite(p, separator)(lines)
  def atomicUpdate(f: File): Boolean = LineOutput.atomicUpdate(f)(lines)
  def atomicUpdate(p: Path): Boolean = LineOutput.atomicUpdate(p)(lines)


object BlockOutput {
  def writeTo(f: File)(block: Array[Byte]): Unit = writeTo(f.toPath)(block)
  def writeTo(p: Path)(block: Array[Byte]): Unit = Files.write(p, block)
  def appendTo(f: File)(block: Array[Byte]): Unit = appendTo(f.toPath)(block)
  def appendTo(p: Path)(block: Array[Byte]): Unit = Files.write(p, block, StandardOpenOption.APPEND)
  def updateTo(f: File)(block: Array[Byte]): Boolean =
    if (f.exists && f.gulp.exists(x => java.util.Arrays.equals(x, block))) false
    else { writeTo(f)(block); true }
  def updateTo(p: Path)(block: Array[Byte]): Boolean =
    if ((Files exists p) && p.gulp.exists(x => java.util.Arrays.equals(x, block))) false
    else { writeTo(p)(block); true }
  def atomicWrite(f: File)(block: Array[Byte]): Unit =
    val a = f.nameFn(_ + ".atomic")
    writeTo(a)(block)
    Files.move(a.toPath, f.toPath, StandardCopyOption.ATOMIC_MOVE)
  def atomicWrite(p: Path)(block: Array[Byte]): Unit =
    val a = p.nameFn(_ + ".atomic")
    writeTo(p)(block)
    Files.move(a, p, StandardCopyOption.ATOMIC_MOVE)
  def atomicUpdate(f: File)(block: Array[Byte]): Boolean =
    if (f.exists && f.gulp.exists(x => java.util.Arrays.equals(x, block))) false
    else { atomicWrite(f)(block); true }
  def atomicUpdate(p: Path)(block: Array[Byte]): Boolean =
    if ((Files exists p) && p.gulp.exists(x => java.util.Arrays.equals(x, block))) false
    else { atomicWrite(p)(block); true }
}

extension (data: Array[Byte])
  def writeTo(f: File): Unit = BlockOutput.writeTo(f)(data)
  def writeTo(p: Path): Unit = BlockOutput.writeTo(p)(data)
  def appendTo(f: File): Unit = BlockOutput.appendTo(f)(data)
  def appendTo(p: Path): Unit = BlockOutput.appendTo(p)(data)
  def updateTo(f: File): Unit = BlockOutput.updateTo(f)(data)
  def updateTo(p: Path): Unit = BlockOutput.updateTo(p)(data)
  def atomicWrite(f: File): Unit = BlockOutput.atomicWrite(f)(data)
  def atomicWrite(p: Path): Unit = BlockOutput.atomicWrite(p)(data)
  def atomicUpdate(f: File): Unit = BlockOutput.atomicUpdate(f)(data)
  def atomicUpdate(p: Path): Unit = BlockOutput.atomicUpdate(p)(data)
*/
