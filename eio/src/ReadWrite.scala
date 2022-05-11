// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020, 2021 Rex Kerr, UCSF, and Calico Life Sciences LLC

/*
package kse.eio

import java.io._
import java.nio._
import java.nio.channels._
import java.nio.file._


class RandomAccessFileOutputStream(val raf: RandomAccessFile) extends OutputStream {
  override def close(): Unit = raf.close()
  override def write(b: Array[Byte]): Unit = raf.write(b)
  override def write(b: Array[Byte], off: Int, len: Int): Unit = raf.write(b, off, len)
  def write(b: Int): Unit = raf.writeByte(b)
}

class SeekableByteChannelOutputStream(val sbc: SeekableByteChannel) extends OutputStream {
  private val oneByte = ByteBuffer.wrap(new Array[Byte](1))
  override def close(): Unit = sbc.close()
  override def write(b: Array[Byte]): Unit =
    val bb = ByteBuffer wrap b
    val n = sbc.write(bb)
    if (n < b.length) throw new IOException("Tried to write ${b.length} bytes but only could write $n")
  override def write(b: Array[Byte], off: Int, len: Int): Unit =
    if (len > 0) {
      val bb = ByteBuffer wrap b
      b.position = off
      b.limit = off + len
      val n = sbc.write(bb)
      if (n < len) throw new IOException("Tried to write ${b.length} bytes but could only write $n")
    }
  override def write(b: Int): Unit = synchronized {
    oneByte.clear
    oneByte put b.toByte
    oneByte.flip
    val n = sbc.write(oneByte)
    if (n != 1) throw new IOException("Tried to write a byte but couldn't")
  }
}

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