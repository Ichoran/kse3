// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.eio


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.lang.foreign.{Arena, Linker, FunctionDescriptor}
import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_INT, JAVA_LONG, JAVA_SHORT}
import java.nio.file.{Files, Path}
import java.util.concurrent.{SynchronousQueue, TimeUnit}


/** The platform-native layer — `PosixSocket`, `FdSock`, `SharedMemory` — run against the kernel it is on.
  * This class is what the cross-platform CI runs by itself on every OS, so everything that differs between
  * Linux, macOS and Windows is pinned here by a real system call rather than by reading the table back:
  * struct layouts (any mistake is `EINVAL` or garbage on the first send), errno values, the per-message
  * descriptor limit, close-on-exec on a received descriptor, `shm_open`'s variadic mode on Darwin, the
  * named shared-memory object on all three, and Windows answering "unsupported" for descriptor passing.
  */
@RunWith(classOf[JUnit4])
class NativeTest {
  import kse.basics.testutilities.TestUtilities.{given, _}
  import kse.basics.{given, _}
  import kse.flow.{given, _}
  import kse.maths.{given, _}
  import kse.eio.{given, _}
  import kse.loom.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  private def must[A](a: Ask[A]): A = a.fold(x => x)(_.toss)

  /** The errno inside a failed `Ask`, or -1 for success or a failure that is not a system call's. */
  private def errnoOf[A](a: Ask[A]): Int = a.fold(_ => -1)(e => PosixSocket.errnoOf(e))

  private def handoff[X](sq: SynchronousQueue[X]): X =
    val x = sq.poll(10, TimeUnit.SECONDS)
    if x == null then throw new RuntimeException("handoff timed out")
    x

  /** A scratch directory whose socket paths stay well inside `sockaddr_un` (103 bytes on Darwin, whose
    * default temp directory is already ~50 of them).
    */
  private def socketDir(prefix: String): Path =
    val d = Files.createTempDirectory(prefix)
    if d.toString.length <= 70 || !Files.isDirectory(Path.of("/tmp")) then d
    else
      Files.deleteIfExists(d) __ Unit
      Files.createTempDirectory(Path.of("/tmp"), prefix)

  /** Whether `fd` is close-on-exec, straight from `fcntl(F_GETFD)`. */
  private def isCloexec(fd: Int): Boolean =
    val tmp = Arena.ofConfined()
    try
      val fl: Int = PosixSocket.Sys.fcntl.invoke(PosixSocket.capture(tmp), fd, PosixSocket.F_GETFD, 0)
      fl >= 0 && (fl & PosixSocket.FD_CLOEXEC) != 0
    finally tmp.close()

  /** A fresh `AF_UNIX` socket of `sockType` connected to nothing. */
  private def loneSocket(sockType: Int): Int =
    val tmp = Arena.ofConfined()
    try (PosixSocket.Sys.socket.invoke(PosixSocket.capture(tmp), PosixSocket.AF_UNIX, sockType, 0): Int)
    finally tmp.close()

  /** macOS: the permission bits of the POSIX shared-memory object `name`, read by `fstat` on a fresh
    * read-only `shm_open`, or -1 if it cannot be opened.  `st_mode` is at byte 4 of Darwin's 64-bit-inode
    * `struct stat`, which is plain `fstat` on arm64 and `fstat$INODE64` on x86_64.
    */
  private def darwinShmMode(name: String): Int =
    val linker = Linker.nativeLinker()
    val look = linker.defaultLookup()
    val shmOpen = linker.downcallHandle(
      look.find("shm_open").orElseThrow(),
      FunctionDescriptor.of(JAVA_INT, ADDRESS, JAVA_INT, JAVA_INT), Linker.Option.firstVariadicArg(2)
    )
    val fstat = linker.downcallHandle(
      look.find("fstat$INODE64").or(() => look.find("fstat")).orElseThrow(),
      FunctionDescriptor.of(JAVA_INT, JAVA_INT, ADDRESS)
    )
    val tmp = Arena.ofConfined()
    try
      val fd: Int = shmOpen.invoke(tmp.allocateFrom(name), 0, 0)
      if fd < 0 then -1
      else
        try
          val st = tmp.allocate(512L)
          if (fstat.invoke(fd, st): Int) != 0 then -1 else st.get(JAVA_SHORT, 4L) & 0xFFF
        finally PosixSocket.closeQuietly(fd)
    finally tmp.close()

  private val windows = System.getProperty("os.name", "").toLowerCase.contains("win")

  /** A python3 that can run `check`, if there is one (Windows runners spell it `python`). */
  private def pythonWith(check: String): Option[String] =
    val candidates = if windows then List("python", "python3") else List("/usr/bin/python3", "python3")
    candidates.find{ py =>
      try
        val p = new ProcessBuilder(py, "-c", check).start()
        val done = p.waitFor(20, TimeUnit.SECONDS)
        if !done then p.destroyForcibly() __ Unit
        done && p.exitValue == 0
      catch case e if e.catchable => false
    }

  /** Speaks `SCM_RIGHTS` (`socket.send_fds`, 3.9+) and POSIX shared memory: the descriptor-passing peer. */
  private lazy val pythonFds: Option[String] = pythonWith("import socket, multiprocessing.shared_memory; socket.send_fds")

  /** Has `multiprocessing.shared_memory` (3.8+): the named-object peer, on Windows too. */
  private lazy val pythonShm: Option[String] = pythonWith("import multiprocessing.shared_memory")

  /** Retries an `Ask` for up to five seconds, for a peer that is still getting started. */
  private def eventually[A](a: => Ask[A]): Ask[A] =
    var r = a
    var tries = 0
    while r.isAlt && tries < 200 do
      Thread.sleep(25)
      r = a
      tries += 1
    r

  private def startPython(py: String, dir: Path, name: String, script: String, args: String*): Process =
    val file = dir.resolve(name)
    Files.writeString(file, script) __ Unit
    new ProcessBuilder((py :: file.toString :: args.toList)*).start()

  /** The trimmed standard output of a finished python, or a description of how it failed. */
  private def finish(p: Process): String =
    val done = p.waitFor(20, TimeUnit.SECONDS)
    if !done then p.destroyForcibly() __ Unit
    val out = new String(p.getInputStream.readAllBytes()).trim
    val err = new String(p.getErrorStream.readAllBytes()).trim
    if done && p.exitValue == 0 then out
    else s"python ${if done then s"exit ${p.exitValue}" else "timed out"}: $out $err".trim


  @Test
  def errnoAsDataTest(): Unit =
    // pure: the table, the count-or-code Result, and the code kept inside an Err; runs on Windows too
    T ~ FdSock.supported                                                 ==== (PosixSocket.linux || PosixSocket.mac)
    T ~ PosixSocket.Errno.name(PosixSocket.Errno.EAGAIN)                 ==== "EAGAIN"
    T ~ PosixSocket.Errno.name(PosixSocket.Errno.ECONNREFUSED)           ==== "ECONNREFUSED"
    T ~ PosixSocket.Errno.name(123456)                                   ==== "errno 123456"
    T ~ PosixSocket.Errno.wouldBlock(PosixSocket.Errno.EWOULDBLOCK)      ==== true
    T ~ PosixSocket.Result.success(7L).count                             ==== 7L
    T ~ PosixSocket.Result.success(7L).ok                                ==== true
    T ~ PosixSocket.Result.failure(PosixSocket.Errno.EAGAIN).wouldBlock  ==== true
    T ~ PosixSocket.Result.failure(PosixSocket.Errno.EAGAIN).backpressure ==== true
    T ~ PosixSocket.Result.failure(PosixSocket.Errno.EPIPE).peerGone     ==== true
    T ~ PosixSocket.Result.failure(PosixSocket.Errno.EPIPE).count        ==== 0L
    T ~ PosixSocket.Result.failure(PosixSocket.Errno.EPIPE).errno        ==== PosixSocket.Errno.EPIPE
    T ~ PosixSocket.Result.failure(PosixSocket.Errno.EINTR).name         ==== "EINTR"
    T ~ PosixSocket.Result.success(3L).ask("send").fold(x => x)(_ => -1L) ==== 3L
    val bad = PosixSocket.Result.failure(PosixSocket.Errno.ENOENT).ask("open", "the thing")
    T ~ errnoOf(bad)                                                     ==== PosixSocket.Errno.ENOENT
    T ~ bad.fold(_ => "")(e => e.toString)                               ==== "open failed: ENOENT (the thing)"
    T ~ errnoOf(bad.mapAlt(_ +# "while starting"))                       ==== PosixSocket.Errno.ENOENT
    T ~ PosixSocket.errnoOf(Err("just words"))                           ==== -1


  @Test
  def unsupportedPlatformTest(): Unit =
    // Windows: every descriptor-passing entry point answers an Err that says so, touches no native code,
    // and leaves nothing behind
    if !FdSock.supported then
      def unsupported[A](a: Ask[A]): Boolean = a.fold(_ => false)(e => e.toString.contains("unsupported"))
      val dir = Files.createTempDirectory("kse-nofd-")
      val sock = dir.resolve("never.sock")
      T ~ unsupported(FdSock.listen(sock, 1.s))                    ==== true
      T ~ unsupported(FdSock.connect(sock, 1.s))                   ==== true
      T ~ unsupported(FdSock.adopt(0))                             ==== true
      T ~ unsupported(FdSock.Raw.adopt(0))                         ==== true
      T ~ unsupported(FdSock.Raw.pair())                           ==== true
      T ~ unsupported(SharedMemory.createFd[Long](4))              ==== true
      T ~ unsupported(SharedMemory.attachFdBytes[Long](0, 32L, false)) ==== true
      T ~ unsupported(SharedMemory.offerFd[Long](sock, 4, 1.s))    ==== true
      T ~ unsupported(Resource.nice(SharedMemory.acceptFd[Long](sock, timeout = 1.s))(_.close())(_ => 0)) ==== true
      T ~ sock.exists                                              ==== false
      Files.deleteIfExists(dir) __ Unit


  @Test
  def rawSocketTest(): Unit =
    if FdSock.supported then
      val missing = socketDir("kse-raw-").resolve("nothing.sock")
      T ~ errnoOf(FdSock.connect(missing, 1.s)) ==== PosixSocket.Errno.ENOENT
      Files.deleteIfExists(missing.getParent) __ Unit

      // Darwin's local domain registers only STREAM and DGRAM, so a SEQPACKET pair is refused there
      val seq = FdSock.Raw.pair(PosixSocket.SOCK_SEQPACKET)
      T ~ seq.isIs ==== !PosixSocket.mac
      seq.foreach(p => { p._1.close(); p._2.close() })
      val packet = if PosixSocket.mac then PosixSocket.SOCK_DGRAM else PosixSocket.SOCK_SEQPACKET

      // a message-preserving pair, non-blocking, polled: one call per datagram, nothing hidden
      val (a, b) = must(FdSock.Raw.pair(packet))
      val poller = new PosixSocket.Poller(2)
      try
        T ~ must(a.sockType)                     ==== packet
        T ~ isCloexec(a.descriptor)              ==== true
        T ~ isCloexec(b.descriptor)              ==== true
        T ~ must(a.isNonblocking)                ==== false
        T ~ a.nonblocking().isIs                 ==== true
        T ~ b.nonblocking().isIs                 ==== true
        T ~ must(a.isNonblocking)                ==== true
        val buf = Mem.alloc[Byte](64L)
        T ~ a.recvMsg(buf).wouldBlock            ==== true     // the kernel's EAGAIN, matched against the table's
        poller.set(0, a.descriptor)
        poller.set(1, b.descriptor)
        T ~ poller.poll(50).count                ==== 0L
        val msg = Mem.alloc[Byte](5L)                 // off-heap: the kernel reads it in place
        (Mem of "hello".bytes).inject(msg) __ Unit
        T ~ b.sendOnce(msg).count                ==== 5L
        T ~ b.sendOnce(msg, 2L).count            ==== 2L
        T ~ poller.poll(1000).count              ==== 1L
        T ~ poller.readable(0)                   ==== true
        T ~ poller.readable(1)                   ==== false
        T ~ a.recvMsg(buf).count                 ==== 5L
        T ~ new String(buf.selectToArray(0L, 5L)) ==== "hello"
        T ~ a.fdCount                            ==== 0
        T ~ a.trunc                              ==== false
        T ~ a.recvMsg(buf).count                 ==== 2L
        T ~ new String(buf.selectToArray(0L, 2L)) ==== "he"
        T ~ a.recvMsg(buf, 3L).wouldBlock        ==== true

        // a datagram longer than the buffer offered is reported as truncated, not silently cut
        T ~ b.sendOnce(msg).count                ==== 5L
        T ~ a.recvMsg(buf, 3L).count             ==== 3L
        T ~ a.trunc                              ==== true

        // a descriptor rides along and is counted, not hidden; the copy is live, ours, and close-on-exec
        // (Linux asks for that with MSG_CMSG_CLOEXEC; Darwin has no such flag and gets an fcntl right after)
        val (c, d) = must(FdSock.Raw.pair(PosixSocket.SOCK_STREAM))
        T ~ b.sendOnceWithFd(msg, 1L, c.descriptor).count ==== 1L
        T ~ a.recvMsg(buf).count                 ==== 1L
        T ~ a.fdCount                            ==== 1
        T ~ a.ctrunc                             ==== false
        val got = a.takeFd()
        T ~ (got >= 0)                           ==== true
        T ~ isCloexec(got)                       ==== true
        T ~ a.takeFd()                           ==== -1
        val g = must(FdSock.Raw.adopt(got))
        T ~ d.nonblocking().isIs                 ==== true
        T ~ g.sendOnce(msg).count                ==== 5L
        T ~ d.recvMsg(buf).count                 ==== 5L
        T ~ new String(buf.selectToArray(0L, 5L)) ==== "hello"

        // several descriptors in one message are all counted; one more than the slots is truncation, never
        // a quiet trim (CMSG_SPACE padding would otherwise admit a stray one at odd slot counts)
        val (m3, n3) = must(FdSock.Raw.pair(packet, 3))
        val (u1, u2) = must(FdSock.Raw.pair(PosixSocket.SOCK_STREAM))
        T ~ n3.sendOnceWithFds(msg, 1L, Array(c.descriptor, u1.descriptor, u2.descriptor)).count ==== 1L
        T ~ m3.recvMsg(buf).count                ==== 1L
        T ~ m3.fdCount                           ==== 3
        T ~ m3.ctrunc                            ==== false
        val t1 = m3.takeFd(); val t2 = m3.takeFd(); val t3 = m3.takeFd()
        T ~ (t1 >= 0 && t2 >= 0 && t3 >= 0)      ==== true
        T ~ m3.takeFd()                          ==== -1
        PosixSocket.closeQuietly(t1); PosixSocket.closeQuietly(t2); PosixSocket.closeQuietly(t3)
        T ~ n3.sendOnceWithFds(msg, 1L, Array(c.descriptor, u1.descriptor, u2.descriptor, u1.descriptor)).errno ==== PosixSocket.Errno.EINVAL
        T ~ n3.sendOnceWithFds(msg, 1L, Array.empty[Int]).errno ==== PosixSocket.Errno.EINVAL
        m3.close(); n3.close()
        val (o1, o2) = must(FdSock.Raw.pair(packet, 2))
        val one = must(FdSock.Raw.adopt(o1.disown(), 1))
        T ~ o2.sendOnceWithFds(msg, 1L, Array(u1.descriptor, u2.descriptor)).count ==== 1L
        T ~ one.recvMsg(buf).count               ==== 1L
        T ~ one.fdCount                          ==== 1
        T ~ one.ctrunc                           ==== true
        one.close(); o2.close(); u1.close(); u2.close()

        // more descriptors than slots: truncation is a reported fact and the extra is closed
        val (e0, e1) = must(FdSock.Raw.pair(packet, 0))
        T ~ e1.sendOnceWithFd(msg, 1L, c.descriptor).count ==== 1L
        T ~ e0.recvMsg(buf).count                ==== 1L
        T ~ e0.ctrunc                            ==== true
        T ~ e0.fdCount                           ==== 0
        e0.close()
        e1.close()
        T ~ e0.recvMsg(buf).errno                ==== PosixSocket.Errno.EBADF

        // the platform's per-message limit is what the table says: exactly ScmMaxFd descriptors arrive in
        // one message, uncut, and one more is refused at the send, so nothing is installed anywhere
        val (h1, h2) = must(FdSock.Raw.pair(packet, PosixSocket.ScmMaxFd + 1))
        T ~ h1.nonblocking().isIs                ==== true
        T ~ h2.sendOnceWithFds(msg, 1L, Array.fill(PosixSocket.ScmMaxFd)(c.descriptor)).count ==== 1L
        T ~ h1.recvMsg(buf).count                ==== 1L
        T ~ h1.fdCount                           ==== PosixSocket.ScmMaxFd
        T ~ h1.ctrunc                            ==== false
        h1.closeFds()
        T ~ h2.sendOnceWithFds(msg, 1L, Array.fill(PosixSocket.ScmMaxFd + 1)(c.descriptor)).failed ==== true
        T ~ h1.recvMsg(buf).wouldBlock           ==== true
        h1.close(); h2.close()

        // the peer goes away: a send answers EPIPE as data, never a signal.  On Darwin MSG_NOSIGNAL is 0 and
        // Raw sets no SO_NOSIGPIPE, so this is also the proof that the JVM's SIGPIPE handler keeps it alive.
        c.close()
        d.close()
        val gone = g.sendOnce(msg)
        T ~ gone.peerGone                        ==== true
        T ~ gone.name                            ==== "EPIPE"
        g.close()
        T ~ g.isClosed                           ==== true

        // the descriptor moves between tiers: a Conn's can be taken and driven raw, and a Raw's adopted as a Conn
        val (p, q) = must(FdSock.Raw.pair(PosixSocket.SOCK_STREAM))
        val conn = must(FdSock.adopt(p.disown()))
        T ~ (conn.descriptor >= 0)               ==== true
        T ~ conn.write("via conn".bytes).isIs    ==== true
        T ~ q.nonblocking().isIs                 ==== true
        T ~ q.recvMsg(buf).count                 ==== 8L
        T ~ new String(buf.selectToArray(0L, 8L)) ==== "via conn"
        val back = must(FdSock.Raw.adopt(conn.disown()))
        T ~ conn.descriptor                      ==== -1
        T ~ conn.write("x".bytes).isIs           ==== false
        T ~ back.nonblocking().isIs              ==== true
        T ~ q.sendOnce(msg).count                ==== 5L
        T ~ back.recvMsg(buf).count              ==== 5L
        val qc = must(FdSock.adopt(q.disown()))
        T ~ q.descriptor                         ==== -1
        T ~ qc.write("hello".bytes).isIs         ==== true
        T ~ back.recvMsg(buf).count              ==== 5L
        back.close()
        qc.close()

        // shutdown wakes a peer's poll with end of stream (a stream pair: Darwin's shutdown reaches the
        // peer only for streams, so a datagram peer there would sleep on)
        val (s1, s2) = must(FdSock.Raw.pair(PosixSocket.SOCK_STREAM))
        T ~ s1.nonblocking().isIs                ==== true
        poller.set(0, s1.descriptor)
        poller.set(1, s2.descriptor)
        T ~ poller.poll(50).count                ==== 0L
        T ~ s2.shutdown().isIs                   ==== true
        T ~ poller.poll(1000).count              ==== 2L
        T ~ s1.recvMsg(buf).count                ==== 0L
        s1.close(); s2.close()
      finally
        poller.close()
        a.close()
        b.close()

      // errno values are pinned against the kernel, not against the table: each code below is a real answer
      locally:
        val dir = socketDir("kse-errno-")
        val stale = dir.resolve("stale.sock")
        val srv = must(FdSock.listen(stale, 1.s))
        PosixSocket.closeQuietly(srv.disown())                   // the path stays; nobody listens there now
        T ~ stale.exists                                     ==== true
        T ~ errnoOf(FdSock.connect(stale, 1.s))              ==== PosixSocket.Errno.ECONNREFUSED
        Files.deleteIfExists(stale) __ Unit
        Files.deleteIfExists(dir) __ Unit
        val lone = must(FdSock.Raw.adopt(loneSocket(PosixSocket.SOCK_STREAM)))
        val one = Mem.alloc[Byte](1L)
        T ~ lone.sendOnce(one).errno                         ==== PosixSocket.Errno.ENOTCONN
        T ~ lone.sendOnce(one).peerGone                      ==== true
        lone.close()
        val (d1, d2) = must(FdSock.Raw.pair(PosixSocket.SOCK_DGRAM))
        val huge = Mem.alloc[Byte](1L << 22)
        T ~ d2.sendOnce(huge).errno                          ==== PosixSocket.Errno.EMSGSIZE
        // SO_NOSIGPIPE is Darwin's; Linux has no such option (MSG_NOSIGNAL does that job there)
        T ~ errnoOf(d1.setOption(PosixSocket.SO_NOSIGPIPE, 1)) ==== (if PosixSocket.mac then -1 else PosixSocket.Errno.ENOPROTOOPT)
        d1.close(); d2.close()

      // a control message cut short (Darwin keeps cmsg_len past the copied bytes): the descriptors that fit are still visited
      locally:
        val arena = Arena.ofConfined()
        try
          val ctrl = arena.allocate(PosixSocket.CMsg.space(12L))
          val hdr = PosixSocket.CMsg.hdr
          if PosixSocket.mac then ctrl.set(JAVA_INT, 0L, (hdr + 12L).toInt)
          else ctrl.set(JAVA_LONG, 0L, hdr + 12L)
          ctrl.set(JAVA_INT, PosixSocket.CMsg.levelOff, PosixSocket.SOL_SOCKET)
          ctrl.set(JAVA_INT, PosixSocket.CMsg.typeOff, PosixSocket.SCM_RIGHTS)
          ctrl.set(JAVA_INT, hdr, 41)
          ctrl.set(JAVA_INT, hdr + 4, 42)
          ctrl.set(JAVA_INT, hdr + 8, 43)
          val seen = collection.mutable.ArrayBuffer.empty[Int]
          PosixSocket.CMsg.forEachFd(ctrl, hdr + 12L)(seen += _)
          T ~ seen.toList                        ==== List(41, 42, 43)
          seen.clear()
          PosixSocket.CMsg.forEachFd(ctrl, hdr + 8L)(seen += _)    // declared 12 bytes of descriptors, only 8 copied
          T ~ seen.toList                        ==== List(41, 42)
          seen.clear()
          PosixSocket.CMsg.forEachFd(ctrl, hdr + 2L)(seen += _)    // not even one whole descriptor
          T ~ seen.toList                        ==== Nil
        finally arena.close()

      // a blocked raw receive is released by shutdown from another thread, after which close is safe
      locally:
        val (x, y) = must(FdSock.Raw.pair(PosixSocket.SOCK_STREAM))
        val buf2 = Mem.alloc[Byte](16L)
        val waiter = Fu:
          x.recvMsg(buf2)                          // blocking: nothing arrives
        Thread.sleep(100)
        T ~ x.shutdown().isIs                    ==== true
        T ~ waiter.await().fold(_.count)(_ => -9L) ==== 0L
        x.close()
        y.close()
        T ~ x.isClosed                           ==== true
        T ~ x.recvMsg(buf2).errno                ==== PosixSocket.Errno.EBADF

      // an idle server's accept times out even where accept ignores SO_RCVTIMEO (Darwin): it is a poll
      locally:
        val dir = socketDir("kse-accept-")
        val srv = must(FdSock.listen(dir.resolve("idle.sock"), 200.ms))
        try
          T ~ isCloexec(srv.descriptor)          ==== true
          val t0 = System.nanoTime
          val r = srv.accept()
          val dt = (System.nanoTime - t0) / 1000000L
          T ~ errnoOf(r)                         ==== PosixSocket.Errno.EAGAIN
          T ~ (dt >= 150L && dt < 5000L)         ==== true
        finally
          srv.close()
          Files.deleteIfExists(dir) __ Unit

      // the tiniest timeout still accepts a queued connection and still reads queued bytes; an empty one still times out
      locally:
        val dir = socketDir("kse-tiny-")
        val path = dir.resolve("tiny.sock")
        val srv = must(FdSock.listen(path, 1.ms))
        try
          T ~ errnoOf(srv.accept())              ==== PosixSocket.Errno.EAGAIN
          val client = must(FdSock.connect(path, 1.ms))
          try
            val server = must(srv.accept())
            try
              T ~ isCloexec(server.descriptor)     ==== true
              T ~ isCloexec(client.descriptor)     ==== true
              T ~ client.write("queued".bytes).isIs  ==== true
              val buf = new Array[Byte](16)
              T ~ server.read(buf).fold(n => new String(buf, 0, n))(_ => "?") ==== "queued"
              T ~ errnoOf(server.read(buf))        ==== PosixSocket.Errno.EAGAIN
              T ~ server.setTimeout(150.ms).isIs   ==== true
              val t0 = System.nanoTime
              T ~ errnoOf(server.read(buf))        ==== PosixSocket.Errno.EAGAIN
              T ~ ((System.nanoTime - t0) / 1000000L >= 100L) ==== true
              T ~ client.write("more".bytes).isIs    ==== true
              T ~ server.recvMsgOrFd(buf).fold(r => r.count)(_ => -1) ==== 4
            finally server.close()
          finally client.close()
        finally
          srv.close()
          Files.deleteIfExists(dir) __ Unit


  @Test
  def fdSharedMemoryTest(): Unit =
    if FdSock.supported then
      val dir = socketDir("kse-fdshm-")
      val ready = new SynchronousQueue[AnyRef]()
      val done  = new SynchronousQueue[AnyRef]()

      // the blocking tier: bytes both ways, then a descriptor, which arrives close-on-exec
      val rawSock = dir.resolve("raw.sock")
      val rawServer = Fu:
        Resource.nice(FdSock.listen(rawSock, 5.s))(_.close()){ srv =>
          ready.put("go")
          val conn = srv.accept().?
          try
            val buf = new Array[Byte](8)
            val n = conn.read(buf).?
            conn.write("pong".bytes).?
            conn.sendFd(conn.descriptor, "dup".bytes).?
            new String(buf, 0, n)
          finally conn.close()
        }.?
      val rawClient = Fu:
        handoff(ready) __ Unit
        Resource.nice(FdSock.connect(rawSock, 5.s))(_.close()){ conn =>
          conn.write("ping".bytes).?
          val buf = new Array[Byte](4)               // byte-exact: the descriptor rides the send after this one
          val n = conn.read(buf).?
          val r = conn.recvFd(new Array[Byte](3)).?
          val cloexec = isCloexec(r.fd)
          PosixSocket.closeQuietly(r.fd)
          (new String(buf, 0, n), r.count, cloexec)
        }.?
      T ~ rawServer.await() ==== "ping"
      T ~ rawClient.await() ==== (("pong", 3, true))

      // offerFd/acceptFd: anonymous region handed across threads via SCM_RIGHTS, with write-back
      val sock = dir.resolve("offer.sock")
      val offerer = Fu:
        Resource.nice(SharedMemory.offerFd[Long](sock, 4, 5.s))(_.close()){ later =>
          later.use(_.use(_.set()(i => (i + 1) * 11)))   // 11, 22, 33, 44
          ready.put("go")
          later.op(_.serveOne()).?
          handoff(done) __ Unit                          // acceptor has written back
          later.op(_.op(_(2)))
        }.?
      val acceptor = Fu:
        handoff(ready) __ Unit
        Resource.nice(SharedMemory.acceptFd[Long](sock, timeout = 5.s))(_.close()){ o =>
          val v = (o.op(_.length), o.op(_(0)), o.op(_(3)))
          o.use(m => m(2) = 99L)
          done.put("ok")
          v
        }.?
      T ~ acceptor.await() ==== ((4L, 11L, 44L))
      T ~ offerer.await() ==== 99L

      // a read-only view of a received region refuses a write as an exception, not a fault
      val roSock = dir.resolve("ro.sock")
      val roOfferer = Fu:
        Resource.nice(SharedMemory.offerFd[Long](roSock, 2, 5.s))(_.close()){ later =>
          later.use(_.use(m => m(1) = 7L))
          ready.put("go")
          later.op(_.serveOne()).?
          handoff(done) __ Unit
          later.op(_.op(_(1)))
        }.?
      val roAcceptor = Fu:
        handoff(ready) __ Unit
        Resource.nice(SharedMemory.acceptFd[Long](roSock, readOnly = true, timeout = 5.s))(_.close()){ o =>
          val v = o.op(_(1))
          val refused = try { o.use(m => m(1) = 8L); false } catch { case _: IllegalArgumentException => true }
          done.put("ok")
          (v, refused)
        }.?
      T ~ roAcceptor.await() ==== ((7L, true))
      T ~ roOfferer.await() ==== 7L

      // Failure paths answer errors, not hangs
      T ~ FdSock.connect(dir.resolve("nobody.sock"), 1.s).isAlt ==== true
      T ~ Resource.nice(FdSock.listen(dir.resolve("lonely.sock"), 250.ms))(_.close()){ srv => srv.accept().isAlt } ==== Is(true)

      // Honest interop: python speaks SCM_RIGHTS natively.  The region is a memfd where there is one
      // (Linux) and a POSIX shared-memory object otherwise (macOS, through multiprocessing.shared_memory).
      // Every Linux and macOS runner has a python 3.9+, so the interop must not quietly skip.
      T ~ pythonFds.isDefined ==== true
      pythonFds.foreach{ py =>
        // python offers a region with the kse header; we accept, read, and write back where python can see it
        val hostSock = dir.resolve("pyhost.sock")
        val host = startPython(py, dir, "pyhost.py",
          """import socket, os, mmap, struct, sys, time
            |srv = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            |srv.bind(sys.argv[1])
            |srv.listen(1)
            |size = 32
            |shm = None
            |if hasattr(os, 'memfd_create'):
            |    fd = os.memfd_create('py-host')
            |    os.ftruncate(fd, size)
            |    m = mmap.mmap(fd, size)
            |else:
            |    from multiprocessing import shared_memory
            |    shm = shared_memory.SharedMemory(create=True, size=size)
            |    fd = shm._fd
            |    m = shm.buf
            |m[0:32] = struct.pack('<4q', 5, 6, 7, 8)
            |conn, _ = srv.accept()
            |tag = b'kseM' + bytes([1, 0, 0, 0]) + struct.pack('<q', size)
            |socket.send_fds(conn, [tag], [fd])
            |deadline = time.time() + 10
            |while time.time() < deadline and struct.unpack('<q', m[24:32])[0] != 55:
            |    time.sleep(0.01)
            |print(struct.unpack('<q', m[24:32])[0])
            |conn.close()
            |srv.close()
            |if shm is not None:
            |    del m
            |    shm.close()
            |    shm.unlink()
            |""".stripMargin, hostSock.toString)
        var w = 0
        while !Files.exists(hostSock) && w < 200 do { Thread.sleep(25); w += 1 }
        T ~ Files.exists(hostSock) ==== true
        T ~ Resource.nice(SharedMemory.acceptFd[Long](hostSock, timeout = 5.s))(_.close()){ o =>
          val v = (o.op(_.length), o.op(_(0)), o.op(_(3)))
          o.use(m => m(3) = 55L)
          v
        } ==== Is((4L, 5L, 8L))
        T ~ finish(host) ==== "55"

        // we offer; a python client receives the descriptor, maps it, reads, and writes back
        val downSock = dir.resolve("pydown.sock")
        T ~ Resource.nice(SharedMemory.offerFd[Long](downSock, 3, 5.s))(_.close()){ later =>
          later.use(_.use(m => m(0) = 123L))
          val cl = startPython(py, dir, "pyclient.py",
            """import socket, mmap, struct, sys
              |c = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
              |c.connect(sys.argv[1])
              |msg, fds, flags, addr = socket.recv_fds(c, 32, 4)
              |size = struct.unpack('<q', msg[8:16])[0]
              |m = mmap.mmap(fds[0], size)
              |print(struct.unpack('<q', m[0:8])[0], size)
              |m[8:16] = struct.pack('<q', 777)
              |c.close()
              |""".stripMargin, downSock.toString)
          T ~ later.op[Ask[Unit]](_.serveOne()).isIs ==== true
          T ~ finish(cl) ==== "123 24"
          later.op(_.op(_(1)))
        } ==== Is(777L)

        // adopt + recvMsgOrFd, doorbell-style: python hands us one end of a packet socketpair, then
        // interleaves a plain record with a region-bearing grant on it; the received end is close-on-exec
        val dbSock = dir.resolve("pydoor.sock")
        val db = startPython(py, dir, "pydoor.py",
          """import socket, os, mmap, struct, sys
            |srv = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            |srv.bind(sys.argv[1])
            |srv.listen(1)
            |conn, _ = srv.accept()
            |try:
            |    a, b = socket.socketpair(socket.AF_UNIX, socket.SOCK_SEQPACKET)
            |except (OSError, AttributeError):
            |    a, b = socket.socketpair(socket.AF_UNIX, socket.SOCK_DGRAM)
            |socket.send_fds(conn, [b'sock'], [b.fileno()])
            |b.close()
            |a.send(b'event-no-fd')
            |shm = None
            |if hasattr(os, 'memfd_create'):
            |    fd = os.memfd_create('grant')
            |    os.ftruncate(fd, 16)
            |    m = mmap.mmap(fd, 16)
            |else:
            |    from multiprocessing import shared_memory
            |    shm = shared_memory.SharedMemory(create=True, size=16)
            |    fd = shm._fd
            |    m = shm.buf
            |m[0:16] = struct.pack('<2q', 41, 42)
            |socket.send_fds(a, [b'grant'], [fd])
            |print(a.recv(16).decode())
            |a.close()
            |conn.close()
            |srv.close()
            |if shm is not None:
            |    del m
            |    shm.close()
            |    shm.unlink()
            |""".stripMargin, dbSock.toString)
        w = 0
        while !Files.exists(dbSock) && w < 200 do { Thread.sleep(25); w += 1 }
        val doorbell = Fu:
          Resource.nice(FdSock.connect(dbSock, 5.s))(_.close()){ c =>
            val r = c.recvFd(new Array[Byte](8)).?
            val cloexec = isCloexec(r.fd)
            val door = FdSock.adopt(r.fd).?
            try
              door.setTimeout(5.s).?
              val buf = new Array[Byte](64)
              val e1 = door.recvMsgOrFd(buf).?
              val s1 = new String(buf, 0, e1.count)
              val e2 = door.recvMsgOrFd(buf).?
              val s2 = new String(buf, 0, e2.count)
              val n = if PosixSocket.mac then 2L else 0L   // a Darwin shm descriptor cannot be measured by lseek
              val vals = Resource.nice(SharedMemory.attachFd[Long](e2.fd.getOrElse(-1), n))(_.close()){ o =>
                (o.op(_.length), o.op(_(0)), o.op(_(1)))
              }.?
              door.write("ok".bytes).?
              (cloexec, e1.fd.isEmpty, s1, e2.fd.isDefined, s2, vals)
            finally door.close()
          }.?
        T ~ doorbell.await() ==== ((true, true, "event-no-fd", true, "grant", (2L, 41L, 42L)))
        T ~ finish(db) ==== "ok"
      }

      // a received descriptor is measured when no count is given: exact on Linux (lseek on a memfd), whole
      // pages on macOS (fstat on a shm object, which does not seek)
      locally:
        val (s1, s2) = must(FdSock.Raw.pair(PosixSocket.SOCK_STREAM))
        val one = Mem.alloc[Byte](1L)
        T ~ Resource.nice(SharedMemory.createFd[Long](3))(_.close()){ anon =>
          anon.use(_.use(m => m(2) = 9L))
          T ~ s1.sendOnceWithFd(one, 1L, anon.op(_.fd)).count ==== 1L
          T ~ s2.recvMsg(one).count ==== 1L
          val got = s2.takeFd()
          must(Resource.nice(SharedMemory.attachFd[Long](got))(_.close()){ o =>
            val n = o.op(_.length)
            (n == 3L || (n > 3L && (n * 8) % 4096 == 0), o.op(_(2)))
          })
        } ==== Is((true, 9L))
        s1.close(); s2.close()

      T ~ FdSock.adopt(-1).isAlt  ==== true
      T ~ FdSock.adopt(999_999).isAlt ==== true   // fd numbers allocate lowest-first; this one cannot be open

      List("raw.sock", "offer.sock", "ro.sock", "lonely.sock", "pyhost.sock", "pydown.sock", "pydoor.sock", "pyhost.py", "pyclient.py", "pydoor.py")
        .foreach(f => Files.deleteIfExists(dir.resolve(f)) __ Unit)
      Files.deleteIfExists(dir) __ Unit


  @Test
  def namedSharedMemoryTest(): Unit =
    // the OS-named route exists everywhere: a tmpfs file on Linux, shm_open on macOS, a pagefile-backed
    // section on Windows; the name is random and the round trip is the same on all three
    var made: String = null
    T ~ Resource.nice(SharedMemory.createNamed[Long](4))(_.close()){ later =>
      made = later.op(_.name)
      T ~ made.startsWith("/kse-") ==== true
      later.use(_.use(_.set()(i => (i + 1) * 10)))    // 10, 20, 30, 40
      // a second view sees the writes, and its own writes come back through the first
      T ~ Resource.nice(SharedMemory.attach[Long](made, 4))(_.close()){ o =>
        T ~ o.op(_.length) ==== 4L
        T ~ o.op(_(3))     ==== 40L
        o.use(m => m(1) = 21L)
        o.op(_(0))
      } ==== 10L
      T ~ later.op(_.op(_(1))) ==== 21L
      // a read-only view reads, and refuses a write as an exception rather than a fault
      T ~ Resource.nice(SharedMemory.attach[Long](made, 4, readOnly = true))(_.close()){ o =>
        T ~ o.op(_(2))             ==== 30L
        T ~ o.use(m => m(2) = 0L)  ==== thrown[IllegalArgumentException]
        o.op(_(2))
      } ==== 30L
      // macOS: shm_open's mode is variadic (on the stack on arm64); the object must really have come out 0600
      if PosixSocket.mac then T ~ darwinShmMode(made) ==== 0x180
      later.op(_.op(_(3)))
    } ==== 40L
    // closing the creator destroys the object: the name no longer attaches, and nothing lingers
    T ~ Resource.nice(SharedMemory.attach[Long](made, 4))(_.close())(_ => 0).isAlt ==== true
    if PosixSocket.mac then T ~ darwinShmMode(made) ==== -1
    if SharedMemory.ramDirectory.isDefined then T ~ SharedMemory.posixShmDir.resolve(made.stripPrefix("/")).exists ==== false
    // a name that never existed, or a size that makes no sense, is an error, not a throw
    T ~ Resource.nice(SharedMemory.attach[Long]("/kse-never-made-this", 1))(_.close())(_ => 0).isAlt ==== true
    T ~ Resource.nice(SharedMemory.attach[Long]("/kse-never-made-this", 0))(_.close())(_ => 0).isAlt ==== true
    T ~ Resource.nice(SharedMemory.attach[Long](made, -1))(_.close())(_ => 0).isAlt                  ==== true
    T ~ SharedMemory.createNamed[Long](0).isAlt                                                      ==== true

    // a name of the caller's choosing, and a size the object itself reports: the receiving side of a
    // protocol, where a peer says "attach to X" and nothing more
    val pid = ProcessHandle.current().pid()
    val chosen = s"/kse-chosen-$pid"
    T ~ Resource.nice(SharedMemory.createNamed[Long](chosen, 4))(_.close()){ later =>
      T ~ later.op(_.name) ==== chosen
      later.use(_.use(_.set()(i => i + 1)))    // 1, 2, 3, 4
      T ~ Resource.nice(SharedMemory.attach[Long](chosen, 0))(_.close()){ o =>
        val n = o.op(_.length)
        T ~ (n == 4L || (n > 4L && (n * 8) % 4096 == 0)) ==== true   // exact on Linux; whole pages elsewhere
        o.op(_(3))
      } ==== 4L
      T ~ SharedMemory.createNamed[Long](chosen, 4).isAlt ==== true   // the name is taken: an error, not a retry
      later.op(_.op(_(0)))
    } ==== 1L
    T ~ Resource.nice(SharedMemory.createNamed[Long](chosen, 2))(_.close()){ _.op(_.name) } ==== Is(chosen)   // free again once closed
    T ~ SharedMemory.createNamed[Long]("", 1).isAlt ==== true
    if PosixSocket.supported then T ~ SharedMemory.createNamed[Long]("/a/b", 1).isAlt ==== true

    // a foreign creator: python makes a named object and we attach by name alone; then the reverse.  The
    // same script runs on all three platforms; python supplies the POSIX slash itself.  Linux and macOS
    // always have a python that can do this, so there the interop must not quietly skip.
    if PosixSocket.supported then T ~ pythonShm.isDefined ==== true
    pythonShm.foreach{ py =>
      val dir = Files.createTempDirectory("kse-pyshm-")
      val pyName = s"kse-pyshm-$pid"
      val kseName = if windows then pyName else "/" + pyName
      val host = startPython(py, dir, "pyhost.py",
        """import struct, sys, time
          |from multiprocessing import shared_memory
          |shm = shared_memory.SharedMemory(name=sys.argv[1], create=True, size=32)
          |m = shm.buf
          |m[0:32] = struct.pack('<4q', 5, 6, 7, 8)
          |deadline = time.time() + 10
          |while time.time() < deadline and struct.unpack('<q', m[24:32])[0] != 55:
          |    time.sleep(0.01)
          |print(struct.unpack('<q', m[24:32])[0])
          |del m
          |shm.close()
          |shm.unlink()
          |""".stripMargin, pyName)
      T ~ Resource.nice(eventually(SharedMemory.attach[Long](kseName, 0)))(_.close()){ o =>
        val v = (o.op(_.length) >= 4L, o.op(_(0)), o.op(_(3)))
        o.use(m => m(3) = 55L)
        v
      } ==== Is((true, 5L, 8L))
      T ~ finish(host) ==== "55"

      T ~ Resource.nice(SharedMemory.createNamed[Long](kseName, 3))(_.close()){ later =>
        later.use(_.use(m => m(0) = 123L))
        val cl = startPython(py, dir, "pyclient.py",
          """import struct, sys, os
            |from multiprocessing import shared_memory
            |try:
            |    shm = shared_memory.SharedMemory(name=sys.argv[1], track=False)
            |except TypeError:
            |    shm = shared_memory.SharedMemory(name=sys.argv[1])
            |    if os.name != 'nt':
            |        from multiprocessing import resource_tracker
            |        resource_tracker.unregister(shm._name, 'shared_memory')
            |m = shm.buf
            |print(struct.unpack('<q', m[0:8])[0], shm.size >= 24)
            |m[8:16] = struct.pack('<q', 777)
            |del m
            |shm.close()
            |""".stripMargin, pyName)
        T ~ finish(cl) ==== "123 True"
        later.op(_.op(_(1)))
      } ==== Is(777L)
      List("pyhost.py", "pyclient.py").foreach(f => Files.deleteIfExists(dir.resolve(f)) __ Unit)
      Files.deleteIfExists(dir) __ Unit
    }
}
