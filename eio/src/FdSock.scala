// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.eio


import java.lang.foreign.{Arena, MemorySegment}
import java.lang.foreign.ValueLayout.JAVA_BYTE
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Files}
import java.time.Duration
import java.util.concurrent.atomic.AtomicBoolean

import kse.basics.{given, _}
import kse.flow.{given, _}
import kse.maths.{given, _}


/** Unix-domain stream sockets that can carry file descriptors (`SCM_RIGHTS`) alongside ordinary bytes,
  * on Linux and macOS.  This is the standard POSIX way to hand another process an open descriptor —
  * in particular an anonymous shared-memory descriptor (see `SharedMemory.createFd` / `attachFd`), so
  * that memory can be shared with no name in any namespace and kernel-refcounted lifetime.
  *
  * The JDK's own Unix-domain channels cannot carry ancillary data, so these sockets are opened and run
  * natively; the JVM needs `--enable-native-access=ALL-UNNAMED` (or the owning module).  Windows has no
  * `SCM_RIGHTS`; use `SharedMemory`'s named objects there.
  *
  * Two tiers.  The blocking tier: a `Server` listens at a filesystem path; `connect` reaches it; a
  * `Conn` moves bytes (`read`/`write`) and descriptors (`sendFd`/`recvFd`, or `recvMsgOrFd` for mixed
  * traffic).  Receives are bounded by a timeout (set at creation, adjustable with `setTimeout`) so
  * nothing waits forever by default.  Calls block in native code, so on a virtual thread they pin their
  * carrier — use a platform thread (or accept the pinning) for long waits.  Connections are not
  * thread-safe; use each from one thread at a time.  A failed system call is an `Err` carrying a
  * [[PosixSocket.Failed]], so the `errno` is data ([[PosixSocket.errnoOf]]), not just a message.
  *
  * The raw tier, [[Raw]]: one system call per method on a descriptor of any socket type, non-blocking
  * by choice, readiness-polled with [[PosixSocket.Poller]], answering [[PosixSocket.Result]] (count or `-errno`)
  * without allocating, and reporting how many descriptors arrived and whether anything was truncated
  * rather than deciding for the caller.  Constants, layouts and downcalls are [[PosixSocket]]'s.
  *
  * The descriptor is never locked in.  Every handle exposes it (`descriptor`) for a call this library
  * does not make, and gives it up on request (`disown`): a `Conn` accepted here can become a `Raw`
  * (`Raw.adopt(conn.disown())`), a descriptor received or inherited can become either, and one taken from
  * a `Raw` can go to a `Conn` (`adopt`), to another process (`sendFd`), or to code that drives
  * [[PosixSocket.Sys]] itself.
  */
object FdSock {
  /** True if this platform supports descriptor-passing sockets (Linux and macOS do; Windows does not). */
  def supported: Boolean = PosixSocket.supported

  /** The receive timeout used when none is given. */
  val defaultTimeout: Duration = 30.s

  /** Timeouts are applied at whole-millisecond resolution, rounding up so a short timeout stays a timeout
    * (200.us waits 1 ms, not forever); a zero or negative timeout means wait without limit.
    */
  private[eio] def millisOf(timeout: Duration): Long =
    val ms = timeout.ceil.ms.toMillis
    if ms < 0 then 0L else ms

  private def checkSupported(): Ask[Unit] =
    if supported then Is.unit
    else Err.or(s"descriptor-passing sockets are unsupported on '${PosixSocket.platformName}'")

  private def newSocket(cap: MemorySegment): Ask[Int] = Ask:
    val fd: Int = PosixSocket.Sys.socket.invoke(cap, PosixSocket.AF_UNIX, PosixSocket.SOCK_STREAM, 0)
    if fd < 0 then PosixSocket.Failed("socket", PosixSocket.errnoOf(cap)).?
    PosixSocket.cloexec(cap, fd) __ Unit   // a failure here is harmless
    fd

  private def sockaddr(tmp: Arena, path: Path): Ask[MemorySegment] = Ask:
    val bytes = path.toString.getBytes(StandardCharsets.UTF_8)
    if bytes.length > PosixSocket.SockAddrUn.pathMax then
      Err ?# s"socket path too long (${bytes.length} > ${PosixSocket.SockAddrUn.pathMax} bytes): $path"
    val sa = tmp.allocate(PosixSocket.SockAddrUn.size)
    PosixSocket.SockAddrUn.write(sa, bytes)
    sa

  /** Waits until `fd` is readable or `timeoutMs` (> 0) has elapsed: a `poll` with a deadline, so an
    * interrupted wait resumes with what remains rather than starting over, and the remainder is rounded up
    * with at least one poll taken, so a tiny timeout still sees an event already queued.  A timeout is
    * `Failed(call, EAGAIN, "timed out …")`.
    */
  private def awaitReadable(cap: MemorySegment, tmp: Arena, fd: Int, timeoutMs: Long, call: String, where: String): Ask[Unit] = Ask:
    val pfd = tmp.allocate(PosixSocket.PollFd.size)
    PosixSocket.PollFd.set(pfd, 0, fd, PosixSocket.POLLIN)
    val deadline = System.nanoTime + timeoutMs * 1000000L
    var ready = -1L
    while ready < 0 do
      val leftNs = deadline - System.nanoTime
      val ms =
        if leftNs <= 0 then 0
        else
          val m = (leftNs + 999999L) / 1000000L
          if m > Int.MaxValue then Int.MaxValue else m.toInt
      val r = PosixSocket.poll(cap, pfd, 1, ms)
      if r.failed then
        if !r.interrupted then PosixSocket.Failed("poll", r.errno, where).?
      else ready = r.count
    if ready == 0 then PosixSocket.Failed(call, PosixSocket.Errno.EAGAIN, if where.isEmpty then "timed out" else s"timed out $where").?

  private def setRcvTimeout(cap: MemorySegment, tmp: Arena, fd: Int, millis: Long): Ask[Unit] = Ask:
    val tv = tmp.allocate(PosixSocket.TimeVal.size)
    PosixSocket.TimeVal.write(tv, millis)
    if (PosixSocket.Sys.setsockopt.invoke(cap, fd, PosixSocket.SOL_SOCKET, PosixSocket.SO_RCVTIMEO, tv, PosixSocket.TimeVal.size.toInt): Int) != 0 then
      PosixSocket.Failed("setsockopt", PosixSocket.errnoOf(cap), "SO_RCVTIMEO").?


  /** A connected Unix-domain stream socket: `read`/`write` for bytes, `sendFd`/`recvFd` for descriptors.
    * Receives wait with a deadline-driven `poll` (see `awaitReadable`) and keep `SO_RCVTIMEO` as a backstop.
    */
  final class Conn private[eio] (fd0: Int, timeout0: Long) extends AutoCloseable {
    private var fd = fd0
    private var timeoutMs = timeout0
    private def live: Ask[Int] = if fd >= 0 then Is(fd) else Err.or("socket is closed")

    /** The descriptor itself, for a call this class does not make; -1 once closed or disowned. */
    def descriptor: Int = fd

    /** Gives the descriptor to the caller (still open, blocking, with whatever timeout was set) and closes
      * this handle without closing it; -1 if already closed.  The caller owns it from here.
      */
    def disown(): Int =
      val f = fd
      fd = -1
      f

    /** Changes the receive timeout (bounds `read` and `recvFd`); zero or negative waits without limit. */
    def setTimeout(timeout: Duration): Ask[Unit] = Ask.flat:
      val f = live.?
      val tmp = Arena.ofConfined()
      try
        val ms = millisOf(timeout)
        setRcvTimeout(PosixSocket.capture(tmp), tmp, f, ms).peek(_ => timeoutMs = ms)
      finally tmp.close()

    /** Writes all of `data` (ordinary bytes, no descriptor). */
    def write(data: Array[Byte]): Ask[Unit] = Ask:
      val f = live.?
      if data.length > 0 then
        val tmp = Arena.ofConfined()
        try
          val cap = PosixSocket.capture(tmp)
          val seg = tmp.allocate(data.length.toLong)
          MemorySegment.copy(data, 0, seg, JAVA_BYTE, 0L, data.length)
          var off = 0L
          while off < data.length do
            val n: Long = PosixSocket.Sys.write.invoke(cap, f, seg.asSlice(off), data.length - off)
            if n < 0 then
              val e = PosixSocket.errnoOf(cap)
              if e != PosixSocket.Errno.EINTR then PosixSocket.Failed("write", e).?
            else off += n
        finally tmp.close()

    /** Reads up to `buf.length` bytes into `buf`, answering the count; 0 means the peer closed. */
    def read(buf: Array[Byte]): Ask[Int] = Ask:
      val f = live.?
      if buf.length == 0 then 0
      else
        val tmp = Arena.ofConfined()
        try
          val cap = PosixSocket.capture(tmp)
          if timeoutMs > 0 then awaitReadable(cap, tmp, f, timeoutMs, "read", "").?
          val seg = tmp.allocate(buf.length.toLong)
          var n = -1L
          while n < 0 do
            n = PosixSocket.Sys.read.invoke(cap, f, seg, buf.length.toLong)
            if n < 0 then
              val e = PosixSocket.errnoOf(cap)
              if e == PosixSocket.Errno.EAGAIN then PosixSocket.Failed("read", e, "timed out").?
              else if e != PosixSocket.Errno.EINTR then PosixSocket.Failed("read", e).?
          if n > 0 then MemorySegment.copy(seg, JAVA_BYTE, 0L, buf, 0, n.toInt)
          n.toInt
        finally tmp.close()

    /** Sends a file descriptor with `tag` bytes riding along (`SCM_RIGHTS` requires at least one byte;
      * the default is a single zero).  The descriptor itself is not consumed — close it when done with it.
      */
    def sendFd(passFd: Int, tag: Array[Byte] = Array[Byte](0)): Ask[Unit] = Ask:
      val f = live.?
      if tag.length == 0 then Err ?# "SCM_RIGHTS requires at least one data byte alongside the descriptor"
      val tmp = Arena.ofConfined()
      try
        val cap = PosixSocket.capture(tmp)
        val data = tmp.allocate(tag.length.toLong)
        MemorySegment.copy(tag, 0, data, JAVA_BYTE, 0L, tag.length)
        val iov = tmp.allocate(PosixSocket.IoVec.size)
        PosixSocket.IoVec.set(iov, data, tag.length.toLong)
        val ctrl = tmp.allocate(PosixSocket.CMsg.space(4L))
        val clen = PosixSocket.CMsg.writeFd(ctrl, passFd)
        val mh = tmp.allocate(PosixSocket.MsgHdr.size)
        PosixSocket.MsgHdr.init(mh, iov, ctrl, clen)
        var sent = -1L
        while sent < 0 do
          sent = PosixSocket.Sys.sendmsg.invoke(cap, f, mh, 0)
          if sent < 0 then
            val e = PosixSocket.errnoOf(cap)
            if e != PosixSocket.Errno.EINTR then PosixSocket.Failed("sendmsg", e).?
        if sent < tag.length then write(java.util.Arrays.copyOfRange(tag, sent.toInt, tag.length)).?
      finally tmp.close()

    /** Receives one message: up to `buf.length` bytes plus, possibly, a file descriptor.  `fd` is `None`
      * when the message carried no descriptor, and `count` is 0 when the peer closed.  An `Err` is an
      * actual failure: timeout, truncation, a closed socket.
      *
      * This is the one receive to use when a peer interleaves different message kinds on a single socket
      * (e.g. a doorbell mixing shared-memory grants with plain event records): give the socket one
      * listener loop, call this for every message, and dispatch on the message's own discriminator.  The
      * bytes in `buf` arrive exactly as the peer sent them — a descriptor travels as kernel-side control
      * data, never woven into the bytes — so the record needs no unwrapping and `fd` says whether one came
      * along.  Do not mix plain `read` into such a loop: a descriptor attached to a message that `read`
      * consumes is closed by the kernel, unrecoverably.
      *
      * One receive is one message only on a message-preserving socket (`SOCK_SEQPACKET` or `SOCK_DGRAM`:
      * an adopted descriptor, or a [[Raw.pair]]), where an oversized datagram is an `Err` (truncation),
      * never a partial delivery, and `buf` should be sized for the protocol's largest message.  A `Conn`
      * from `listen`/`connect` is a stream: one receive may hand back several sends, and a descriptor is
      * delivered with whichever receive consumes the first byte of the send that carried it — so bytes
      * from an earlier, descriptor-less send can arrive in the same receive as the descriptor.  Framing
      * alone does not settle the association; the receive boundaries must: read each record with
      * byte-exact lengths (a fixed header, then exactly the body it announces), never past a record's
      * end, and then `fd` belongs to the record being read.
      *
      * Should a peer send several descriptors, the first is kept and the rest are closed; the control
      * buffer holds [[PosixSocket.ScmMaxFd]] of them, so truncation means a malformed peer.  On Darwin a
      * truncated message has nonetheless installed every descriptor it carried, and those beyond the buffer
      * cannot be found to close: treat that connection as compromised.  A received descriptor is
      * close-on-exec and ours to close.  (The raw tier keeps them all: see [[Raw.recvMsg]].)
      */
    def recvMsgOrFd(buf: Array[Byte]): Ask[(fd: Option[Int], count: Int)] = Ask:
      val f = live.?
      if buf.length == 0 then Err ?# "receive buffer must hold at least one byte"
      Resource.assemble:
        val tmp = Resource.whileAssembling(Arena.ofConfined())(_.close())
        val cap = PosixSocket.capture(tmp)
        if timeoutMs > 0 then awaitReadable(cap, tmp, f, timeoutMs, "recvmsg", "").?
        val data = tmp.allocate(buf.length.toLong)
        val iov = tmp.allocate(PosixSocket.IoVec.size)
        PosixSocket.IoVec.set(iov, data, buf.length.toLong)
        val ctrlCap = PosixSocket.CMsg.space(4L * PosixSocket.ScmMaxFd)   // the most one message can carry, so it cannot truncate
        val ctrl = tmp.allocate(ctrlCap)
        val mh = tmp.allocate(PosixSocket.MsgHdr.size)
        PosixSocket.MsgHdr.init(mh, iov, ctrl, ctrlCap)
        var n = -1L
        while n < 0 do
          n = PosixSocket.Sys.recvmsg.invoke(cap, f, mh, PosixSocket.MSG_CMSG_CLOEXEC)
          if n < 0 then
            val e = PosixSocket.errnoOf(cap)
            if e == PosixSocket.Errno.EAGAIN then PosixSocket.Failed("recvmsg", e, "timed out with no descriptor").?
            else if e != PosixSocket.Errno.EINTR then PosixSocket.Failed("recvmsg", e).?
        var first = -1
        PosixSocket.CMsg.forEachFd(ctrl, PosixSocket.MsgHdr.controlLength(mh)){ f2 => if first < 0 then first = f2 else PosixSocket.closeQuietly(f2) }
        val got = guarded(first)(PosixSocket.closeQuietly)   // -1 closes nothing; a failure from here on discards the descriptor
        val flags = PosixSocket.MsgHdr.flagsOf(mh)
        if (flags & PosixSocket.MSG_CTRUNC) != 0 then Err ?# "control data truncated; descriptor discarded"
        if (flags & PosixSocket.MSG_TRUNC) != 0 then Err ?# s"message truncated (buffer holds only ${buf.length} bytes); descriptor discarded"
        if n == 0 then
          PosixSocket.closeQuietly(got)   // no bytes means the peer is gone; a stray descriptor is not a grant
          got.mapGuarded[(fd: Option[Int], count: Int)](_ => (fd = None, count = 0))
        else
          if got >= 0 && PosixSocket.mac then PosixSocket.cloexec(cap, got) __ Unit   // Linux already got it via MSG_CMSG_CLOEXEC
          MemorySegment.copy(data, JAVA_BYTE, 0L, buf, 0, n.toInt)
          got.mapGuarded(g => (fd = if g >= 0 then Some(g) else None, count = n.toInt))

    /** Receives a file descriptor plus its accompanying bytes into `buf` (which must hold at least one),
      * answering the descriptor and the byte count; a message without a descriptor is an error here — use
      * [[recvMsgOrFd]] when descriptor-less records are expected traffic on the same socket.
      */
    def recvFd(buf: Array[Byte] = new Array[Byte](1)): Ask[(fd: Int, count: Int)] =
      recvMsgOrFd(buf).flatMap: r =>
        r.fd match
          case Some(f) => Is((fd = f, count = r.count))
          case _ =>
            if r.count == 0 then Err.or("connection closed before a descriptor arrived")
            else Err.or(s"no file descriptor accompanied the data (${r.count} bytes received)")

    def close(): Unit =
      if fd >= 0 then
        PosixSocket.closeQuietly(fd)
        fd = -1
  }


  /** A listening Unix-domain socket bound at `path`; `accept()` yields one connection per client.
    * `close` stops listening and unlinks `path`.
    */
  final class Server private[eio] (fd0: Int, val path: Path, timeoutMs: Long) extends AutoCloseable {
    private var fd = fd0
    private def live: Ask[Int] = if fd >= 0 then Is(fd) else Err.or("server socket is closed")

    /** The listening descriptor itself, for a call this class does not make (`poll` it, say); -1 once closed or disowned. */
    def descriptor: Int = fd

    /** Gives the listening descriptor to the caller and closes this handle without closing it or unlinking
      * `path` (both are the caller's from here); -1 if already closed.
      */
    def disown(): Int =
      val f = fd
      fd = -1
      f

    /** Accepts one connection, waiting at most the server's timeout; the connection inherits the timeout.
      * The wait is a `poll` on the listening descriptor (Darwin's `accept` ignores `SO_RCVTIMEO`), so a
      * timeout is reported as `EAGAIN` on every platform.
      */
    def accept(): Ask[Conn] = Ask:
      val f = live.?
      Resource.assemble:
        val tmp = Resource.whileAssembling(Arena.ofConfined())(_.close())
        val cap = PosixSocket.capture(tmp)
        if timeoutMs > 0 then awaitReadable(cap, tmp, f, timeoutMs, "accept", s"at $path").?
        var c = -1
        while c < 0 do
          c = (PosixSocket.Sys.accept.invoke(cap, f, MemorySegment.NULL, MemorySegment.NULL): Int)
          if c < 0 then
            val e = PosixSocket.errnoOf(cap)
            if e == PosixSocket.Errno.EAGAIN then PosixSocket.Failed("accept", e, s"timed out at $path").?
            else if e != PosixSocket.Errno.EINTR then PosixSocket.Failed("accept", e, s"at $path").?
        val accepted = guarded(c)(PosixSocket.closeQuietly)
        PosixSocket.cloexec(cap, accepted) __ Unit
        setRcvTimeout(cap, tmp, accepted, timeoutMs).?
        accepted.mapGuarded(new Conn(_, timeoutMs))

    def close(): Unit =
      if fd >= 0 then
        PosixSocket.closeQuietly(fd)
        fd = -1
        Files.deleteIfExists(path) __ Unit
  }


  /** Adopts an already-open descriptor — inherited from a parent process, or received over another
    * socket — as a `Conn`, which then owns it (`close` closes the descriptor).  The descriptor is
    * checked for validity but not for socketness, and works for stream or datagram-style sockets
    * alike; no receive timeout is set (use `setTimeout` if one is wanted).
    */
  def adopt(fd: Int): Ask[Conn] = Ask.flat:
    checkSupported().?
    val tmp = Arena.ofConfined()
    try
      val cap = PosixSocket.capture(tmp)
      Ask:
        if (PosixSocket.Sys.fcntl.invoke(cap, fd, PosixSocket.F_GETFD, 0): Int) < 0 then   // is it even open?
          PosixSocket.Failed("fcntl", PosixSocket.errnoOf(cap), s"cannot adopt descriptor $fd").?
        new Conn(fd, 0L)
    finally tmp.close()

  /** Binds and listens at `path`, which must not already exist (stale socket files are the caller's to
    * clear).  The timeout bounds each `accept` wait and is inherited by accepted connections.
    */
  def listen(path: Path, timeout: Duration = defaultTimeout): Ask[Server] = Ask:
    checkSupported().?
    Resource.assemble:
      val tmp = Resource.whileAssembling(Arena.ofConfined())(_.close())
      val cap = PosixSocket.capture(tmp)
      val fd = guarded(newSocket(cap).?)(PosixSocket.closeQuietly)
      val sa = sockaddr(tmp, path).?
      if (PosixSocket.Sys.bind.invoke(cap, fd, sa, PosixSocket.SockAddrUn.size): Int) != 0 then
        PosixSocket.Failed("bind", PosixSocket.errnoOf(cap), s"at $path").?
      guarded(path)(p => Files.deleteIfExists(p) __ Unit) __ Unit   // bind made the socket file; a failure from here on must not leave it
      if (PosixSocket.Sys.listen.invoke(cap, fd, 16): Int) != 0 then
        PosixSocket.Failed("listen", PosixSocket.errnoOf(cap), s"at $path").?
      val ms = millisOf(timeout)
      setRcvTimeout(cap, tmp, fd, ms).?
      fd.mapGuarded(new Server(_, path, ms))

  /** Connects to the listening socket at `path`; receives on the connection are bounded by the timeout. */
  def connect(path: Path, timeout: Duration = defaultTimeout): Ask[Conn] = Ask:
    checkSupported().?
    Resource.assemble:
      val tmp = Resource.whileAssembling(Arena.ofConfined())(_.close())
      val cap = PosixSocket.capture(tmp)
      val fd = guarded(newSocket(cap).?)(PosixSocket.closeQuietly)
      val sa = sockaddr(tmp, path).?
      if (PosixSocket.Sys.connect.invoke(cap, fd, sa, PosixSocket.SockAddrUn.size): Int) != 0 then
        PosixSocket.Failed("connect", PosixSocket.errnoOf(cap), s"at $path").?
      val ms = millisOf(timeout)
      setRcvTimeout(cap, tmp, fd, ms).?
      fd.mapGuarded(new Conn(_, ms))


  ///////////////////////////////////////////////////////////////////////
  // The raw tier: one system call per method, buffers allocated once, //
  // Result answers, descriptor count and truncation reported as facts //
  ///////////////////////////////////////////////////////////////////////

  /** A socket descriptor driven one system call at a time — the tier for a non-blocking, readiness-polled
    * loop.  Every buffer is allocated at construction, so a receive or a send allocates nothing and answers
    * a [[PosixSocket.Result]]: a count, or `-errno` to classify (`wouldBlock`, `interrupted`, `peerGone`, …).
    * Nothing here retries `EINTR` or loops on a partial send; `sendOnce` uses `MSG_DONTWAIT` so it never
    * blocks even on a blocking descriptor, while `readOnce`/`writeOnce` are plain `read`/`write`.
    *
    * Descriptors that arrive with a message are kept, up to `fdSlots` of them: `fdCount` says how many,
    * `takeFd` hands them over one at a time, and any not taken are closed by the next receive or by
    * `close`.  The control buffer is offered at exactly `fdSlots` descriptors, so a message carrying more
    * is reported as `ctrunc`, never quietly trimmed.  On Darwin such a message has nonetheless installed
    * every descriptor it carried, and those beyond the slots cannot be found to close: size `fdSlots` for
    * the protocol's maximum, and treat `ctrunc` as a compromised connection.  `trunc` is the kernel's
    * `MSG_TRUNC` (datagram longer than the buffer).  A received descriptor is close-on-exec (Darwin closes
    * the window with an `fcntl` right after).  `sendOnceWithFds` sends up to `fdSlots` (at least one).
    *
    * Buffers must be off-heap (`Mem.alloc`, or a native segment): the kernel reads and writes them in place,
    * and the linker refuses a heap-backed one outright.  A buffer's arena must stay open until the call
    * returns; an automatic one is kept reachable through the call here, since the kernel holds its address
    * inside an `iovec` where the linker cannot see it.
    *
    * Setup calls (`cloexec`, `nonblocking`, `setOption`, …) and receives belong to one thread, sends to at
    * most one other.  `shutdown` is thread-safe and is how a blocked call is made to return; `close` is
    * idempotent and never throws, but must wait for in-flight calls to return (shutdown, let the loop exit,
    * then close) — closing under a receive races its descriptor bookkeeping.  Wait for readiness with a
    * [[PosixSocket.Poller]] over `descriptor` (and, typically, a wake endpoint from [[Raw.pair]]).
    */
  final class Raw private[eio] (fd0: Int, fdSlots: Int) extends AutoCloseable {
    private var fd = fd0
    private val arena   = Arena.ofAuto()             // reclaimed with this object; never closed under a call
    private val cap     = PosixSocket.capture(arena)        // receive side and setup
    private val sendCap = PosixSocket.capture(arena)        // send side
    private val iov     = arena.allocate(PosixSocket.IoVec.size)
    private val ctrlLen = PosixSocket.CMsg.len(4L * fdSlots)               // CMSG_LEN: exactly fdSlots descriptors fit
    private val ctrl    = arena.allocate(PosixSocket.CMsg.space(4L * fdSlots))
    private val mh      = arena.allocate(PosixSocket.MsgHdr.size)
    private val sendSlots = if fdSlots < 1 then 1 else fdSlots
    private val sIov    = arena.allocate(PosixSocket.IoVec.size)
    private val sCtrl   = arena.allocate(PosixSocket.CMsg.space(4L * sendSlots))
    private val sMh     = arena.allocate(PosixSocket.MsgHdr.size)
    private val fds = new Array[Int](fdSlots)
    private var nFds = 0
    private var taken = 0
    private var ctrlTruncated = false
    private var dataTruncated = false
    private val closed = new AtomicBoolean(false)

    /** The descriptor itself, for calls this class does not wrap; -1 once closed. */
    def descriptor: Int = fd
    def isClosed: Boolean = closed.get

    private def live: Ask[Int] = if fd >= 0 then Is(fd) else Err.or("socket is closed")
    private def withTmp[A](f: (MemorySegment, Arena) => A): A =
      val tmp = Arena.ofConfined()
      try f(PosixSocket.capture(tmp), tmp)
      finally tmp.close()

    /** Marks the descriptor close-on-exec. */
    def cloexec(): Ask[Unit] = Ask.flat:
      val f = live.?
      withTmp((c, _) => PosixSocket.cloexec(c, f).ask("fcntl(F_SETFD)")).map(_ => ())

    /** Sets (or clears) `O_NONBLOCK`. */
    def nonblocking(on: Boolean = true): Ask[Unit] = Ask.flat:
      val f = live.?
      withTmp((c, _) => PosixSocket.nonblocking(c, f, on).ask("fcntl(F_SETFL)")).map(_ => ())

    def isNonblocking: Ask[Boolean] = Ask.flat:
      val f = live.?
      withTmp((c, _) => PosixSocket.isNonblocking(c, f).ask("fcntl(F_GETFL)")).map(_ == 1L)

    /** The socket's `SO_TYPE` (`PosixSocket.SOCK_STREAM`, `SOCK_DGRAM`, `SOCK_SEQPACKET`). */
    def sockType: Ask[Int] = option(PosixSocket.SO_TYPE)

    /** An `int`-valued `SOL_SOCKET` option, e.g. `PosixSocket.SO_RCVBUF`. */
    def option(opt: Int): Ask[Int] = Ask.flat:
      val f = live.?
      withTmp((c, tmp) => PosixSocket.getOptInt(c, tmp, f, opt).ask("getsockopt", s"option $opt")).map(_.toInt)

    def setOption(opt: Int, value: Int): Ask[Unit] = Ask.flat:
      val f = live.?
      withTmp((c, tmp) => PosixSocket.setOptInt(c, tmp, f, opt, value).ask("setsockopt", s"option $opt")).map(_ => ())

    /** One `recvmsg` into the first `limit` bytes of `buf` (all of it when `limit` is negative): the byte
      * count (0 = end of stream, or an empty datagram), or `-errno`.  Descriptors that came along are
      * collected (see `fdCount`/`takeFd`); any left from the previous receive are closed first.
      */
    def recvMsg(buf: Mem[Byte], limit: Long = -1L): PosixSocket.Result =
      closeFds()
      ctrlTruncated = false
      dataTruncated = false
      if fd < 0 then PosixSocket.Result.failure(PosixSocket.Errno.EBADF)
      else
        val k = if limit < 0 || limit > buf.length then buf.length else limit
        PosixSocket.IoVec.set(iov, buf.segment, k)
        PosixSocket.MsgHdr.init(mh, iov, ctrl, ctrlLen)
        val n: Long = PosixSocket.Sys.recvmsg.invoke(cap, fd, mh, PosixSocket.MSG_CMSG_CLOEXEC)
        java.lang.ref.Reference.reachabilityFence(buf)   // the kernel wrote through the iovec's address
        if n < 0 then PosixSocket.Result.of(n, cap)
        else
          PosixSocket.CMsg.forEachFd(ctrl, PosixSocket.MsgHdr.controlLength(mh)){ f =>
            if nFds < fds.length then
              fds(nFds) = f
              nFds += 1
            else
              PosixSocket.closeQuietly(f)   // cannot happen at CMSG_LEN capacity; if it does, say so
              ctrlTruncated = true
          }
          if PosixSocket.mac && nFds > 0 then
            var i = 0
            while i < nFds do
              PosixSocket.cloexec(cap, fds(i)) __ Unit   // no MSG_CMSG_CLOEXEC on Darwin: a real, if short, inheritable window
              i += 1
          val flags = PosixSocket.MsgHdr.flagsOf(mh)
          ctrlTruncated = ctrlTruncated || (flags & PosixSocket.MSG_CTRUNC) != 0
          dataTruncated = (flags & PosixSocket.MSG_TRUNC) != 0
          PosixSocket.Result.success(n)

    /** Descriptors attached to the last successful `recvMsg` (at most `fdSlots`; extras were closed). */
    def fdCount: Int = nFds
    /** The last receive's control data was truncated (more descriptors than slots, or a foreign control message). */
    def ctrunc: Boolean = ctrlTruncated
    /** The last receive's datagram was longer than the buffer offered. */
    def trunc: Boolean = dataTruncated
    /** Ownership of one attached descriptor, in arrival order; -1 when none remain. */
    def takeFd(): Int =
      if taken < nFds then
        val f = fds(taken)
        taken += 1
        f
      else -1
    /** Closes every attached descriptor not yet taken. */
    def closeFds(): Unit =
      while taken < nFds do
        PosixSocket.closeQuietly(fds(taken))
        taken += 1
      nFds = 0
      taken = 0

    /** One `send` of the first `n` bytes of `buf` (all when negative) with `MSG_DONTWAIT | MSG_NOSIGNAL`:
      * bytes accepted, or `-errno`.  Never loops, never blocks.
      */
    def sendOnce(buf: Mem[Byte], n: Long = -1L): PosixSocket.Result =
      if fd < 0 then PosixSocket.Result.failure(PosixSocket.Errno.EBADF)
      else
        val k = if n < 0 || n > buf.length then buf.length else n
        PosixSocket.Result.of((PosixSocket.Sys.send.invoke(sendCap, fd, buf.segment, k, PosixSocket.MSG_DONTWAIT | PosixSocket.MSG_NOSIGNAL): Long), sendCap)

    /** As `sendOnce`, with all of `fds` attached as one `SCM_RIGHTS` message (`n` must be at least 1;
      * between 1 and `fdSlots` descriptors, else `EINVAL`).
      */
    def sendOnceWithFds(buf: Mem[Byte], n: Long, fds: Array[Int]): PosixSocket.Result =
      if fd < 0 then PosixSocket.Result.failure(PosixSocket.Errno.EBADF)
      else if fds.length < 1 || fds.length > sendSlots then PosixSocket.Result.failure(PosixSocket.Errno.EINVAL)
      else
        val k = if n < 0 || n > buf.length then buf.length else n
        PosixSocket.IoVec.set(sIov, buf.segment, k)
        val clen = PosixSocket.CMsg.writeFds(sCtrl, fds)
        PosixSocket.MsgHdr.init(sMh, sIov, sCtrl, clen)
        val r: Long = PosixSocket.Sys.sendmsg.invoke(sendCap, fd, sMh, PosixSocket.MSG_DONTWAIT | PosixSocket.MSG_NOSIGNAL)
        java.lang.ref.Reference.reachabilityFence(buf)
        PosixSocket.Result.of(r, sendCap)

    /** As `sendOnce`, with `passFd` attached as `SCM_RIGHTS` (`n` must be at least 1). */
    def sendOnceWithFd(buf: Mem[Byte], n: Long, passFd: Int): PosixSocket.Result =
      if fd < 0 then PosixSocket.Result.failure(PosixSocket.Errno.EBADF)
      else
        val k = if n < 0 || n > buf.length then buf.length else n
        PosixSocket.IoVec.set(sIov, buf.segment, k)
        val clen = PosixSocket.CMsg.writeFd(sCtrl, passFd)
        PosixSocket.MsgHdr.init(sMh, sIov, sCtrl, clen)
        val r: Long = PosixSocket.Sys.sendmsg.invoke(sendCap, fd, sMh, PosixSocket.MSG_DONTWAIT | PosixSocket.MSG_NOSIGNAL)
        java.lang.ref.Reference.reachabilityFence(buf)   // the kernel read through the iovec's address
        PosixSocket.Result.of(r, sendCap)

    /** One `read` into the first `n` bytes of `buf` (all when negative); blocks only if the descriptor does. */
    def readOnce(buf: Mem[Byte], n: Long = -1L): PosixSocket.Result =
      if fd < 0 then PosixSocket.Result.failure(PosixSocket.Errno.EBADF)
      else
        val k = if n < 0 || n > buf.length then buf.length else n
        PosixSocket.Result.of((PosixSocket.Sys.read.invoke(cap, fd, buf.segment, k): Long), cap)

    /** One `write` of the first `n` bytes of `buf` (all when negative); blocks only if the descriptor does. */
    def writeOnce(buf: Mem[Byte], n: Long = -1L): PosixSocket.Result =
      if fd < 0 then PosixSocket.Result.failure(PosixSocket.Errno.EBADF)
      else
        val k = if n < 0 || n > buf.length then buf.length else n
        PosixSocket.Result.of((PosixSocket.Sys.write.invoke(sendCap, fd, buf.segment, k): Long), sendCap)

    /** Stops traffic in the given direction(s) (default both); a blocked peer sees the socket gone.  Thread-safe. */
    def shutdown(how: Int = PosixSocket.SHUT_RDWR): Ask[Unit] =
      val f = fd
      if f < 0 then Err.or("socket is closed")
      else withTmp((c, _) => PosixSocket.shutdown(c, f, how).ask("shutdown")).map(_ => ())

    /** Socket state for a diagnostic: type, buffer sizes, blocking configuration. */
    def describe: String =
      val f = fd
      if f < 0 then "fd=closed"
      else withTmp: (c, tmp) =>
        def opt(o: Int): String = PosixSocket.getOptInt(c, tmp, f, o) match
          case r if r.ok => r.count.toString
          case r => r.name
        val nb = PosixSocket.isNonblocking(c, f)
        s"fd=$f SO_TYPE=${opt(PosixSocket.SO_TYPE)} SO_RCVBUF=${opt(PosixSocket.SO_RCVBUF)} SO_SNDBUF=${opt(PosixSocket.SO_SNDBUF)} nonblocking=${if nb.ok then nb.count == 1 else nb.name}"

    /** Closes untaken descriptors and the socket; the buffers go with this object.  Idempotent, never throws;
      * call it once in-flight calls have returned (see the class note).
      */
    def close(): Unit =
      if closed.compareAndSet(false, true) then
        closeFds()
        PosixSocket.closeQuietly(fd)
        fd = -1

    /** Gives the descriptor back to the caller instead of closing it: untaken received descriptors are
      * closed, this `Raw` is closed, and the answer is the descriptor (-1 if already closed).  For backing
      * out of a setup that adopted a descriptor it does not own.
      */
    def disown(): Int =
      if closed.compareAndSet(false, true) then
        closeFds()
        val f = fd
        fd = -1
        f
      else -1
  }

  object Raw {
    /** Descriptors one receive can hold before `ctrunc`; more than one so that a peer sending two is a
      * countable fact rather than a truncation.
      */
    inline val DefaultFdSlots = 4

    /** Adopts an already-open descriptor of any socket type; it is checked for being open, nothing else,
      * and its flags are left alone (call `cloexec`/`nonblocking` as wanted).  The `Raw` owns it.
      */
    def adopt(fd: Int, fdSlots: Int = DefaultFdSlots): Ask[Raw] = Ask.flat:
      checkSupported().?
      val tmp = Arena.ofConfined()
      try
        val cap = PosixSocket.capture(tmp)
        if (PosixSocket.Sys.fcntl.invoke(cap, fd, PosixSocket.F_GETFD, 0): Int) < 0 then
          Err.or(PosixSocket.Failed("fcntl", PosixSocket.errnoOf(cap), s"cannot adopt descriptor $fd"))
        else Is(new Raw(fd, fdSlots))
      finally tmp.close()

    /** A connected `AF_UNIX` pair of the given type (`PosixSocket.SOCK_STREAM`, `SOCK_DGRAM`, `SOCK_SEQPACKET`),
      * both ends close-on-exec and blocking until told otherwise.  A stream pair makes a wake endpoint for
      * a polled loop: write a byte to one end, poll the other beside the socket of interest.
      */
    def pair(sockType: Int = PosixSocket.SOCK_STREAM, fdSlots: Int = DefaultFdSlots): Ask[(Raw, Raw)] =
      PosixSocket.pair(sockType).map((a, b) => (new Raw(a, fdSlots), new Raw(b, fdSlots)))
  }
}
