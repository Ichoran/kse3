// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.eio


import java.lang.foreign.{Arena, MemorySegment, Linker, FunctionDescriptor}
import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_BYTE, JAVA_SHORT, JAVA_INT, JAVA_LONG}
import java.lang.invoke.MethodHandle
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Files}
import java.time.Duration

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
  * A `Server` listens at a filesystem path; `connect` reaches it; a `Conn` moves bytes (`read`/`write`)
  * and descriptors (`sendFd`/`recvFd`, or `recvMsgOrFd` for mixed traffic).  Receives are bounded by a timeout (set at creation, adjustable
  * with `setTimeout`) so nothing waits forever by default.  Calls block in native code, so on a virtual
  * thread they pin their carrier — use a platform thread (or accept the pinning) for long waits.
  * Connections are not thread-safe; use each from one thread at a time.
  */
object FdSock {
  private lazy val osName  = System.getProperty("os.name", "").toLowerCase
  private lazy val onMac   = osName.contains("mac") || osName.contains("darwin")
  private lazy val onLinux = osName.contains("nux")

  /** True if this platform supports descriptor-passing sockets (Linux and macOS do; Windows does not). */
  def supported: Boolean = onMac || onLinux

  /** The receive timeout used when none is given. */
  val defaultTimeout: Duration = 30.s

  /** Timeouts are applied at whole-millisecond resolution, rounding up so a short timeout stays a timeout
    * (200.us waits 1 ms, not forever); a zero or negative timeout means wait without limit.
    */
  private[eio] def millisOf(timeout: Duration): Long =
    val ms = timeout.ceil.ms.toMillis
    if ms < 0 then 0L else ms

  /** The platform-varying struct layouts and constants (64-bit Linux vs. macOS), in one place. */
  private object P {
    val mac = onMac
    // struct sockaddr_un: Linux { u16 family; char path[108] }; macOS { u8 len; u8 family; char path[104] }
    val saSize    = if mac then 106 else 110
    val saPathOff = 2L
    val saPathMax = if mac then 103 else 107          // room for the terminating NUL
    // struct msghdr: same fields, different widths/padding (Linux 56 bytes, macOS 48)
    val mhSize       = if mac then 48L else 56L
    val mhIov        = 16L
    val mhIovLen     = 24L                            // size_t on Linux, int on macOS
    val mhControl    = 32L
    val mhControlLen = 40L                            // size_t on Linux, socklen_t on macOS
    val mhFlags      = if mac then 44L else 48L
    // struct cmsghdr: Linux { u64 len; i32 level; i32 type } 8-aligned; macOS { u32 len; i32 level; i32 type } 4-aligned
    val cmHdr      = if mac then 12L else 16L
    val cmAlign    = if mac then 4L else 8L
    val cmLevelOff = if mac then 4L else 8L
    val cmTypeOff  = if mac then 8L else 12L
    def cmsgAlign(n: Long): Long = (n + cmAlign - 1) & ~(cmAlign - 1)
    def cmsgSpace(data: Long): Long = cmHdr + cmsgAlign(data)
    val AF_UNIX = 1
    val SOCK_STREAM = 1
    val SOL_SOCKET  = if mac then 0xFFFF else 1
    val SCM_RIGHTS  = 1
    val SO_RCVTIMEO = if mac then 0x1006 else 20
    val MSG_CTRUNC  = if mac then 0x20 else 0x08
    val MSG_TRUNC   = if mac then 0x10 else 0x20
    val MSG_CMSG_CLOEXEC = if mac then 0 else 0x40000000   // no such flag on macOS; fcntl afterwards instead
    val EINTR  = 4
    val EAGAIN = if mac then 35 else 11
  }

  /** Native libc bindings.  Calls that acquire or wait capture `errno` (prepended capture-segment arg). */
  private object N {
    private val linker = Linker.nativeLinker()
    private val look   = linker.defaultLookup()
    private def dl(sym: String, fd: FunctionDescriptor, opts: Linker.Option*): MethodHandle =
      linker.downcallHandle(look.find(sym).orElseThrow(() => new UnsatisfiedLinkError(s"missing native symbol: $sym")), fd, opts*)
    private val captureErr = Linker.Option.captureCallState("errno")
    val captureLayout = Linker.Option.captureStateLayout()
    val errnoVH       = captureLayout.varHandle(java.lang.foreign.MemoryLayout.PathElement.groupElement("errno"))
    val socket     = dl("socket",     FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT), captureErr)
    val bind       = dl("bind",       FunctionDescriptor.of(JAVA_INT, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val listen     = dl("listen",     FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT), captureErr)
    val accept     = dl("accept",     FunctionDescriptor.of(JAVA_INT, JAVA_INT, ADDRESS, ADDRESS), captureErr)
    val connect    = dl("connect",    FunctionDescriptor.of(JAVA_INT, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val sendmsg    = dl("sendmsg",    FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val recvmsg    = dl("recvmsg",    FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val setsockopt = dl("setsockopt", FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT, ADDRESS, JAVA_INT), captureErr)
    val readFd     = dl("read",       FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_LONG), captureErr)
    val writeFd    = dl("write",      FunctionDescriptor.of(JAVA_LONG, JAVA_INT, ADDRESS, JAVA_LONG), captureErr)
    val closeFd    = dl("close",      FunctionDescriptor.of(JAVA_INT, JAVA_INT))
    val fcntl      = dl("fcntl",      FunctionDescriptor.of(JAVA_INT, JAVA_INT, JAVA_INT, JAVA_INT), Linker.Option.firstVariadicArg(2), captureErr)
  }

  private inline def err(cap: MemorySegment): Int = N.errnoVH.get(cap, 0L)

  private def closeQuietly(fd: Int): Unit = (N.closeFd.invoke(fd): Int) __ Unit

  private def cloexec(cap: MemorySegment, fd: Int): Unit =
    (N.fcntl.invoke(cap, fd, 2, 1): Int) __ Unit   // F_SETFD, FD_CLOEXEC; a failure here is harmless

  private def checkSupported(): Ask[Unit] =
    if supported then Is.unit
    else Err.or(s"descriptor-passing sockets are unsupported on '$osName'")

  private def newSocket(cap: MemorySegment): Ask[Int] = Ask:
    val fd: Int = N.socket.invoke(cap, P.AF_UNIX, P.SOCK_STREAM, 0)
    if fd < 0 then Err ?# s"socket failed (errno=${err(cap)})"
    cloexec(cap, fd)
    fd

  private def sockaddr(tmp: Arena, path: Path): Ask[MemorySegment] = Ask:
    val bytes = path.toString.getBytes(StandardCharsets.UTF_8)
    if bytes.length > P.saPathMax then Err ?# s"socket path too long (${bytes.length} > ${P.saPathMax} bytes): $path"
    val sa = tmp.allocate(P.saSize)
    if P.mac then
      sa.set(JAVA_BYTE, 0L, P.saSize.toByte)
      sa.set(JAVA_BYTE, 1L, P.AF_UNIX.toByte)
    else sa.set(JAVA_SHORT, 0L, P.AF_UNIX.toShort)
    MemorySegment.copy(bytes, 0, sa, JAVA_BYTE, P.saPathOff, bytes.length)
    sa

  private def setRcvTimeout(cap: MemorySegment, tmp: Arena, fd: Int, millis: Long): Ask[Unit] = Ask:
    val tv = tmp.allocate(16L)   // struct timeval: { i64 sec; i64 usec } on Linux, { i64 sec; i32 usec } on macOS
    tv.set(JAVA_LONG, 0L, millis / 1000)
    if P.mac then tv.set(JAVA_INT, 8L, ((millis % 1000) * 1000).toInt)
    else tv.set(JAVA_LONG, 8L, (millis % 1000) * 1000)
    if (N.setsockopt.invoke(cap, fd, P.SOL_SOCKET, P.SO_RCVTIMEO, tv, 16): Int) != 0 then
      Err ?# s"setsockopt(SO_RCVTIMEO) failed (errno=${err(cap)})"


  /** A connected Unix-domain stream socket: `read`/`write` for bytes, `sendFd`/`recvFd` for descriptors. */
  final class Conn private[eio] (fd0: Int) extends AutoCloseable {
    private var fd = fd0
    private def live: Ask[Int] = if fd >= 0 then Is(fd) else Err.or("socket is closed")

    /** Changes the receive timeout (bounds `read` and `recvFd`); zero or negative waits without limit. */
    def setTimeout(timeout: Duration): Ask[Unit] = Ask.flat:
      val f = live.?
      val tmp = Arena.ofConfined()
      try setRcvTimeout(tmp.allocate(N.captureLayout), tmp, f, millisOf(timeout))
      finally tmp.close()

    /** Writes all of `data` (ordinary bytes, no descriptor). */
    def write(data: Array[Byte]): Ask[Unit] = Ask:
      val f = live.?
      if data.length > 0 then
        val tmp = Arena.ofConfined()
        try
          val cap = tmp.allocate(N.captureLayout)
          val seg = tmp.allocate(data.length.toLong)
          MemorySegment.copy(data, 0, seg, JAVA_BYTE, 0L, data.length)
          var off = 0L
          while off < data.length do
            val n: Long = N.writeFd.invoke(cap, f, seg.asSlice(off), data.length - off)
            if n < 0 then
              val e = err(cap)
              if e != P.EINTR then Err ?# s"socket write failed (errno=$e)"
            else off += n
        finally tmp.close()

    /** Reads up to `buf.length` bytes into `buf`, answering the count; 0 means the peer closed. */
    def read(buf: Array[Byte]): Ask[Int] = Ask:
      val f = live.?
      if buf.length == 0 then 0
      else
        val tmp = Arena.ofConfined()
        try
          val cap = tmp.allocate(N.captureLayout)
          val seg = tmp.allocate(buf.length.toLong)
          var n = -1L
          while n < 0 do
            n = N.readFd.invoke(cap, f, seg, buf.length.toLong)
            if n < 0 then
              val e = err(cap)
              if e == P.EAGAIN then Err ?# "socket read timed out"
              else if e != P.EINTR then Err ?# s"socket read failed (errno=$e)"
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
        val cap = tmp.allocate(N.captureLayout)
        val data = tmp.allocate(tag.length.toLong)
        MemorySegment.copy(tag, 0, data, JAVA_BYTE, 0L, tag.length)
        val iov = tmp.allocate(16L)                    // struct iovec { void* base; size_t len }
        iov.set(ADDRESS, 0L, data)
        iov.set(JAVA_LONG, 8L, tag.length.toLong)
        val ctrl = tmp.allocate(P.cmsgSpace(4L))
        if P.mac then ctrl.set(JAVA_INT, 0L, (P.cmHdr + 4L).toInt) else ctrl.set(JAVA_LONG, 0L, P.cmHdr + 4L)
        ctrl.set(JAVA_INT, P.cmLevelOff, P.SOL_SOCKET)
        ctrl.set(JAVA_INT, P.cmTypeOff, P.SCM_RIGHTS)
        ctrl.set(JAVA_INT, P.cmHdr, passFd)
        val mh = tmp.allocate(P.mhSize)
        mh.set(ADDRESS, P.mhIov, iov)
        if P.mac then mh.set(JAVA_INT, P.mhIovLen, 1) else mh.set(JAVA_LONG, P.mhIovLen, 1L)
        mh.set(ADDRESS, P.mhControl, ctrl)
        if P.mac then mh.set(JAVA_INT, P.mhControlLen, P.cmsgSpace(4L).toInt) else mh.set(JAVA_LONG, P.mhControlLen, P.cmsgSpace(4L))
        var sent = -1L
        while sent < 0 do
          sent = N.sendmsg.invoke(cap, f, mh, 0)
          if sent < 0 then
            val e = err(cap)
            if e != P.EINTR then Err ?# s"sendmsg failed (errno=$e)"
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
      * consumes is closed by the kernel, unrecoverably.  Size `buf` for the protocol's largest message —
      * an oversized datagram is an `Err` (truncation), not a partial delivery.
      *
      * Should a peer send several descriptors, the first is kept and the rest are closed.  A received
      * descriptor is close-on-exec and ours to close.
      */
    def recvMsgOrFd(buf: Array[Byte]): Ask[(fd: Option[Int], count: Int)] = Ask:
      val f = live.?
      if buf.length == 0 then Err ?# "receive buffer must hold at least one byte"
      val tmp = Arena.ofConfined()
      try
        val cap = tmp.allocate(N.captureLayout)
        val data = tmp.allocate(buf.length.toLong)
        val iov = tmp.allocate(16L)
        iov.set(ADDRESS, 0L, data)
        iov.set(JAVA_LONG, 8L, buf.length.toLong)
        val ctrlCap = P.cmsgSpace(4L * 8)              // room for extra descriptors so a chatty peer cannot truncate
        val ctrl = tmp.allocate(ctrlCap)
        val mh = tmp.allocate(P.mhSize)
        mh.set(ADDRESS, P.mhIov, iov)
        if P.mac then mh.set(JAVA_INT, P.mhIovLen, 1) else mh.set(JAVA_LONG, P.mhIovLen, 1L)
        mh.set(ADDRESS, P.mhControl, ctrl)
        if P.mac then mh.set(JAVA_INT, P.mhControlLen, ctrlCap.toInt) else mh.set(JAVA_LONG, P.mhControlLen, ctrlCap)
        var n = -1L
        while n < 0 do
          n = N.recvmsg.invoke(cap, f, mh, P.MSG_CMSG_CLOEXEC)
          if n < 0 then
            val e = err(cap)
            if e == P.EAGAIN then Err ?# "recvmsg timed out with no descriptor"
            else if e != P.EINTR then Err ?# s"recvmsg failed (errno=$e)"
        val clen = if P.mac then (mh.get(JAVA_INT, P.mhControlLen)).toLong else mh.get(JAVA_LONG, P.mhControlLen)
        var got = -1
        var off = 0L
        while off + P.cmHdr <= clen do
          val len = if P.mac then (ctrl.get(JAVA_INT, off)).toLong else ctrl.get(JAVA_LONG, off)
          if len < P.cmHdr || off + len > clen then off = clen   // malformed control data; stop walking
          else
            if ctrl.get(JAVA_INT, off + P.cmLevelOff) == P.SOL_SOCKET && ctrl.get(JAVA_INT, off + P.cmTypeOff) == P.SCM_RIGHTS then
              var d = off + P.cmHdr
              while d + 4 <= off + len do
                val f2 = ctrl.get(JAVA_INT, d)
                if got < 0 then got = f2 else closeQuietly(f2)
                d += 4
            off += P.cmsgAlign(len)
        val flags = mh.get(JAVA_INT, P.mhFlags)
        if (flags & P.MSG_CTRUNC) != 0 then
          if got >= 0 then closeQuietly(got)
          Err ?# "control data truncated; descriptor discarded"
        if (flags & P.MSG_TRUNC) != 0 then
          if got >= 0 then closeQuietly(got)
          Err ?# s"message truncated (buffer holds only ${buf.length} bytes); descriptor discarded"
        if n == 0 then
          if got >= 0 then closeQuietly(got)   // no bytes means the peer is gone; a stray descriptor is not a grant
          (fd = None, count = 0)
        else
          if got >= 0 && P.mac then cloexec(cap, got)   // Linux already got it via MSG_CMSG_CLOEXEC
          MemorySegment.copy(data, JAVA_BYTE, 0L, buf, 0, n.toInt)
          (fd = if got >= 0 then Some(got) else None, count = n.toInt)
      finally tmp.close()

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
        closeQuietly(fd)
        fd = -1
  }


  /** A listening Unix-domain socket bound at `path`; `accept()` yields one connection per client.
    * `close` stops listening and unlinks `path`.
    */
  final class Server private[eio] (fd0: Int, val path: Path, timeoutMs: Long) extends AutoCloseable {
    private var fd = fd0
    private def live: Ask[Int] = if fd >= 0 then Is(fd) else Err.or("server socket is closed")

    /** Accepts one connection, waiting at most the server's timeout; the connection inherits the timeout. */
    def accept(): Ask[Conn] = Ask.flat:
      val f = live.?
      val tmp = Arena.ofConfined()
      try
        val cap = tmp.allocate(N.captureLayout)
        var c = -1
        while c < 0 do
          c = (N.accept.invoke(cap, f, MemorySegment.NULL, MemorySegment.NULL): Int)
          if c < 0 then
            val e = err(cap)
            if e == P.EAGAIN then Err ?# s"accept timed out at $path"
            else if e != P.EINTR then Err ?# s"accept failed at $path (errno=$e)"
        cloexec(cap, c)
        setRcvTimeout(cap, tmp, c, timeoutMs)
          .peekAlt(_ => closeQuietly(c))
          .map(_ => new Conn(c))
      finally tmp.close()

    def close(): Unit =
      if fd >= 0 then
        closeQuietly(fd)
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
      val cap = tmp.allocate(N.captureLayout)
      Ask:
        if (N.fcntl.invoke(cap, fd, 1, 0): Int) < 0 then Err ?# s"cannot adopt descriptor $fd (errno=${err(cap)})"   // F_GETFD: is it even open?
        new Conn(fd)
    finally tmp.close()

  /** Binds and listens at `path`, which must not already exist (stale socket files are the caller's to
    * clear).  The timeout bounds each `accept` wait and is inherited by accepted connections.
    */
  def listen(path: Path, timeout: Duration = defaultTimeout): Ask[Server] = Ask.flat:
    checkSupported().?
    val tmp = Arena.ofConfined()
    try
      val cap = tmp.allocate(N.captureLayout)
      val fd = newSocket(cap).?
      Ask:
        val sa = sockaddr(tmp, path).?
        if (N.bind.invoke(cap, fd, sa, P.saSize): Int) != 0 then Err ?# s"bind failed at $path (errno=${err(cap)})"
        if (N.listen.invoke(cap, fd, 16): Int) != 0 then Err ?# s"listen failed at $path (errno=${err(cap)})"
        val ms = millisOf(timeout)
        setRcvTimeout(cap, tmp, fd, ms).?
        new Server(fd, path, ms)
      .peekAlt: _ =>
        closeQuietly(fd)
    finally tmp.close()

  /** Connects to the listening socket at `path`; receives on the connection are bounded by the timeout. */
  def connect(path: Path, timeout: Duration = defaultTimeout): Ask[Conn] = Ask.flat:
    checkSupported().?
    val tmp = Arena.ofConfined()
    try
      val cap = tmp.allocate(N.captureLayout)
      val fd = newSocket(cap).?
      Ask:
        val sa = sockaddr(tmp, path).?
        if (N.connect.invoke(cap, fd, sa, P.saSize): Int) != 0 then Err ?# s"connect failed at $path (errno=${err(cap)})"
        setRcvTimeout(cap, tmp, fd, millisOf(timeout)).?
        new Conn(fd)
      .peekAlt: _ =>
        closeQuietly(fd)
    finally tmp.close()
}
