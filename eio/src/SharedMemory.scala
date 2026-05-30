// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.eio


import java.lang.foreign.Arena
import java.nio.channels.FileChannel
import java.nio.file.{Path, Files, StandardOpenOption}

import kse.basics.{given, _}
import kse.flow.{given, _}


/** RAM-backed off-heap memory shared between processes (or threads) by mapping the same path read-write.
  *
  * For high-performance sharing the backing must never hit disk: the region lives on a RAM filesystem
  * (tmpfs), so pages stay in memory and only ever reach disk if the OS swaps.  There is no portable JDK
  * way to guarantee this — it relies on a tmpfs mount (Linux `/dev/shm`); on platforms without one,
  * [[create]] fails rather than silently spilling to disk (true portability awaits native `shm_open` /
  * `CreateFileMapping` support in a future `alien` package).
  *
  * The backing file is the shared medium, so its naming, lifetime, and cleanup are the caller's job
  * (a tmpfs file holds RAM until deleted).  A region is sized to exactly `n * bytesOf[A]` bytes, so its
  * length is self-describing — another party need only learn the `path` (and agree on the element type)
  * and can attach with `path.openIOMem[A]()`, recovering the count from the file length.
  */
object SharedMemory {
  /** A writable RAM-filesystem (tmpfs) directory where mappings stay off disk, if one can be found.
    * Checked in order: `-Dkse.eio.shmdir`, then `/dev/shm`, `/run/shm`.  `None` on macOS/Windows.
    */
  lazy val ramDirectory: Option[Path] =
    val candidates =
      Option(System.getProperty("kse.eio.shmdir")).map(Path.of(_)).toList :::
      List(Path.of("/dev/shm"), Path.of("/run/shm"))
    candidates.find(p => Files.isDirectory(p) && Files.isWritable(p))
  /** A shared region: the owned mapping plus the `path` others can attach to.  `close` unmaps and unlinks
    * the backing file — POSIX semantics, so a mapping already open elsewhere stays valid until it too
    * closes, while the name disappears (no new attachers) and the memory is reclaimed once all unmap.
    */
  final class Region[A <: Mem.Type] private[SharedMemory] (val path: Path, val owned: Mem.Owned[A]) extends AutoCloseable {
    inline def memory: Mem[A] = owned.memory
    inline def op[B](inline f: Mem[A] => B): B = owned.op(f)
    inline def use(inline f: Mem[A] => Unit): Unit = owned.use(f)
    def close(): Unit =
      try owned.close()
      finally Files.deleteIfExists(path) __ Unit
  }

  /** Map `p` (created if absent) read-write shared, sized to exactly `bytes`, in a fresh shared `Arena`. */
  def mapShared[A <: Mem.Type](p: Path, bytes: Long): Region[A] =
    val arena = Arena.ofShared()
    try
      val owned = Mem.Owned.create[A](arena){ a =>
        val ch = FileChannel.open(p, StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.CREATE)
        try
          if bytes < ch.size then ch.truncate(bytes) __ Unit
          ch.map(FileChannel.MapMode.READ_WRITE, 0L, bytes, a)
        finally ch.close()
      }
      new Region[A](p, owned)
    catch
      case e if e.catchable =>
        arena.close()
        throw e

  /** Create a fresh file and share `n` items of `A` through it; the region's `path` is the new file.
    * Prefers a RAM filesystem (see [[ramDirectory]]).  With `allowBackingFile = false` (default) this
    * fails when no tmpfs is available rather than touching disk; set it `true` to permit a disk-backed
    * temp file as a cross-platform fallback (NOT RAM-resident — such a mapping spills to disk).
    */
  inline def create[A <: Mem.Type](n: Long, allowBackingFile: Boolean = false)(using Tidy.Nice[Region[A]]): Ask[Region[A]] =
    Ask:
      val file = ramDirectory match
        case Some(dir) => Files.createTempFile(dir, "kse-shm-", ".mem")
        case None =>
          if allowBackingFile then Files.createTempFile("kse-shm-", ".mem")
          else Err ?# "No RAM-backed (tmpfs) directory found; pass allowBackingFile = true to fall back to a disk-backed temp file"
      mapShared[A](file, n * Mem.bytesOf[A])

  /** Share `n` items of `A` through `p` (created if absent), a path you or another party chose.  For RAM
    * residence, `p` must live on a tmpfs mount (e.g. under [[ramDirectory]]); that is the caller's to ensure.
    */
  inline def createFrom[A <: Mem.Type](p: Path, n: Long)(using Tidy.Nice[Region[A]]): Ask[Region[A]] =
    Ask:
      mapShared[A](p, n * Mem.bytesOf[A])
}
