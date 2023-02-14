// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020, 2021, 2023 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio

import java.io._
import java.nio.file._
import java.nio.file.attribute.FileTime
import java.time._
import java.util.zip._

import kse.flow.{given, _}

extension (pathname: String) {
  def file = new File(pathname)
  def path = FileSystems.getDefault.getPath(pathname)
}

extension (the_path: Path) {
  def name = the_path.getFileName.toString

  def nameTo(s: String) = the_path resolveSibling s

  def nameFn(f: String => String) = the_path resolveSibling f(the_path.getFileName.toString)

  def ext =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    if i < 1 then "" else n.substring(i+1)
  
  def extTo(x: String) =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    if i < 1 then
      if x.isEmpty then the_path
      else the_path resolveSibling n + "." + x
    else
      if x.isEmpty then the_path resolveSibling n.substring(0, i)
      else the_path resolveSibling n.substring(0, i+1) + x
  
  def extOp(f: String => String) =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    val e = if i < 1 then "" else n.substring(i+1)
    val x = f(e)
    if x == e then the_path
    else if i < 1 then the_path resolveSibling n + "." + x
    else if x.isEmpty then the_path resolveSibling n.substring(0, i)
    else the_path resolveSibling n.substring(0, i+1) + x
  
  def base =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    if i < 1 then n else n.substring(0, i)
  
  def baseTo(b: String) = 
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    if i < 1 then
      if n == b then the_path
      else the_path resolveSibling b
    else
      if i == b.length && n.substring(0, i) == b then the_path
      else the_path resolveSibling b + n.substring(i)
  
  def baseOp(f: String => String) =
    val n = the_path.getFileName.toString
    val i = n.lastIndexOf('.')
    val b = if i < 1 then n else n.substring(0, i)
    val x = f(b)
    if b == x then the_path
    else if i < 1 then the_path resolveSibling x
    else the_path resolveSibling x+n.substring(i)
  
  def parentName = the_path.getParent match { case null => ""; case p => p.getFileName.toString }

  def namesIterator = Iterator.iterate(the_path)(_.getParent).takeWhile(_ != null).map(_.getFileName.toString)

  def pathsIterator = Iterator.iterate(the_path)(_.getParent).takeWhile(_ != null)

  def parentOption = Option(the_path.getParent)

  def absolute = the_path.toAbsolutePath()

  def real =
    var abs = the_path.toAbsolutePath().normalize()
    var tail: Path = null
    var found = false
    while abs != null && !{ found = Files exists abs; found } do
      tail = if tail eq null then abs.getFileName else abs.getFileName resolve tail
      abs = abs.getParent
    val trunk = if found then abs.toRealPath() else abs
    if tail eq null then trunk else trunk resolve tail
  
  def file = the_path.toFile

  def /(that: String) = the_path resolve that

  def /(that: Path) = the_path resolve that

  def `..` = the_path.getParent match { case null => the_path; case p => p }

  def sib(that: String) = the_path resolveSibling that

  def sib(that: Path) = the_path resolveSibling that

  def reroot(oldRoot: Path, newRoot: Path): Path Or Unit =
    if the_path startsWith oldRoot then Is(newRoot resolve oldRoot.relativize(the_path))
    else Alt.unit

  def reroot(roots: (Path, Path)): Path Or Unit = reroot(roots._1, roots._2)

  def prune(child: Path): Path Or Unit =
    if child startsWith the_path then Is(the_path relativize child)
    else Alt.unit

  def exists = Files exists the_path

  def isDirectory = Files isDirectory the_path

  def isSymbolic = Files isSymbolicLink the_path

  def size = Files size the_path

  def safely: kse.eio.PathsHelper.Safely = PathsHelper.Safely(the_path)

  def t: FileTime = Files getLastModifiedTime the_path

  def t_=(ft: FileTime): Unit = Files.setLastModifiedTime(the_path, ft)
  
  def mkdir() = Files createDirectory the_path

  def mkdirs() = Files createDirectories the_path

  def delete() = Files delete the_path

  def touch(): Unit =
    if Files exists the_path then Files.setLastModifiedTime(the_path, FileTime from Instant.now)
    else Files.write(the_path, new Array[Byte](0))

  def paths =
    if !(Files exists the_path) then PathsHelper.emptyPathArray
    else if !(Files isDirectory the_path) then PathsHelper.emptyPathArray
    else safe { 
      val list = Files.list(the_path)
      val ans = list.toArray(i => new Array[Path](i))
      list.close
      ans
    }.getOrElse(_ => PathsHelper.emptyPathArray)

  def slurp: Array[String] Or Err = nice {
    val s = Files lines the_path
    val vb = Array.newBuilder[String]
    s.forEach(vb += _)
    s.close
    vb.result
  }

  def gulp: Array[Byte] Or Err = nice {
    Files readAllBytes the_path
  }

  /*
  // TODO -- evaluate whether it's better to use a ZipFileSystem to do this
  def unzipMap[A](selector: ZipEntry => Option[Array[Byte] => A]): Ok[String, List[A]] = safe{
    (new InputStreamShouldDoThis(Files newInputStream the_path)).unzipMap(selector).mapNo(e => s"Error while unzipping $the_path\n$e")
  }.mapNo(e => s"Could not open path $the_path\n${e.explain()}").flatten
  */

  def copyTo(to: Path): Unit =
    Files.copy(the_path, to, StandardCopyOption.REPLACE_EXISTING)

  def moveTo(to: Path): Unit =
    Files.move(the_path, to, StandardCopyOption.REPLACE_EXISTING)

  def atomicCopy(to: Path): Unit =
    val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
    to.getParent.tap{ gp => if gp ne null then { if !Files.exists(gp) then Files.createDirectories(gp) } }
    Files.copy(the_path, temp, StandardCopyOption.REPLACE_EXISTING)
    Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

  def atomicMove(to: Path): Unit =
    val up = to.getParent
    if up != null then
      if !Files.exists(up) then Files.createDirectories(up)
    if up != null && Files.getFileStore(the_path) == Files.getFileStore(up) then
      Files.move(the_path, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    else       
      val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
      to.getParent.tap{ gp => if gp ne null then { if !Files.exists(gp) then Files.createDirectories(gp) } }
      Files.copy(the_path, temp, StandardCopyOption.REPLACE_EXISTING)
      Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      Files.delete(the_path)

  def atomicZipCopy(to: Path, compression: Int Or Unit = Alt.unit, maxDirectoryDepth: Int = 10): Unit =
    val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
    to.getParent.tap{ gp => if gp ne null then { if !Files.exists(gp) then Files.createDirectories(gp) } }
    val zos = new ZipOutputStream(new FileOutputStream(temp.toFile))
    compression.foreach(zos.setLevel)
    if Files.isDirectory(the_path) then
      val base = the_path.getParent.fn{ fp => if fp eq null then FileSystems.getDefault.getPath("") else fp }
      def recurse(current: Path, maxDepth: Int): Unit = {
        val stable = current.paths
        val (directories, files) = stable.sortBy(_.getFileName.toString).partition(x => Files.isDirectory(x))
        files.foreach{ fi =>
          val rel = base relativize fi
          val ze = new ZipEntry(rel.toString)
          ze.setLastModifiedTime(Files.getLastModifiedTime(fi))
          zos.putNextEntry(ze)
          Files.copy(fi, zos)
          zos.closeEntry
        }
        if maxDepth > 1 then directories.foreach(d => recurse(d, maxDepth-1))
      }
      recurse(the_path, maxDirectoryDepth)
    else
      val ze = new ZipEntry(the_path.getFileName.toString)
      ze.setLastModifiedTime(Files.getLastModifiedTime(the_path))
      zos.putNextEntry(ze)
      Files.copy(the_path, zos)
      zos.closeEntry
    zos.close
    Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

  def recursively = new PathsHelper.RootedRecursion(the_path, the_path)

  def recurseIn(inside: Path) =
    if the_path startsWith inside then new PathsHelper.RootedRecursion(inside, the_path)
    else throw new IOException(s"Trying recursive operation in $inside but started outside at $the_path")
}

/*
extension (underlying: File) {
  def name = underlying.getName

  HERE -- TODO -- HERE

  def nameTo(s: String) = underlying resolveSibling s

  def nameFn(f: String => String) = underlying resolveSibling f(underlying.getFileName.toString)

  def ext =
    val n = underlying.getName
    val i = n.lastIndexOf('.')
    if (i < 1) "" else n.substring(i+1)
  
  def extTo(x: String) =
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    if (i < 1) {
      if (x.isEmpty) underlying
      else underlying resolveSibling n + "." + x
    }
    else {
      if (x.isEmpty) underlying resolveSibling n.substring(0, i)
      else underlying resolveSibling n.substring(0, i+1) + x
    }
  
  def extFn(f: String => String) =
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    val e = if (i < 1) "" else n.substring(i+1)
    val x = f(e)
    if (x == e) underlying
    else if (i < 1) underlying resolveSibling n + "." + x
    else if (x.isEmpty) underlying resolveSibling n.substring(0, i)
    else underlying resolveSibling n.substring(0, i+1) + x
  
  def base =
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    if (i < 1) n else n.substring(0, i)
  
  def baseTo(b: String) = 
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    if (i < 1) {
      if (n == b) underlying
      else underlying resolveSibling b
    }
    else {
      if (i == b.length && n.substring(0, i) == b) underlying
      else underlying resolveSibling b + n.substring(i)
    }
  
  def baseFn(f: String => String) =
    val n = underlying.getFileName.toString
    val i = n.lastIndexOf('.')
    val b = if (i < 1) n else n.substring(0, i)
    val x = f(b)
    if (b == x) underlying
    else if (i < 1) underlying resolveSibling x
    else underlying resolveSibling x+n.substring(i)
  
  def parentName = underlying.getParent match { case null => ""; case p => p.getFileName.toString }

  def namesIterator = Iterator.iterate(underlying)(_.getParent).takeWhile(_ != null).map(_.getFileName.toString)

  def pathsIterator = Iterator.iterate(underlying)(_.getParent).takeWhile(_ != null)

  def parentOption = Option(underlying.getParent)

  def absolute = underlying.toAbsolutePath()

  def real =
    var abs = underlying.toAbsolutePath().normalize()
    var tail: Path = null
    var found = false
    while (abs != null && !{ found = Files exists abs; found }) {
      tail = if (tail eq null) abs.getFileName else abs.getFileName resolve tail
      abs = abs.getParent
    }
    val trunk = if (found) abs.toRealPath() else abs
    if (tail eq null) trunk else trunk resolve tail
  
  def file = underlying.toFile

  def /(that: String) = underlying resolve that

  def /(that: Path) = underlying resolve that

  def `..` = underlying.getParent match { case null => underlying; case p => p }

  def sib(that: String) = underlying resolveSibling that

  def sib(that: Path) = underlying resolveSibling that

  def reroot(oldRoot: Path, newRoot: Path): Option[Path] =
    if (underlying startsWith oldRoot) Some(newRoot resolve oldRoot.relativize(underlying))
    else None

  def reroot(roots: (Path, Path)): Option[Path] = reroot(roots._1, roots._2)

  def prune(child: Path): Option[Path] =
    if (child startsWith underlying) Some(underlying relativize child)
    else None

  def exists = Files exists underlying

  def isDirectory = Files isDirectory underlying

  def isSymbolic = Files isSymbolicLink underlying

  def size = Files size underlying

  def safely = new PathShouldSafelyDoThis(underlying)

  def t: FileTime = Files getLastModifiedTime underlying

  def t_=(ft: FileTime): Unit =
    Files.setLastModifiedTime(underlying, ft)
  
  def mkdir() = Files createDirectory underlying

  def mkdirs() = Files createDirectories underlying

  def delete() = Files delete underlying

  def touch(): Unit =
    if (Files exists underlying) Files.setLastModifiedTime(underlying, FileTime from Instant.now)
    else Files.write(underlying, new Array[Byte](0))

  def paths =
    if (!(Files exists underlying)) PathsHelper.emptyPathArray
    else if (!(Files isDirectory underlying)) PathsHelper.emptyPathArray
    else safe{ 
      val list = Files.list(underlying)
      val ans = list.toArray(i => new Array[Path](i))
      list.close
      ans
    }.yesOr(_ => PathsHelper.emptyPathArray)

  def slurp: Ok[String, Array[String]] = safe {
    val s = Files lines underlying
    val vb = Array.newBuilder[String]
    s.forEach(vb += _)
    s.close
    vb.result
  }.mapNo(_.explain())

  def gulp: Ok[String, Array[Byte]] = safe {
    Files readAllBytes underlying
  }.mapNo(_.explain())

  /*
  // TODO -- evaluate whether it's better to use a ZipFileSystem to do this
  def unzipMap[A](selector: ZipEntry => Option[Array[Byte] => A]): Ok[String, List[A]] = safe{
    (new InputStreamShouldDoThis(Files newInputStream underlying)).unzipMap(selector).mapNo(e => s"Error while unzipping $underlying\n$e")
  }.mapNo(e => s"Could not open path $underlying\n${e.explain()}").flatten
  */

  def copyTo(to: Path): Unit =
    Files.copy(underlying, to, StandardCopyOption.REPLACE_EXISTING)

  def moveTo(to: Path): Unit =
    Files.move(underlying, to, StandardCopyOption.REPLACE_EXISTING)

  def atomicCopy(to: Path): Unit =
    val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
    to.getParent.tap{ gp => if (gp ne null) { if (!Files.exists(gp)) Files.createDirectories(gp) }}
    Files.copy(underlying, temp, StandardCopyOption.REPLACE_EXISTING)
    Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

  def atomicMove(to: Path): Unit =
    val up = to.getParent
    if (up != null) {
      if (!Files.exists(up)) Files.createDirectories(up)
    }
    if (up != null && Files.getFileStore(underlying) == Files.getFileStore(up))
      Files.move(underlying, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    else {        
      val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
      to.getParent.tap{ gp => if (gp ne null) { if (!Files.exists(gp)) Files.createDirectories(gp) }}
      Files.copy(underlying, temp, StandardCopyOption.REPLACE_EXISTING)
      Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      Files.delete(underlying)
    }

  def atomicZipCopy(to: Path, compression: Option[Int] = None, maxDirectoryDepth: Int = 10): Unit =
    val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
    to.getParent.tap{ gp => if (gp ne null) { if (!Files.exists(gp)) Files.createDirectories(gp) }}
    val zos = new ZipOutputStream(new FileOutputStream(temp.toFile))
    compression.foreach(zos.setLevel)
    if (Files.isDirectory(underlying)) {
      val base = underlying.getParent.fn{ fp => if (fp eq null) FileSystems.getDefault.getPath("") else fp }
      def recurse(current: Path, maxDepth: Int): Unit = {
        val stable = current.paths
        val (directories, files) = stable.sortBy(_.getFileName.toString).partition(x => Files.isDirectory(x))
        files.foreach{ fi =>
          val rel = base relativize fi
          val ze = new ZipEntry(rel.toString)
          ze.setLastModifiedTime(Files.getLastModifiedTime(fi))
          zos.putNextEntry(ze)
          Files.copy(fi, zos)
          zos.closeEntry
        }
        if (maxDepth > 1) directories.foreach(d => recurse(d, maxDepth-1))
      }
      recurse(underlying, maxDirectoryDepth)
    }
    else {
      val ze = new ZipEntry(underlying.getFileName.toString)
      ze.setLastModifiedTime(Files.getLastModifiedTime(underlying))
      zos.putNextEntry(ze)
      Files.copy(underlying, zos)
      zos.closeEntry
    }
    zos.close
    Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)

  def recursively = new PathsHelper.RootedRecursion(underlying, underlying)

  def recurseIn(inside: Path) =
    if (underlying startsWith inside) new PathsHelper.RootedRecursion(inside, underlying)
    else throw new IOException(s"Trying recursive operation in $inside but started outside at $underlying")
}
*/


object PathsHelper {
  val emptyPathArray = new Array[Path](0)

  val doNothingHook: Path => Unit = _ => ()

  class RootedRecursion(val root: Path, val origin: Path) {
    def delete(hook: Path => Unit = doNothingHook) = recursiveDelete(origin, root, hook)
    def atomicDelete(hook: Path => Unit = doNothingHook) = atomicRecursiveDelete(origin, root, hook)
  }

  private[PathsHelper] def recursiveDelete(f: Path, root: Path, hook: Path => Unit = _ => ()): Unit =
    if !(f startsWith root) then throw new IOException(s"Tried to delete $f but escaped root path $root")
    if Files.isDirectory(f) && !Files.isSymbolicLink(f) then
      f.paths.foreach(fi => recursiveDelete(fi, root, hook))
    hook(f)
    Files delete f

  private[PathsHelper] def atomicRecursiveDelete(f: Path, root: Path, hook: Path => Unit = _ => ()): Unit =
    if !(f startsWith root) then throw new IOException(s"Tried to delete $f but escaped root path $root")
    val name = f.getFileName.toString
    val i = name.lastIndexOf('.')
    val delext = if i > 0 then name.substring(i+1) else ""
    val delnum = 
      if !delext.startsWith("deleted") then None
      else if delext.length > "deleted".length + 8 then None
      else if delext.length > "deleted".length then
        val more = delext.substring("deleted".length)
        if !more.forall(_.isDigit) then None
        else safe{ more.toInt }.toOption
      else Some(1)
    val delname = delnum match
      case Some(n) => name.substring(0, i) + ".deleted" + (n+1).toString
      case _ => name + ".deleted"
    val del = f.resolveSibling(delname).normalize
    if Files exists del then atomicRecursiveDelete(del, root, hook)
    Files.move(f, del, StandardCopyOption.ATOMIC_MOVE)
    val drp = del.toRealPath()
    recursiveDelete(drp, drp, hook)

  opaque type Safely = Path
  object Safely {
    def apply(path: Path): kse.eio.PathsHelper.Safely = path

    extension (path: Safely)
      def underlying: Path = path

    extension (path: kse.eio.PathsHelper.Safely) {
      def exists: Boolean =
        try Files.exists(path.underlying)
        catch case e if catchable(e) => false

      def isDirectory: Boolean =
        try Files.isDirectory(path.underlying)
        catch case e if catchable(e) => false

      def isSymbolic: Boolean =
        try Files.isSymbolicLink(path.underlying)
        catch case e if catchable(e) => false

      def size: Long =
        try
          if Files.exists(path.underlying) then Files.size(path.underlying)
          else -1L
        catch case e if catchable(e) => -1L

      def real: Path =
        try
          val abs = path.underlying.toAbsolutePath()
          try
            val norm = abs.normalize()
            try
              var p = norm
              var tail: Path = null
              var found = false
              while p != null && !{ found = Files.exists(p); found } do
                tail = if tail eq null then p.getFileName else p.getFileName resolve tail
                p = p.getParent
              val trunk = if found then p.toRealPath() else p
              if tail eq null then trunk else trunk resolve tail
            catch case e if catchable(e) => norm
          catch case e if catchable(e) => abs
        catch case e if catchable(e) => path.underlying
    }  
  }
}
