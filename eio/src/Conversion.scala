// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020, 2021 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio

import java.io._
import java.nio._
import java.nio.file._

extension (underlying: Array[Byte]) {
  def utf8 = new String(underlying, java.nio.charset.StandardCharsets.UTF_8)
  def ascii = new String(underlying, java.nio.charset.StandardCharsets.US_ASCII)
  def rawString = new String(underlying, java.nio.charset.StandardCharsets.ISO_8859_1)
  def iso8859_1 = new String(underlying, java.nio.charset.StandardCharsets.ISO_8859_1)
  def buffer = ByteBuffer.wrap(underlying)
  def input = new ByteArrayInputStream(underlying)
}

extension (underlying: java.util.zip.ZipEntry) {
  def fixedName =
    val n = underlying.getName
    val i = n.indexOf('/')
    val j = n.indexOf('\\')
    if (i < 0 && j > 0) n.replace('\\', '/') else n
  
  def nameAsFile =
    val n = underlying.fixedName
    new File(if (File.separatorChar == '/') n else n.replace('/',File.separatorChar))
  
  def nameAsPath =
    val n = underlying.fixedName
    val fs = FileSystems.getDefault
    fs.getPath(if (fs.getSeparator == "/") n else n.replace("/", fs.getSeparator))
}
