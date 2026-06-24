// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.thyme

// Standalone demo of Parsley's shutdown-hook auto-print — NOT part of the JUnit suite.
// Run:  mill thyme.test.runMain kse.test.thyme.ParsleyDemo
//
// It races two implementations many times and *never closes* the Parsley; the Tidy.Later backstop
// fires at JVM shutdown and prints the report.  This is the intended companion-object usage.

import kse.basics.{given, *}
import kse.thyme.Parsley

object ParsleyDemo {
  // One line is all it takes; the report prints itself when the JVM exits.
  val parsley = Parsley.onClose(println)

  def busywork(n: Int): Long =
    var s = 0L
    var i = 0
    while i < n do { s += i.toLong * (i ^ 0x5DEECE66L); i += 1 }
    s

  def main(args: Array[String]): Unit =
    var acc = 0L
    var i = 0
    while i < 20000 do
      acc += parsley.timeOff("busy-2000", "busy-1000"){ busywork(2000) }{ busywork(1000) }
      i += 1
    println(s"main finished (acc=$acc); Parsley was never closed — it prints at shutdown.")
}
