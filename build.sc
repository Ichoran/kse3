// This file is distributed under the BSD 3-clause license.  See file LICENSE
// Copyright 2021 Rex Kerr and Calico Life Sciences, LLC


import mill._
import mill.scalalib._

trait Base extends ScalaModule {
  def scalaVersion = "3.1.0"

  def scalaOptions = T{Seq(
    "-opt"
  )}
}

object testutilities extends Base {}

trait Common extends Base {
  object test extends Tests with TestModule.Junit4 {
    def moduleDeps = Seq(testutilities) ++ super.moduleDeps
  }

  object bench extends Tests with TestModule.Junit4 {}
}

object flow extends Common {}

object maths extends Common {
  def moduleDeps = Seq(flow)
}

object jsonal extends Common {
  def moduleDeps = Seq(flow, maths)
}

object eio extends Common {
  def moduleDeps = Seq(flow, maths, jsonal)
}

object all extends Common {
  def moduleDeps = Seq(flow, maths, jsonal, eio)
}
