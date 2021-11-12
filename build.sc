// This file is distributed under the BSD 3-clause license.  See file LICENSE
// Copyright 2021 Rex Kerr and Calico Life Sciences, LLC


import mill._, scalalib._

trait Scala3 extends ScalaModule {
  def scalaVersion = "3.1.0"
}

object flow extends Scala3 {}

object maths extends Scala3 {
  def moduleDeps = Seq(flow)
}

object all extends Scala3 {
  def moduleDeps = Seq(flow, maths)
}
