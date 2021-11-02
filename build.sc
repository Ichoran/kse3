import mill._, scalalib._

trait Scala3 extends ScalaModule {
  def scalaVersion = "3.1.0"
}

object flow extends Scala3 {}

object maths extends Scala3 {}

object all extends Scala3 {
  def moduleDeps = Seq(flow, maths)
}
