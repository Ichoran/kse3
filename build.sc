import mill._, scalalib._

trait Scala3 extends ScalaModule {
  def scalaVersion = "3.0.2"
}

object flow extends Scala3 {}

object all extends Scala3 {
  def moduleDeps = Seq(flow)
}
