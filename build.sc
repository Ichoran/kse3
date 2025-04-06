// This file is distributed under the BSD 3-clause license.  See file LICENSE
// Copyright 2021-2025 Rex Kerr and Calico Life Sciences, LLC


import mill._
import mill.scalalib._
import publish._


trait Base extends ScalaModule {
  def scalaVersion = "3.6.4"

  def scalacOptions = Seq(
    "-deprecation",
    "-Wconf:msg=is not declared infix:s"
  )
}


trait PublishKse3 extends PublishModule {
  def publicationName: String = ""

  override def artifactName = {
    if (publicationName.isEmpty) "kse3-" + super.artifactName()
    else publicationName
  }

  def publishVersion = "0.4.0"

  def pomSettings = PomSettings(
    description = "Kerr Scala Extensions 3, module " + artifactName(),
    organization = "com.github.ichoran",
    url = "https://github.com/ichoran/kse3",
    licenses = Seq(License.`BSD-3-Clause-Clear`),
    versionControl = VersionControl.github("ichoran", "kse3"),
    developers = Seq(
      Developer("ichoran", "Rex Kerr", "https://github.com/ichoran")
    )
  )
}


object testutilities extends Base with PublishKse3 {
  override def publicationName = "kse3-testing"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.3.1"
  )
}

trait Common extends Base {
  def extraTestDeps: Seq[ScalaModule] = Seq.empty
  def extraTestIvy: T[Agg[Dep]] = T{ Agg.empty[Dep] }

  object test extends ScalaTests with TestModule.Junit4 {
    override def ivyDeps = T{
      super.ivyDeps() ++
      Agg(
        ivy"com.lihaoyi::sourcecode:0.3.1"
      ) ++
      extraTestIvy()
    }
    def moduleDeps = Seq(testutilities) ++ extraTestDeps ++ super.moduleDeps
  }
}


object basics extends Common with PublishKse3 {}


object flow extends Common with PublishKse3 {
  def moduleDeps = Seq(basics)
  object bench extends ScalaTests with TestModule.Junit4 {}
}

object maths extends Common with PublishKse3 {
  def moduleDeps = Seq(basics)
  override def extraTestDeps = Seq(flow)
}

object eio extends Common with PublishKse3 {
  def moduleDeps = Seq(flow, maths)
  override def extraTestIvy = T { Agg(
    ivy"com.github.marschall:memoryfilesystem:2.5.0"
  ) }
}

object data extends Common with PublishKse3 {
  override def ivyDeps = T {
    super.ivyDeps() ++
    Agg(ivy"org.ojalgo:ojalgo:51.4.1")
  }
  def moduleDeps = Seq(flow, maths, eio)
}

object jsonal extends Common {
  def moduleDeps = Seq(flow, maths)
}

object all extends Common {
  override def ivyDeps = T {
    super.ivyDeps() ++
    Agg(
      ivy"org.typelevel::cats-core:2.12.0",
      ivy"com.diffplug.matsim:matfilerw:3.1.1",
      ivy"com.lihaoyi::upickle:4.1.0"
    )
  }
  def moduleDeps = Seq(basics, flow, maths, eio)
}
