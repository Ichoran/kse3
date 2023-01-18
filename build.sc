// This file is distributed under the BSD 3-clause license.  See file LICENSE
// Copyright 2021-2023 Rex Kerr and Calico Life Sciences, LLC


import mill._
import mill.scalalib._
import publish._


trait Base extends ScalaModule {
  def scalaVersion = "3.1.3"

  def scalaOptions = T{Seq(
    "-opt",
    "-deprecation"
  )}
}


trait PublishKse3 extends PublishModule {
  def publicationName: String = ""

  override def artifactName = {
    if (publicationName.isEmpty) "kse3-" + super.artifactName()
    else publicationName
  }

  def publishVersion = "0.0.3"

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


object testutilities extends Base {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.2.7"
  )
}

trait Common extends Base {
  def extraTestDeps: Seq[ScalaModule] = Seq.empty

  object test extends Tests with TestModule.Junit4 {
    override def ivyDeps = T{
      super.ivyDeps() ++ Agg(
        ivy"com.lihaoyi::sourcecode:0.2.7"
      )
    }
    def moduleDeps = Seq(testutilities) ++ extraTestDeps ++ super.moduleDeps
  }

  object bench extends Tests with TestModule.Junit4 {}
}

object flow extends Common with PublishKse3 {}

object maths extends Common {
  override def extraTestDeps = Seq(flow)
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
