import sbt._
import sbtcrossproject.CrossPlugin.autoImport._

object Dependencies {
  lazy val scalaTest = Def.setting("org.scalatest" %%% "scalatest" % "3.0.3")

  lazy val scalaMeta = Def.setting("org.scalameta" %%% "scalameta" % "2.0.0-RC1")

  lazy val fastParse = Def.setting("com.lihaoyi" %%% "fastparse" % "0.4.4")
}
