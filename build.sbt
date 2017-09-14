import Dependencies._

lazy val root =
  (project in file("."))
    .settings(
      inThisBuild(List(
        organization := "co.technius",
        scalaVersion := "2.11.11",
        version      := "0.1.0-SNAPSHOT"
      ))
    )
  .aggregate(siggyJVM)

lazy val siggy =
  crossProject(JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .settings(
      name := "siggy",
      libraryDependencies ++= Seq(
        scalaTest.value % Test,
        scalaMeta.value,
        fastParse.value
      )
    )

lazy val siggyJVM = siggy.jvm
lazy val siggyNative = siggy.native // doesn't compile yet -- no scalameta support
