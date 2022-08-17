val scala3Version = "3.1.3"

ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "com.bilal-fazlani"
ThisBuild / organizationName := "Bilal Fazlani"
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/bilal-fazlani/jslt-parser"),
    "https://github.com/bilal-fazlani/jslt-parser.git"
  )
)
ThisBuild / developers := List(
  Developer(
    "bilal-fazlani",
    "Bilal Fazlani",
    "bilal.m.fazlani@gmail.com",
    url("https://bilal-fazlani.com")
  )
)
ThisBuild / licenses := List(
  "MIT License" -> url(
    "https://github.com/bilal-fazlani/jslt-parser/blob/main/LICENSE"
  )
)
ThisBuild / homepage := Some(
  url("https://github.com/bilal-fazlani/jslt-parser")
)

//ThisBuild / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val root = project
  .in(file("."))
  .settings(
    name := "jslt-parser",
    moduleName := "jslt-parser",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      Libs.zioParser,
      Libs.zioTest,
      Libs.zioTestJUnit,
      Libs.zioTestSbt
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
