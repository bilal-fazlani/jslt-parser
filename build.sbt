val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "jslt-parser",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      Libs.zioParser,
      Libs.zioTest % Test,
      Libs.zioTestJUnit % Test
    )
  )
