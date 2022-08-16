import sbt._

object Libs {
  lazy val zioParser = "dev.zio" %% "zio-parser" % "0.1.7"
  lazy val zioTest = "dev.zio" %% "zio-test" % "2.0.0"
  lazy val zioTestJUnit = "dev.zio" %% "zio-test-junit" % "2.0.0"
}
