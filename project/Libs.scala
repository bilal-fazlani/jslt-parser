import sbt._

object Libs {
  private val zioOrg = "dev.zio"
  private val zioVersion = "2.0.0"

  lazy val zioParser = zioOrg %% "zio-parser" % "0.1.7"

  lazy val zioTest = zioOrg %% "zio-test" % zioVersion % Test
  lazy val zioTestJUnit = zioOrg %% "zio-test-junit" % zioVersion % Test
  lazy val zioTestSbt = zioOrg %% "zio-test-sbt" % zioVersion % Test
}
