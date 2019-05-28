import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "2.0.0-M1"
  lazy val catsKernel = "org.typelevel" %% "cats-kernel" % "2.0.0-M1"
  lazy val catsMacros = "org.typelevel" %% "cats-macros" % "2.0.0-M1"
  lazy val kittens = "org.typelevel" %% "kittens" % "1.1.0"
}
