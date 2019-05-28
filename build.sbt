import Dependencies._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "higher-kinded-data",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += shapeless,
    libraryDependencies += catsCore,
    libraryDependencies += catsKernel,
    libraryDependencies += catsMacros,
    libraryDependencies += kittens
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
