import Dependencies._

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "frawa"
ThisBuild / name := "json-schema-validator"
lazy val root = (project in file("."))
  .settings(
    name := ".",
    libraryDependencies += munit % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
