import Dependencies._

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / organization := "frawa.typedjson"

lazy val root = (project in file("."))
  .settings(
    name := ".",
    libraryDependencies += zioJson,
    libraryDependencies += munit % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
