import Dependencies._

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / organization := "frawa.typedjson"

lazy val root = (project in file("."))
  .settings(
    name := "scala-json-schema-validator",
    libraryDependencies += zioJson,
    libraryDependencies += munit % Test,
    testFrameworks += new TestFramework("munit.Framework"),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDS")
    // Test / parallelExecution := false,
    // Test / logBuffered := false
  )
