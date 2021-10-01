import Dependencies._

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / organization := "frawa.typedjson"

lazy val root = (project in file("."))
  .settings(
    name := "scala-json-schema-validator",
    libraryDependencies ++= Seq(zioJson, munit % Test),
    Test / testOptions += Tests.Argument("+l", "--summary=1")
    // Test / parallelExecution := false,
    // Test / logBuffered := false
  )
