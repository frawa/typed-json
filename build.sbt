import Dependencies._

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / organization := "frawa.typedjson"

ThisBuild / organizationName := "Frank Wagner"
ThisBuild / startYear := Some(2021)
ThisBuild / licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

lazy val root = (project in file("."))
  .settings(
    name := "scala-json-schema-validator",
    libraryDependencies ++= Seq(zioJson, munit % Test),
    Test / testOptions += Tests.Argument("+l", "--summary=1")
    // Test / parallelExecution := false,
    // Test / logBuffered := false
  )
