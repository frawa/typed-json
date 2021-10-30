import Dependencies._

ThisBuild / scalaVersion := "2.13.6"
ThisBuild / organization := "frawa.typedjson"

ThisBuild / organizationName := "Frank Wagner"
ThisBuild / startYear := Some(2021)
ThisBuild / licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

lazy val root = (project in file("."))
  .settings(
    name := "scala-json-schema-validator-root"
  )
  .aggregate(parser, macros, validator)

lazy val parser = (project in file("parser"))
  .settings(
    name := "scala-json-schema-parser",
    libraryDependencies ++= Seq(zioJson)
  )

lazy val macros = (project in file("macros"))
  .settings(
    name := "scala-json-schema-macros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val validator = (project in file("validator"))
  .dependsOn(parser, macros)
  .settings(
    name := "scala-json-schema-validator",
    libraryDependencies += munit % Test,
    Test / testOptions += Tests.Argument("+l", "--summary=1")
    // Test / parallelExecution := false,
    // Test / logBuffered := false
  )
