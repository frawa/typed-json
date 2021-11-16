import Dependencies._

val sharedSettings = Seq(
  ThisBuild / scalaVersion := "2.13.6",
  ThisBuild / organization := "frawa.typedjson",
  ThisBuild / organizationName := "Frank Wagner",
  ThisBuild / startYear := Some(2021),
  ThisBuild / licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  ThisBuild / scalacOptions ++= Seq(
    "-encoding",
    "utf8",
    // "-Xfatal-warnings",
    // "-deprecation"
    "-unchecked"
    // "-language:implicitConversions",
    // "-language:higherKinds",
    // "-language:existentials",
    // "-language:postfixOps"
  )
)

lazy val root = (project in file("."))
  .settings(sharedSettings)
  .settings(
    name := "scala-json-schema-validator-root"
  )
  .aggregate(parser.jvm, parser.js, macros.jvm, validator.jvm, validator.js)

lazy val parser =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("parser"))
    .settings(sharedSettings)
    .settings(
      name := "scala-json-schema-parser",
      libraryDependencies ++= Seq(zioJson)
    )

lazy val macros = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("macros"))
  .settings(sharedSettings)
  .settings(
    name := "scala-json-schema-macros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
  .dependsOn(parser)

lazy val validator =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("validator"))
    .settings(sharedSettings)
    .settings(
      name := "scala-json-schema-validator",
      libraryDependencies += munit % Test,
      Test / testOptions += Tests.Argument("+l", "-q", "--summary=0")
    )
    .jsSettings()
    .jvmSettings()
    .dependsOn(parser, macros)
