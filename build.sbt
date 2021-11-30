import Dependencies._

addCommandAlias("lint", "headerCheckAll;fmtCheck;fixCheck")
addCommandAlias("lintFix", "headerCreateAll;fixFix;fmtFix")
addCommandAlias("fmtCheck", "all scalafmtCheck scalafmtSbtCheck")
addCommandAlias("fmtFix", "all scalafmt scalafmtSbt")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fixFix", "scalafixAll")

val sharedSettings = Seq(
  scalaVersion     := "2.13.6",
  organization     := "frawa.typedjson",
  organizationName := "Frank Wagner",
  startYear        := Some(2021),
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
)

val sharedScalacSettings = Seq(
  scalacOptions ++= Seq(
    "-Wunused:imports",
    "-Xfatal-warnings",
    "-deprecation",
    "-unchecked"
  ),
  semanticdbEnabled                      := true,
  semanticdbVersion                      := scalafixSemanticdb.revision,
  ThisBuild / scalafixScalaBinaryVersion := "2.13"
)

val strictScalacSettings = Seq(
  scalacOptions ++= Seq(
    "-Xlint:inaccessible",
    "-Xlint:nonlocal-return",
    "-Xlint:deprecation"
    // "-language:implicitConversions",
    // "-language:higherKinds",
    // "-language:existentials",
    // "-language:postfixOps"
  )
)

lazy val root = (project in file("."))
  .settings(sharedSettings)
  .settings(
    name    := "scala-json-schema-validator-root",
    publish := false
  )
  .aggregate(parser.jvm, parser.js, macros.jvm, validator.jvm, validator.js)

lazy val parser =
  crossProject(JVMPlatform, JSPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("parser"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "scala-json-schema-parser"
    )

lazy val parserZio =
  crossProject(JVMPlatform, JSPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("parser-zio"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "scala-json-schema-parser-zio"
    )
    .jvmSettings(
      libraryDependencies += "dev.zio"       %% "zio-json" % zioJsonVersion,
      libraryDependencies += "org.scalameta" %% "munit"    % munitVersion % Test
    )
    .jsSettings(
      libraryDependencies += "dev.zio"       %%% "zio-json" % zioJsonVersion,
      libraryDependencies += "org.scalameta" %%% "munit"    % munitVersion % Test
    )
    .dependsOn(parser)

lazy val macros = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("macros"))
  .settings(sharedSettings)
  .settings(sharedScalacSettings)
  .settings(
    name                                   := "scala-json-schema-macros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
  .dependsOn(parser, parserZio)

lazy val validator =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("validator"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "scala-json-schema-validator"
    )
    .settings(
      Test / testOptions += Tests.Argument("+l", "-q", "--summary=0")
    )
    .jvmSettings(
      libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
    )
    .jsSettings(
      libraryDependencies += "org.scalameta" %%% "munit" % munitVersion % Test
    )
    .dependsOn(parser, macros)
    .dependsOn(parserZio % "test")

lazy val validatorJS = validator.js
