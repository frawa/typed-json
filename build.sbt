import Dependencies._

addCommandAlias("lint", "headerCheckAll;fmtCheck;fixCheck;npmAll")
addCommandAlias("lintFix", "headerCreateAll;fixFix;fmtFix")
addCommandAlias("fmtCheck", "all scalafmtCheck scalafmtSbtCheck")
addCommandAlias("fmtFix", "all scalafmt scalafmtSbt")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fixFix", "scalafixAll")
addCommandAlias("npmAll", "npmCI;npmRunCI")

lazy val npmCI    = taskKey[Unit]("npm ci")
lazy val npmRunCI = taskKey[Unit]("npm run ci")

lazy val publishToDocs = taskKey[Unit]("publish to docs/, aka GitHub Pages")

val sharedSettings = Seq(
  scalaVersion     := "2.13.7",
  organization     := "frawa",
  organizationName := "Frank Wagner",
  startYear        := Some(2021),
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
)

val sharedScalacSettings = Seq(
  scalacOptions ++= Seq(
    "-Wunused:imports",
    "-Xfatal-warnings",
    "-feature",
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

val sharedTestSettings = Seq(
  Test / testOptions += Tests.Argument("+l", "-q", "--summary=0")
)

lazy val root = (project in file("."))
  .settings(sharedSettings)
  .settings(
    name    := "typed-json-root",
    publish := false
  )
  .aggregate(
    parser.jvm,
    parser.js,
    parserZio.jvm,
    parserZio.js,
    parserJawn.jvm,
    parserJawn.js,
    macros.jvm,
    typedJson.jvm,
    typedJson.js,
    typedJsonJsExport
  )

lazy val parser =
  crossProject(JVMPlatform, JSPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("parser"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "typed-json-parser"
    )
    .settings(sharedTestSettings)
    .jvmSettings(
      libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
    )
    .jsSettings(
      libraryDependencies += "org.scalameta" %%% "munit" % munitVersion % Test
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
      name := "typed-json-parser-zio"
    )
    .settings(sharedTestSettings)
    .jvmSettings(
      libraryDependencies += "dev.zio"       %% "zio-json" % zioJsonVersion,
      libraryDependencies += "org.scalameta" %% "munit"    % munitVersion % Test
    )
    .jsSettings(
      libraryDependencies += "dev.zio"       %%% "zio-json" % zioJsonVersion,
      libraryDependencies += "org.scalameta" %%% "munit"    % munitVersion % Test
    )
    .dependsOn(parser)

lazy val parserJawn =
  crossProject(JVMPlatform, JSPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("parser-jawn"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "typed-json-parser-jawn"
    )
    .settings(sharedTestSettings)
    .jvmSettings(
      libraryDependencies += "org.typelevel" %% "jawn-parser" % jawnVersion,
      libraryDependencies += "org.scalameta" %% "munit"       % munitVersion % Test
    )
    .jsSettings(
      libraryDependencies += "org.typelevel" %%% "jawn-parser" % jawnVersion,
      libraryDependencies += "org.scalameta" %%% "munit"       % munitVersion % Test
    )
    .dependsOn(parser)

lazy val macros = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("macros"))
  .settings(sharedSettings)
  .settings(sharedScalacSettings)
  .settings(
    name                                   := "typed-json-macros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
  .dependsOn(parser, parserJawn)

lazy val typedJson =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("typed-json"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "typed-json"
    )
    .settings(sharedTestSettings)
    .jvmSettings(
      libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
    )
    .jsSettings(
      libraryDependencies += "org.scalameta" %%% "munit" % munitVersion % Test
    )
    .dependsOn(macros)
    .dependsOn(parser)
    .dependsOn(parserJawn % "test")

lazy val typedJsonJS = typedJson.js

lazy val typedJsonJsExport = (project in file("typed-json-js-export"))
  .enablePlugins(ScalaJSPlugin)
  .settings(sharedSettings)
  .settings(sharedScalacSettings)
  .settings(strictScalacSettings)
  .settings(
    name := "typed-json-js-export"
  )
  .settings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
  )
  .settings(
    // TODO testing
    Test / test := {}
  )
  .dependsOn(parserJawn.js)
  .dependsOn(typedJson.js)

// sample-editor
npmCI := {
  import scala.sys.process._
  val log = streams.value.log
  Process("npm" :: "ci" :: Nil, file("./sample-editor")) ! log
}

npmRunCI := {
  (typedJsonJsExport / Compile / fastLinkJS).value

  import scala.sys.process._
  val log = streams.value.log
  Process("npm" :: "run" :: "ci" :: Nil, file("./sample-editor")) ! log
}

publishToDocs := {
  npmRunCI.value
  IO.delete(file("./docs"))
  IO.copyDirectory(file("./sample-editor/public"), file("./docs"))
}

(Compile / packageBin) := ((Compile / packageBin) dependsOn publishToDocs).value
