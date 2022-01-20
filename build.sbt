import Dependencies._

addCommandAlias("lint", "headerCheckAll;fmtCheck;fixCheck;npmAll")
addCommandAlias("lintFix", "headerCreateAll;fixFix;fmtFix")
addCommandAlias("fmtCheck", "all scalafmtCheck scalafmtSbtCheck")
addCommandAlias("fmtFix", "all scalafmt scalafmtSbt")
addCommandAlias("fixCheck", "scalafixAll --check")
addCommandAlias("fixFix", "scalafixAll")
addCommandAlias("npmAll", "npmCI;npmRunCI")

// dev convenience
addCommandAlias("testJs", "allJsJS/test")
addCommandAlias("testJvm", "allJvm/test")

lazy val npmCI    = taskKey[Unit]("npm ci")
lazy val npmRunCI = taskKey[Unit]("npm run ci")

lazy val publishToDocs = taskKey[Unit]("publish to docs/, aka GitHub Pages")

lazy val scalaVersion213 = "2.13.7"

lazy val sharedSettings = Seq(
  scalaVersion     := scalaVersion213,
  organization     := "frawa",
  organizationName := "Frank Wagner",
  startYear        := Some(2021),
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
)

lazy val sharedPlatformSettings = Seq(
  scalaVersion213
  // "2.12.10"
)

lazy val sharedScalacSettings = Seq(
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

lazy val strictScalacSettings = Seq(
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

lazy val sharedTestSettings = Seq(
  Test / testOptions += Tests.Argument("+l", "-q", "--summary=0")
)

lazy val allJvm = projectMatrix
  .aggregate(
    parser,
    parserZio,
    macros,
    typedJson
  )
  .settings(sharedSettings)
  .jvmPlatform(sharedPlatformSettings)

lazy val allJs = projectMatrix
  .aggregate(
    parser,
    parserZio,
    macros,
    typedJson
  )
  .settings(sharedSettings)
  .jsPlatform(sharedPlatformSettings)

lazy val root = project
  .in(file("."))
  .settings(sharedSettings)
  .settings(
    name    := "typed-json-root",
    publish := false
  )
  .aggregate(allJvm.projectRefs: _*)
  .aggregate(allJs.projectRefs: _*)
  .aggregate(typedJsonJsExport)

lazy val parser =
  projectMatrix
    .in(file("parser"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "typed-json-parser"
    )
    .settings(
      libraryDependencies += "org.scalameta" %%% "munit" % munitVersion % Test
    )
    .settings(sharedTestSettings)
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)

lazy val parserZio =
  projectMatrix
    .in(file("parser-zio"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "typed-json-parser-zio"
    )
    .settings(sharedTestSettings)
    .settings(
      libraryDependencies += "dev.zio"       %%% "zio-json" % zioJsonVersion,
      libraryDependencies += "org.scalameta" %%% "munit"    % munitVersion % Test
    )
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)
    .dependsOn(parser)

lazy val parserJawn =
  projectMatrix
    .in(file("parser-jawn"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "typed-json-parser-jawn"
    )
    .settings(sharedTestSettings)
    .settings(
      libraryDependencies += "org.typelevel" %%% "jawn-parser" % jawnVersion,
      libraryDependencies += "org.scalameta" %%% "munit"       % munitVersion % Test
    )
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)
    .dependsOn(parser)

lazy val macros = projectMatrix
  .in(file("macros"))
  .settings(sharedSettings)
  .settings(sharedScalacSettings)
  .settings(sharedTestSettings)
  .settings(
    name := "typed-json-macros"
  )
  .settings(
    libraryDependencies += "org.scala-lang"  % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scalameta" %%% "munit"         % munitVersion % Test
  )
  .jvmPlatform(sharedPlatformSettings)
  .jsPlatform(sharedPlatformSettings)
  .dependsOn(parser)
  .dependsOn(parserJawn)

lazy val typedJson =
  projectMatrix
    .in(file("typed-json"))
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(
      name := "typed-json"
    )
    .settings(sharedTestSettings)
    .settings(
      libraryDependencies += "org.scalameta" %%% "munit" % munitVersion % Test
    )
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)
    .dependsOn(macros)
    .dependsOn(parser)
    .dependsOn(parserJawn % "test")

lazy val typedJsonJsExport = project
  .in(file("typed-json-js-export"))
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
  .dependsOn(parserJawn.js(scalaVersion213))
  .dependsOn(typedJson.js(scalaVersion213))

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
