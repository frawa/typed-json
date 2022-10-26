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

lazy val scalaVersion3 = "3.2.0"

import xerial.sbt.Sonatype._

// resolvers ++= Seq(Resolver.jcenterRepo, Resolver.sonatypeRepo("releases"))

lazy val sharedSettings = Seq(
  scalaVersion     := scalaVersion3,
  organization     := "io.github.frawa",
  organizationName := "Frank Wagner",
  startYear        := Some(2021),
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  description            := "A library helping type Json data with Json Schema.",
  sonatypeProjectHosting := Some(GitHubHosting("frawa", "typed-json", "agilecoderfrank@gmail.com")),
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  publishTo              := sonatypePublishToBundle.value,
  versionScheme          := Some("semver-spec")
)

lazy val sharedPlatformSettings = Seq(
  scalaVersion3
)

lazy val sharedScalacSettings = Seq(
  scalacOptions ++= {
    Seq(
      "-deprecation",
      "-feature"
      // "-version",
      // "-help",
      // "-encoding",
      // "UTF-8"
      // "-language:implicitConversions"
      // disabled during the migration
      // "-Xfatal-warnings"
    ) ++
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Seq(
            "-unchecked",
            "-Xmigration",
            "-new-syntax",
            "-indent"
            // "-Ywarn-unused",
            // "-source:future",
            // "-source:future-migration",
            // "-source:3.2-migration",
            // "-source:3.0-migration",
            // "-rewrite"
            // "-explain"
          )
        case _ =>
          Seq(
            "-Xfatal-warnings",
            "-Wunused:imports,privates,locals",
            "-Wvalue-discard"
          )
      })
  },
  ThisBuild / semanticdbEnabled := true,
  ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
)

lazy val strictScalacSettings = Seq(
  scalacOptions ++= Seq(
    // TODO Scala3 equivalent
    // "-Xlint:inaccessible",
    // "-Xlint:nonlocal-return",
    // TODO remove?
    // "-Xlint:deprecation"
    // "-language:implicitConversions",
    // "-language:higherKinds",
    // "-language:existentials",
    // "-language:postfixOps"
  )
)

lazy val sharedTestSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test,
  // Test / testOptions += Tests.Argument("+l", "-q", "--summary=0")
  // perferred to copy&paste expectations into tests:
  Test / testOptions += Tests.Argument("-q", "--summary=0")
)

lazy val root = project
  .in(file("."))
  .settings(sharedSettings)
  .settings(
    name           := "typed-json-root",
    publish / skip := true
  )
  .aggregate(parser.projectRefs: _*)
  .aggregate(folderContents.projectRefs: _*)
  .aggregate(parserZio.projectRefs: _*)
  .aggregate(macros)
  .aggregate(typedJson.projectRefs: _*)
  .aggregate(typedJsonJsExport)

lazy val parser =
  projectMatrix
    .in(file("parser"))
    .settings(
      name := "typed-json-parser"
    )
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(sharedTestSettings)
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)

lazy val folderContents =
  projectMatrix
    .in(file("folder-contents"))
    .settings(
      name := "typed-json-folder-contents"
    )
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(sharedTestSettings)
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)

lazy val parserZio =
  projectMatrix
    .in(file("parser-zio"))
    .settings(
      name := "typed-json-parser-zio"
    )
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(sharedTestSettings)
    .settings(
      libraryDependencies += "dev.zio" %%% "zio-json" % zioJsonVersion
    )
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)
    .dependsOn(parser)

lazy val parserJawn =
  projectMatrix
    .in(file("parser-jawn"))
    .settings(
      name := "typed-json-parser-jawn"
    )
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(sharedTestSettings)
    .settings(
      libraryDependencies += "org.typelevel" %%% "jawn-parser" % jawnVersion
    )
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)
    .dependsOn(parser)

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "typed-json-macros"
  )
  .settings(sharedSettings)
  .settings(sharedScalacSettings)
  .settings(sharedTestSettings)
  .dependsOn(parser.jvm(scalaVersion3))
  .dependsOn(folderContents.jvm(scalaVersion3))
  .dependsOn(parserJawn.jvm(scalaVersion3))

lazy val typedJson =
  projectMatrix
    .in(file("typed-json"))
    .settings(
      name := "typed-json"
    )
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(sharedTestSettings)
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)
    .dependsOn(parser)
    .dependsOn(parserJawn % "test")
    .dependsOn(folderContents % "test")
    .configure(p => p.dependsOn(macros))

lazy val typedJsonJsExport = project
  .in(file("typed-json-js-export"))
  .settings(
    name := "typed-json-js-export"
  )
  .enablePlugins(ScalaJSPlugin)
  .settings(sharedSettings)
  .settings(sharedScalacSettings)
  .settings(strictScalacSettings)
  .settings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
  )
  .settings(
    // TODO testing
    Test / test := {}
  )
  .dependsOn(parserJawn.js(scalaVersion3))
  .dependsOn(typedJson.js(scalaVersion3))

// sample-editor
npmCI := {
  import scala.sys.process._
  val log = streams.value.log
  Process("npm" :: "ci" :: Nil, file("./sample-editor")) ! log
}

npmRunCI := {
  val doit = (typedJsonJsExport / Compile / fastLinkJS).value

  import scala.sys.process._
  val log = streams.value.log
  Process("npm" :: "run" :: "ci" :: Nil, file("./sample-editor")) ! log
}

publishToDocs := {
  val doit = npmRunCI.value
  IO.delete(file("./docs"))
  IO.copyDirectory(file("./sample-editor/public"), file("./docs"))
}

(Compile / packageBin) := ((Compile / packageBin) dependsOn publishToDocs).value
