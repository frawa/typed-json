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

lazy val scalaVersion3 = "3.4.1"

import xerial.sbt.Sonatype._

ThisBuild / resolvers += "Sonatype OSS Releases" at "https://s01.oss.sonatype.org/content/repositories/releases"

lazy val sharedSettings = Seq(
  scalaVersion     := scalaVersion3,
  organization     := "io.github.frawa",
  organizationName := "Frank Wagner",
  startYear        := Some(2021),
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  description            := "A library helping type Json data with Json Schema.",
  sonatypeProjectHosting := Some(GitHubHosting("frawa", "typed-json", "agilecoderfrank@gmail.com")),
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  sonatypeRepository     := "https://s01.oss.sonatype.org/service/local",
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
            "-indent",
            "-Wunused:linted"
            // "-source:future",
            // "-source:future-migration",
            // "-source:3.2-migration",
            // "-source:3.0-migration",
            // "-rewrite"
            // "-explain"
          )
        case _ =>
          Seq(
            // "-Xfatal-warnings",
            "-Wunused:imports,privates,locals",
            "-Wvalue-discard"
          )
      })
  },
  ThisBuild / semanticdbEnabled := true
  // ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
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
  libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M11" % Test,
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
  .aggregate(parserJawn.projectRefs: _*)
  .aggregate(parserZio.projectRefs: _*)
  .aggregate(macros)
  .aggregate(formats.projectRefs: _*)
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
      libraryDependencies += "dev.zio" %%% "zio-json" % "0.6.2"
    )
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings)
    .dependsOn(parser)

// see https://www.scala-js.org/doc/semantics.html
// from https://github.com/typelevel/jawn/blob/v1.4.0/build.sbt#L25
// fixes parsing blank or incomplete strings
lazy val jsSettingsJawn = Seq(scalaJSLinkerConfig ~= {
  _.withSemantics(
    _.withArrayIndexOutOfBounds(org.scalajs.linker.interface.CheckedBehavior.Compliant)
      .withStringIndexOutOfBounds(org.scalajs.linker.interface.CheckedBehavior.Compliant)
  )
})

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
      libraryDependencies += "org.typelevel" %%% "jawn-parser" % "1.5.1"
    )
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(sharedPlatformSettings, jsSettingsJawn)
    .dependsOn(parser)

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "typed-json-macros"
  )
  .settings(sharedSettings)
  .settings(sharedScalacSettings)
  .settings(sharedTestSettings)
  .settings(
    libraryDependencies += "io.github.frawa" %%% "inline-files" % "0.7.0"
  )
  .dependsOn(parser.jvm(scalaVersion3))
  .dependsOn(parserJawn.jvm(scalaVersion3))

lazy val ESVersion = org.scalajs.linker.interface.ESVersion
lazy val jsSettingsES2018 = Seq(
  // fixes regex error: Look-behind group is not supported because it requires RegExp features of ECMAScript 2018.
  scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES2018)) }
  // jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(
  //   org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--stack-size=2013"))
  // )
)

lazy val jsSettingsESModule = Seq(
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
)

lazy val formats = projectMatrix
  .in(file("formats"))
  .settings(
    name := "typed-json-formats"
  )
  .settings(sharedSettings)
  .settings(sharedScalacSettings)
  .settings(strictScalacSettings)
  .settings(sharedTestSettings)
  .settings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0"
  )
  .jvmPlatform(sharedPlatformSettings)
  .jsPlatform(
    sharedPlatformSettings,
    Seq(),
    configure = { p =>
      p.enablePlugins(ScalaJSBundlerPlugin)
        .settings(jsSettingsES2018)
        .settings(
          Compile / npmDependencies += "punycode" -> "2.3.0"
        )
    }
  )
  .dependsOn(parser) // TODO just for Pointer

lazy val typedJson =
  projectMatrix
    .in(file("typed-json"))
    .settings(
      name := "typed-json"
    )
    .settings(
      libraryDependencies += "io.github.frawa" %%% "inline-files" % "0.7.0" % Test
    )
    .settings(
      unmanagedSources / excludeFilter := "*/suggestion/*" || "*/validation/*"
    )
    .settings(sharedSettings)
    .settings(sharedScalacSettings)
    .settings(strictScalacSettings)
    .settings(sharedTestSettings)
    .jvmPlatform(sharedPlatformSettings)
    .jsPlatform(
      sharedPlatformSettings,
      Seq(),
      configure = { p =>
        p.enablePlugins(ScalaJSBundlerPlugin)
          .settings(jsSettingsES2018)
      }
    )
    .dependsOn(parser)
    .dependsOn(formats)
    .configure(p => p.dependsOn(macros))
    .dependsOn(parserJawn % "test")

lazy val typedJsonJsExport = project
  .in(file("typed-json-js-export"))
  .settings(
    name := "typed-json-js-export"
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
  .settings(sharedSettings)
  .settings(sharedScalacSettings)
  .settings(strictScalacSettings)
  .settings(jsSettingsJawn)
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
  val doit = (typedJsonJsExport / Compile / fastOptJS / webpack).value

  import scala.sys.process._
  val log = streams.value.log
  Process("npm" :: "run" :: "ci" :: Nil, file("./sample-editor")) ! log
}

publishToDocs := {
  val doit = npmRunCI.value
  IO.delete(file("./docs"))
  IO.copyDirectory(file("./sample-editor/dist"), file("./docs"))
}

(Compile / packageBin) := ((Compile / packageBin) dependsOn publishToDocs).value
