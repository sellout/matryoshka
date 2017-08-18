import org.scalajs.sbtplugin.ScalaJSCrossVersion
import scoverage._
import sbt._
import Keys._
import slamdata.SbtSlamData.transferPublishAndTagResources

lazy val catsVersion = "1.1.0"
lazy val monocleVersion = "1.5.1-cats"
lazy val specs2Version = "4.0.0"

lazy val standardSettings = commonBuildSettings ++ Seq(
  logBuffered in Compile := false,
  logBuffered in Test := false,
  updateOptions := updateOptions.value.withCachedResolution(true),
  exportJars := true,
  organization := "org.technomadic",
  ScoverageKeys.coverageHighlighting := true,
  scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits"),
  wartremoverWarnings in (Compile, compile) --= Seq(
    Wart.PublicInference,    // TODO: enable incrementally — currently results in many errors
    Wart.ImplicitParameter), // see wartremover/wartremover#350 & #351

  libraryDependencies ++= Seq(
    "com.slamdata"               %% "slamdata-predef" % "0.0.2",
    "org.typelevel"              %% "cats-core"       % catsVersion    % "compile, test",
    "org.typelevel"              %% "cats-free"       % catsVersion    % "compile, test",
    "org.typelevel"              %% "cats-testkit"    % catsVersion    % "compile, test",
    // "org.typelevel"              %% "kittens"         % "1.0.0-RC1"    % "compile, test",
    "com.github.julien-truffaut" %% "monocle-core"    % monocleVersion % "compile, test",
    // "com.github.julien-truffaut" %% "newts-core"      % "0.3.1"   % "compile, test",
    "com.github.mpilquist"       %% "simulacrum"      % "0.10.0"       % "compile, test"))

lazy val publishSettings = commonPublishSettings ++ Seq(
  organizationName := "Greg Pfeil",
  organizationHomepage := Some(url("http://technomadic.org")),
  homepage := Some(url("https://github.com/sellout/turtles")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/sellout/turtles"),
      "scm:git@github.com:sellout/turtles.git")))

lazy val root = Project("root", file("."))
  .settings(name := "turtles")
  .settings(standardSettings ++ noPublishSettings: _*)
  .settings(transferPublishAndTagResources)
  .settings(console := (console in replJVM).value)
  .aggregate(
    coreJS,  scalacheckJS,  testsJS,
    coreJVM, scalacheckJVM, testsJVM,
    docs
  )
  .enablePlugins(AutomateHeaderPlugin)

lazy val core = crossProject.in(file("core"))
  .settings(name := "turtles-core")
  .settings(standardSettings ++ publishSettings: _*)
  .enablePlugins(AutomateHeaderPlugin)

lazy val scalacheck = crossProject
  .dependsOn(core)
  .settings(name := "turtles-scalacheck")
  .settings(standardSettings ++ publishSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scalacheck"      %% "scalacheck"      % "1.14.0",
    "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0")
  )
  .enablePlugins(AutomateHeaderPlugin)

lazy val tests = crossProject
  .settings(name := "turtles-tests")
  .dependsOn(core, scalacheck)
  .settings(standardSettings ++ noPublishSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "io.github.amrhassan"        %% "scalacheck-cats"   % "0.4.0" % Test,
    "org.typelevel"              %% "cats-laws"         % catsVersion % Test,
    "com.github.julien-truffaut" %% "monocle-law"       % monocleVersion % Test,
    "org.specs2"                 %% "specs2-core"       % specs2Version % Test,
    "org.specs2"                 %% "specs2-scalacheck" % specs2Version % Test))
  .enablePlugins(AutomateHeaderPlugin)

lazy val docs = project
  .settings(name := "turtles-docs")
  .dependsOn(coreJVM)
  .settings(standardSettings ++ noPublishSettings: _*)
  .settings(scalacOptions in Tut --= Seq("-Yno-imports", "-Ywarn-unused-import"))
  .enablePlugins(MicrositesPlugin)
  .settings(
    micrositeName             := "Turtles",
    micrositeDescription      := "Generalized folds, unfolds, and traversals for fixed point data structures in Scala, using Cats.",
    micrositeAuthor           := "Greg Pfeil",
    micrositeGithubOwner      := "sellout",
    micrositeGithubRepo       := "turtles",
    micrositeBaseUrl          := "/turtles",
    micrositeDocumentationUrl := "/turtles/docs/01-Getting-Started.html",
    micrositeHighlightTheme   := "color-brewer")

/** A project just for the console.
  * Applies only the settings necessary for that purpose.
  */
lazy val repl = crossProject dependsOn (tests % "compile->test") settings standardSettings settings (
  console := (console in Test).value,
  scalacOptions --= Seq("-Yno-imports", "-Ywarn-unused-import"),
  initialCommands in console += """
    |import turtles._
    |import turtles.data._
    |import turtles.implicits._
    |import turtles.patterns._
    |import cats._
    |import cats.implicits._
  """.stripMargin.trim
)

lazy val replJVM = repl.jvm
lazy val coreJS  = core.js
lazy val coreJVM = core.jvm
lazy val scalacheckJS  = scalacheck.js
lazy val scalacheckJVM = scalacheck.jvm
lazy val testsJS  = tests.js
lazy val testsJVM = tests.jvm
