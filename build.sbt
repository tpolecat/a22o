
// Global Settings
lazy val commonSettings = Seq(

  // Publishing
  organization := "org.tpolecat",
  licenses    ++= Seq(("MIT", url("http://opensource.org/licenses/MIT"))),
  homepage     := Some(url("https://github.com/tpolecat/a22o")),
  developers   := List(
    Developer("tpolecat", "Rob Norris", "rob_norris@mac.com", url("http://www.tpolecat.org"))
  ),

  // Headers
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2020 by Rob Norris
       |This software is licensed under the MIT License (MIT).
       |For more information see LICENSE or https://opensource.org/licenses/MIT
       |""".stripMargin
    )
  ),

  // Compilation
  scalaVersion := "2.13.1",
  Compile / doc / scalacOptions --= Seq("-Xfatal-warnings"),
  Compile / doc / scalacOptions ++= Seq(
    "-groups",
    "-implicits",
    "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
    "-doc-source-url", "https://github.com/tpolecat/a22o/blob/v" + version.value + "€{FILE_PATH}.scala",
  ),

  // Let's use MUnit for all tests, and allow Cats in test as well.
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit"            % "0.7.6" % Test,
    "org.scalameta" %% "munit-scalacheck" % "0.7.6" % Test,
    "org.typelevel" %% "cats-core"        % "2.1.1" % Test,
  ),
  testFrameworks += new TestFramework("munit.Framework"),

  // don't publish by default
  publish / skip := true,

)

lazy val a22o = project
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .dependsOn(core, bench, allocation, gen)
  .aggregate(core, bench, allocation, gen)

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(
    name := "a22o-core",
    description := "Like atto, but faster.",
    publish / skip := false,
    scalacOptions -= "-Xfatal-warnings",
    scalacOptions ++= Seq(
      "-opt:l:inline",
      "-opt-inline-from:<sources>",
      "-opt:l:method",
      "-opt-warnings",
    ),
    libraryDependencies += "org.typelevel" %% "cats-free" % "2.1.1",
  )

lazy val gen = project
  .in(file("modules/gen"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)

lazy val bench = project
  .in(file("modules/bench"))
  .enablePlugins(AutomateHeaderPlugin)
  .enablePlugins(JmhPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings {
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "atto-core" % "0.8.0",
      "com.lihaoyi"  %% "fastparse" % "2.2.2",
    )
  }

// the allocation project needs to look up the path to one of its dependneci
val allocationInstrumentationModule = "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "3.1.0"
val allocationInstrumentationJarfile = taskKey[File]("Path to the allocation instrumentation jarfile.")

lazy val allocation = project
  .in(file("modules/allocation"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    allocationInstrumentationJarfile :=
      (dependencyClasspath in Test).value
        .find(_.metadata.get(moduleID.key) == Some(allocationInstrumentationModule % "default"))
        .fold(sys.error("Can't find allocation instrumentation jarfile."))(_.data),
    libraryDependencies += allocationInstrumentationModule % Test,
    Test / parallelExecution := false,
    Test / fork := true,
    Test / javaOptions ++= Seq(s"-javaagent:${allocationInstrumentationJarfile.value}"),
  )

