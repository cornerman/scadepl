name := "scadepl"

version := "0.1"

val scalaV = "2.11.8"

val scalacOpts = Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-explaintypes",
  "-feature",
  //"-Yinline", "-Yinline-warnings",
  "-language:_",
  "-Xlint:_",
  "-Ywarn-unused"
//,"-Xdisable-assertions", "-optimize"
)

lazy val main = (project in file(".")).settings(
  scalaVersion := scalaV,
  scalacOptions ++= scalacOpts,
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-debugger" %% "scala-debugger-api" % "1.0.0",
    "ch.qos.logback"      %  "logback-classic"      % "1.0.7"
  ),
  fork := true,
  connectInput in run := true, // Connects stdin to sbt during forked runs
  outputStrategy := Some(StdoutOutput) // Get rid of output prefix
).dependsOn(macros)

lazy val macros = (project in file("macros")).settings(
  scalaVersion := scalaV,
  scalacOptions ++= scalacOpts,
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )
)

