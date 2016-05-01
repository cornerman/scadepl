name := "scadepl"

version := "0.0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-debugger" %% "scala-debugger-api" % "1.0.0",
  "ch.qos.logback"      %  "logback-classic"      % "1.0.7"
)

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-explaintypes",
  "-feature",
  //"-Yinline", "-Yinline-warnings",
  "-language:_",
  "-Xlint:_",
  "-Ywarn-unused"
)

organization := "com.github.cornerman"
