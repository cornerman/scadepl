name := "scadepl"

version := "0.0.1"

scalaVersion := "2.11.8"

libraryDependencies ++=
  "org.scala-lang" % "scala-compiler" % scalaVersion.value ::
  "org.scala-debugger" %% "scala-debugger-api" % "1.0.0" ::
  Nil

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  //"-Yinline", "-Yinline-warnings" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  Nil

organization := "com.github.cornerman"
