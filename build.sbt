name := "scala-debug-repl"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value
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
//,"-Xdisable-assertions", "-optimize"
)
