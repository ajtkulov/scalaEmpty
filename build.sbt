name := "scalaEmpty"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

parallelExecution in Test := false

fork := true

autoCompilerPlugins := true

addCompilerPlugin(
  "org.scala-lang.plugins" % "scala-continuations-plugin_2.11.8" % "1.0.2")

libraryDependencies +=
  "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.2"

scalacOptions += "-P:continuations:enable"