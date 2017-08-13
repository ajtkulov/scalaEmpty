name := "scalaEmpty"

version := "0.1"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

parallelExecution in Test := false

fork := true

libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0"

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full)