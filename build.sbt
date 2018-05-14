
name := "scalaEmpty"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.5.16"

libraryDependencies += "joda-time" % "joda-time" % "2.9.7"

parallelExecution in Test := false

fork := true
