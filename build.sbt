name := "scalaEmpty"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.google.guava" % "guava" % "28.1-jre"


parallelExecution in Test := false

fork := true
