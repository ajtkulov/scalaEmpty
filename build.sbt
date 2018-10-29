name := "scalaEmpty"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

parallelExecution in Test := false

libraryDependencies += "io.circe" %% "circe-core" % "0.10.0-M1"

libraryDependencies += "io.circe" %% "circe-generic" % "0.10.0-M1"

libraryDependencies += "io.circe" %% "circe-parser" % "0.10.0-M1"

libraryDependencies +=  "com.typesafe.akka" %% "akka-http"            % "10.0.10"

libraryDependencies +=  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.10"

fork := true
