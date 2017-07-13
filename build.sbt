name := "scalaEmpty"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

parallelExecution in Test := false

fork := true

val kafkaVersion = "0.10.0.0"

libraryDependencies += "org.apache.kafka" % "kafka-clients" % kafkaVersion

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.5"

libraryDependencies += "org.slf4j" % "log4j-over-slf4j" % "1.7.16"

libraryDependencies += "com.typesafe.akka" %% "akka-stream-kafka" % "0.16"

libraryDependencies += "joda-time" % "joda-time" % "2.9.7"