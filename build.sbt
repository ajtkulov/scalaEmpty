
name := "scalaEmpty"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.5.16"

libraryDependencies += "joda-time" % "joda-time" % "2.9.7"

parallelExecution in Test := false

libraryDependencies += "info.mukel" %% "telegrambot4s" % "3.0.14"

libraryDependencies += "org.slf4j" % "log4j-over-slf4j" % "1.7.16"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.1"

fork := true
