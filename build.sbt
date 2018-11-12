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

libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case _ ⇒ "1.4.2"
  }
  "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
}

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

// Optional, required for the `source` command to work
(fullClasspath in Test) ++= {
  (updateClassifiers in Test).value
    .configurations
    .find(_.configuration == Test.name)
    .get
    .modules
    .flatMap(_.artifacts)
    .collect{case (a, f) if a.classifier == Some("sources") => f}
}