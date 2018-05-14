package main

import model._
import org.joda.time.Instant
import play.api.libs.json.Json
import request.Request

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val json = scala.io.Source.fromFile("1.json").getLines().toList.mkString("")
    val js = Json.parse(json)
    val res = ModelReader.readJson(js)
    println(res.take(5).mkString("\n\n"))

    println(Request.getModel(new Instant()).mkString("\n\n"))

  }
}