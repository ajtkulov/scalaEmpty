package main

import model.ModelReader
import play.api.libs.json.Json

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val json = scala.io.Source.fromFile("1.json").getLines().toList.mkString("")
    val js = Json.parse(json)
    ModelReader.readJson(js)
  }
}