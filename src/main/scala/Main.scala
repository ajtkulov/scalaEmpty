package main

import model.{Coor, Geometry, ModelReader, Poly}
import play.api.libs.json.Json

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val json = scala.io.Source.fromFile("1.json").getLines().toList.mkString("")
    val js = Json.parse(json)
    ModelReader.readJson(js)

    val p = Poly(List(
      Coor(0, 0),
      Coor(1, 0),
      Coor(1, 1),
      Coor(0, 1)
    ))

    val z = Geometry.sq(Coor(0, 0), p)
    println(z)
  }
}