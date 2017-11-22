package model

import play.api.libs.json.{JsArray, JsObject, JsValue}

case class Coor(x: BigDecimal, y: BigDecimal) {}

case class Poly(values: List[Coor])

case class Model(coordinates: Array[Array[Array[BigDecimal]]])

object ModelReader {
  def readJson(jsValue: JsValue): Unit = {

    val obj: JsObject = jsValue.as[JsObject]

    val obj1 = obj.fields.take(1).head._2

    val z = (obj1 \ "features").as[JsArray]

    z.value.map(readGeo(_))
  }

  def toCoor(values: List[BigDecimal]): Coor = {
    Coor(values(0), values(1))
  }

  def toPoly(values: List[List[BigDecimal]]): Poly = {
    Poly(values.map(toCoor))
  }

  def readGeo(jsValue: JsValue): Unit = {
    val z = (jsValue \ "geometry" \ "coordinates").as[List[List[List[BigDecimal]]]]

    val polys = z.map(x => toPoly(x))
  }
}
