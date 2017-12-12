package model

import play.api.libs.json.{JsArray, JsObject, JsValue}

import scala.util.{Random, Try}

case class Coor(x: BigDecimal, y: BigDecimal) {
  def -->(dest: Coor): Coor = {
    Coor(dest.x - x, dest.y - y)
  }

}

case class Poly(values: List[Coor]) {
  def pairs: List[(Coor, Coor)] = {
    values.zip(values.drop(1) :+ values.head)
  }
}

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


object Geometry {
  lazy val rand = new Random(1)

  def vectorProd(x: Coor, y: Coor): BigDecimal = {
    x.x * y.y - x.y * y.x
  }

  def sq(x: Coor, vect: (Coor, Coor)): BigDecimal = {
    vectorProd(x --> vect._1, x --> vect._2)
  }

  def sq(c: Coor, p: Poly): BigDecimal = {
    p.pairs.map(x => sq(c, x)).sum
  }

  def det(x: Coor, y: Coor): BigDecimal = {
    x.x * y.y - x.y * y.x
  }

  /**
    *
    * @param p1 line
    * @param p2 line
    * @param b  beam start
    * @param d  beam direction
    * @return does line (p1-p2) intersect with beam (b, d)
    */
  def intersect(p1: Coor, p2: Coor, b: Coor, d: Coor): Boolean = {
    Try {
      val dd = det(Coor(p2.x - p1.x, -d.x), Coor(p2.y - p1.y, -d.y))
      val d1 = det(Coor(b.x - p1.x, -d.x), Coor(b.y - p1.y, -d.y))
      val d2 = det(Coor(p2.x - p1.x, b.x - p1.x), Coor(p2.y - p1.y, b.y - p1.y))

      val t1 = d1 / dd
      val t2 = d2 / dd

      t1 >= 0 && t1 <= 1 && t2 >= 0
    }.getOrElse(false)
  }

  def inside(poly: Poly, s: Coor): Boolean = {
    val d = Coor(rand.nextDouble(), rand.nextDouble())

    poly.pairs.count(x => intersect(x._1, x._2, s, d)) % 2 == 1
  }
}
