package model

import org.joda.time.Instant
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

case class Cloud(timeStamp: Instant, poly: Poly, precipitationStrength: Double, precipitationType: Int) {}

case class SkyTimeLine(clouds: List[Cloud]) {
  def forecast(pos: Coor): Map[Instant, Forecast] = {
    clouds.groupBy(_.timeStamp).mapValues(Sky.apply).mapValues(_.forecast(pos))
  }
}

case class Sky(clouds: List[Cloud]) {
  require(clouds.map(_.timeStamp).distinct.size == 1)

  def forecast(pos: Coor): Forecast = {
    val inside = clouds.filter(cloud => Geometry.inside(cloud.poly, pos)).sortBy(x => x.precipitationStrength)(Ordering[Double].reverse).headOption

    val nearest = clouds.map(cloud => (cloud, Geometry.nearest(cloud.poly, pos))).minBy(x => x._2)._1

    Forecast(inside, nearest)
  }

}

case class Forecast(inside: Option[Cloud], nearest: Cloud) {}

object ModelReader {
  def readJson(jsValue: JsValue): List[Cloud] = {

    val obj: JsObject = jsValue.as[JsObject]

    obj.fields.flatMap(x => readCloud(new Instant(x._1.toLong * 1000), x._2)).toList
  }

  def readCloud(timestamp: Instant, jsValue: JsValue): List[Cloud] = {
    val obj = jsValue.as[JsObject]
    (obj \ "features").as[JsArray].value.flatMap(x => readGeo(timestamp, x)).toList
  }

  def toCoor(values: List[BigDecimal]): Coor = {
    Coor(values(0), values(1))
  }

  def toPoly(values: List[List[BigDecimal]]): Poly = {
    Poly(values.map(toCoor))
  }

  def readGeo(instant: Instant, jsValue: JsValue): List[Cloud] = {
    val polies = (jsValue \ "geometry" \ "coordinates").as[List[List[List[BigDecimal]]]]

    polies.map(x => Cloud(instant, toPoly(x), (jsValue \ "properties" \ "prec_strength").as[Double], (jsValue \ "properties" \ "prec_type").as[Int]))
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

  def distance(a: Coor, b: Coor): BigDecimal = {
    val sq = (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y)
    Math.sqrt(sq.toDouble)
  }

  def nearest(poly: Poly, d: Coor): BigDecimal = {
    poly.values.map(distance(_, d)).min
  }
}
