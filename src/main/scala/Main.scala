package main

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import main.Handler._

import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag
import MathUtils._
import IteratorUtils._

import scala.util.Random

case class Color(r: Int, g: Int, b: Int) {}

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val r = read("test.jpg")

    val ff = rotate(r, Pos(0, 0), Pos(5, 100), Pos(438,366))

    val res: Item = selectItem(r)
    println(res)
//    res.edgePoints.foreach(x => mutate(r, x))
//    mutate(r, Pos(648, 228))
//    mutate(r, Pos(235, 506))
    //    mutate(r, Pos(692, 354))
    //    mutate(r, Pos(232, 505))
    //    mutate(r, Pos(530, 593))
    //    mutate(r, Pos(351, 143))
    //    mutate(r, Pos(363 + 10, 466 + 10))
    ImageIO.write(r, "png", new File("1234.jpg"))
    //    base()
  }

  def base() = {
    val r = read("/Users/pavel/down/2.jpg")
    val res = selectBasement(r)

    ImageIO.write(res, "png", new File("test.jpg"))
  }
}

case class Item(center: Pos, r: Rect, edgePoints: List[Pos])

case class Pos(x: Int, y: Int) {
  def +(other: Pos): Pos = Pos(x + other.x, y + other.y)

  def toCoor = Coor(x, y)
}

object IteratorUtils {

  implicit class Last[T](val iter: Iterator[T]) {
    def last = {
      var x: Option[T] = None
      while (iter.hasNext) {
        x = Some(iter.next())
      }

      x.get
    }
  }

}

object MathUtils {

  /**
    * Implicit class for safe division
    *
    * @param dividend dividend
    * @param t        numeric implicit
    * @tparam T type
    */
  implicit class Divide[T](val dividend: T)(implicit t: Numeric[T]) {
    // scalastyle:off
    def /#(divider: T): Double = {
      // scalastyle:on
      if (t.compare(divider, t.zero) == 0) {
        0
      } else {
        t.toDouble(dividend) / t.toDouble(divider)
      }
    }
  }

}

class Image[C](values: Array[Array[C]]) {
  def apply(x: Int) = values(x)

  def apply(pos: Pos): C = values(pos.x)(pos.y)

  def width: Int = values.length

  def height: Int = values(0).length

  def map[T: ClassTag](f: C => T): Image[T] = {
    val ar = Array.ofDim[T](width, height)

    for {
      x <- 0 until width
      y <- 0 until height
    } ar(x)(y) = f(values(x)(y))

    new Image(ar)
  }

  def center(f: C => Boolean): Pos = {
    var cx = 0
    var cy = 0
    var cnt = 0
    for {
      x <- 0 until width
      y <- 0 until height
    } if (f(values(x)(y))) {
      cx = cx + x
      cy = cy + y
      cnt = cnt + 1
    }

    Pos(cx / cnt, cy / cnt)
  }

  lazy val shifts = List[Pos](Pos(1, 0), Pos(0, 1), Pos(-1, 0), Pos(0, -1))

  def bfs(pos: Pos, f: C => Boolean): (Int, Rect, Pos) = {
    val set = mutable.Set[Pos]()
    var size = 0
    var br: Pos = Pos(Int.MaxValue, Int.MaxValue)
    var rb: Pos = Pos(0, 0)
    var tl: Pos = Pos(0, 0)
    var lt: Pos = Pos(Int.MaxValue, Int.MaxValue)
    var sx = 0
    var sy = 0

    val q = mutable.Queue[Pos]()
    set.add(pos)
    q.enqueue(pos)
    while (q.nonEmpty) {
      val cur = q.dequeue()
      for {
        shift <- shifts
      } {
        val n = cur + shift
        if (!set.contains(n) && f(values(n.x)(n.y))) {
          size = size + 1
          set.add(n)
          q.enqueue(n)
          if (n.y > tl.y || (n.y == tl.y && n.x < tl.x)) tl = n
          if (n.y < br.y || (n.y == br.y && n.x > br.x)) br = n
          if (n.x < lt.x || (n.x == lt.x && n.y > lt.y)) lt = n
          if (n.x > rb.x || (n.x == rb.x && n.y < rb.y)) rb = n
          sx = sx + n.x
          sy = sy + n.y
        }
      }
    }

    (size, (br, rb, tl, lt), Pos((sx /# size).toInt, (sy /# size).toInt))
  }
}

case class ColorImage(array: Array[Array[Color]]) extends Image[Color](array)

object Handler {
  type ColorImage = Image[Color]
  type Rect = (Pos, Pos, Pos, Pos)
  val size = 1024


  implicit class ImplicitImage(f: BufferedImage) {
    def getColor(x: Int, y: Int): Color = {
      rgb(f.getRGB(x, y))
    }

    def getColors: Array[Array[Color]] = {
      val a = Array.ofDim[Color](f.getWidth, f.getHeight)
      for {
        x <- 0 until f.getWidth
        y <- 0 until f.getHeight
      } a(x)(y) = getColor(x, y)
      a
    }

    def isInside(pos: Pos): Boolean = {
      pos.x >=0 && pos.y >= 0 && pos.x < f.getWidth && pos.y < f.getHeight
    }
  }

  def read(file: String): BufferedImage = {
    ImageIO.read(new java.io.File(file))
  }

  def rgb(c: Int): Color = {
    val red = (c & 0xff0000) / 65536
    val green = (c & 0xff00) / 256
    val blue = (c & 0xff)
    Color(red, green, blue)
  }

  def isGreen(color: Color): Boolean = {
    color.g > color.r * 1.6
  }

  def isEmpty(color: Color): Boolean = {
    color.g == 0 && color.b == 0 && color.r == 0
  }

  def affine(f: BufferedImage, rect: Rect): BufferedImage = {
    val img = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)

    val p1 = rect._1
    val p2 = rect._2
    val l1 = rect._4
    val l2 = rect._3

    for {
      x <- 0 until size
      y <- 0 until size
    } {
      val x1 = Coor(p1.x + x.toDouble / size * (p2.x - p1.x), p1.y + x.toDouble / size * (p2.y - p1.y))
      val x2 = Coor(l1.x + x.toDouble / size * (l2.x - l1.x), l1.y + x.toDouble / size * (l2.y - l1.y))

      val y1 = Coor(p1.x + y.toDouble / size * (l1.x - p1.x), p1.y + y.toDouble / size * (l1.y - p1.y))
      val y2 = Coor(p2.x + y.toDouble / size * (l2.x - p2.x), p2.y + y.toDouble / size * (l2.y - p2.y))

      val inter = Geom.intersect(x1, x2, y1, y2)

      val dot = Pos(Math.round(inter.x.toDouble).toInt, Math.round(inter.y.toDouble).toInt)
      val color = f.getRGB(dot.x, dot.y)
      img.setRGB(x, y, color)
    }

    img
  }

  def mutateAll(f: BufferedImage, rect: Rect) = {
    for {
      x <- 0 until f.getWidth
      y <- 0 until f.getHeight
      if isGreen(f.getColor(x, y))
    } f.setRGB(x, y, 0)

    ImageIO.write(f, "jpg", new File("output.jpg"))
  }

  def mutate(f: BufferedImage, pos: Pos) = {
    for {
      x <- 0 to 10
      y <- 0 to 10
    } f.setRGB(pos.x + x, pos.y + y, 123123123)

  }

  def replaceBase(f: BufferedImage) = {
    for {
      x <- 0 until f.getWidth
      y <- 0 until f.getHeight
      if isGreen(f.getColor(x, y))
    } f.setRGB(x, y, 0)

    f
  }

  def selectBasement(f: BufferedImage) = {
    val img = ColorImage(f.getColors)

    val bool = img.map(isGreen)

    val center = bool.center(identity)

    val rr = Iterator.from(0).map(x => bool.bfs(center + Pos(x, x), identity)).filter { case (a, _, _) => a > 100 }.take(1).toList.head

    val aff = affine(f, rr._2)
    val res = replaceBase(aff)

    res
  }


  case class TTT(before: AngleDist, after: AngleDist)

  case class AngleDist(angle: Double, dist: Double, coor: Coor)

  /**
    *
    * @param fst
    * @param snd snd should be > fst
    * @return
    */
  def angleDiff(fst: Double, snd: Double): Double = {
    val res = snd - fst
    if (res < 0) {
      res + 2 * Math.PI
    } else {
      res
    }
  }

  def anglePlus(angle: Double, diff: Double): Double = {
    val res = angle + diff
    if (res > 2 * Math.PI) {
      res - 2 * Math.PI
    } else {
      res
    }
  }

  def selectItem(f: BufferedImage) = {
    assert(f.getWidth == size)
    assert(f.getHeight == size)

    val img: Image[Boolean] = ColorImage(f.getColors).map(x => !isEmpty(x))
    val rand = new Random(1)
    val last = Iterator.continually(1).map(x => Pos(rand.nextInt(size), rand.nextInt(size))).map(x => img.bfs(x, identity)).filter { case (a, _, _) => a > 1000 }.take(1).toList.head

    val steps = 720

    val beams: immutable.IndexedSeq[(Double, (Double, Coor))] = for (rad <- 0 until steps) yield {
      val angle = 2 * Math.PI * rad / steps
      (angle, beamLength(img, last._3, angle))
    }

    val double: Array[(Double, (Double, Coor))] = (beams ++ beams).toArray

    val buf = scala.collection.mutable.ArrayBuffer[TTT]()

    val res = scala.collection.mutable.ArrayBuffer[Coor]()
    val st = 100
    val eps = 0.1

    for (i <- 10 to double.size - 10) {
      if (double(i)._2._1 < double(i + 1)._2._1 * 0.8) {
        buf.append(TTT(AngleDist(double(i)._1, double(i)._2._1, double(i)._2._2), AngleDist(double(i + 1)._1, double(i + 1)._2._1, double(i + 1)._2._2)))
      }
    }

    for (i <- 0 to buf.size - 2) {
      if (angleDiff(buf(i).before.angle, buf(i + 1).before.angle) < 1.4) {
        val startAngle = buf(i).before.angle + eps
        val diff = (angleDiff(buf(i).before.angle, buf(i + 1).before.angle) - 2 * eps) / st
        val d = (0 to st).map { idx =>
          beamLength(img, last._3, anglePlus(startAngle, diff * idx))
        }.maxBy(_._1)

        res.append(d._2)
      }
    }

    buf.clear()
    for (i <- 10 to double.size - 10) {
      if (double(i)._2._1 > double(i + 1)._2._1 / 0.8) {
        buf.append(TTT(AngleDist(double(i)._1, double(i)._2._1, double(i)._2._2), AngleDist(double(i + 1)._1, double(i + 1)._2._1, double(i + 1)._2._2)))
      }
    }

    for (i <- 0 to buf.size - 2) {
      if (angleDiff(buf(i).before.angle, buf(i + 1).before.angle) < 1.4) {
        val startAngle = buf(i).before.angle + eps
        val diff = (angleDiff(buf(i).before.angle, buf(i + 1).before.angle) - 2 * eps) / st
        val d = (0 to st).map { idx =>
          beamLength(img, last._3, anglePlus(startAngle, diff * idx))
        }.maxBy(_._1)

        res.append(d._2)
      }
    }

    Item(last._3, last._2, res.toList.map(_.toPos))
  }

  def beamLength(f: Image[Boolean], start: Pos, angle: Double): (Double, Coor) = {
    val beam = Coor(Math.sin(angle), Math.cos(angle))
    val st = Coor(start.x, start.y)
    val last = Iterator.iterate(st)(x => x + beam).takeWhile(x => inside(f, x.toPos)).last
    (Coor.distance(st, last), last)
  }

  def inside(f: Image[Boolean], pos: Pos): Boolean = {
    var c = 0
    for {
      x <- pos.x - 2 to pos.x + 2
      y <- pos.y - 2 to pos.y + 2
    } {
      if (f(x)(y)) {
        c = c + 1
      }
    }

    c > 0
  }

  def rotate(f: BufferedImage, fst: Pos, snd: Pos, center: Pos = Pos(0, 0)): BufferedImage = {
    val angle = Math.atan2(snd.x - fst.x, snd.y - fst.y)

    val ff = new BufferedImage(f.getWidth, f.getHeight, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 until f.getWidth
      y <- 0 until f.getHeight
    } {
      val pos = ((Coor(x, y) - center.toCoor).rotate(angle) + center.toCoor).toPos
      if (f.isInside(pos)) {
        ff.setRGB(x, y, f.getRGB(pos.x, pos.y))
      }
    }

    ff
  }
}

case class Coor(x: Double, y: Double) {
  def -->(dest: Coor): Coor = {
    Coor(dest.x - x, dest.y - y)
  }

  def +(shift: Coor): Coor = {
    Coor(x + shift.x, y + shift.y)
  }

  def -(shift: Coor): Coor = {
    Coor(x - shift.x, y - shift.y)
  }

  def rotate(angle: Double): Coor = {
    val c = Math.cos(angle)
    val s = Math.sin(angle)
    Coor(x * c - y * s, x * s  + y * c)
  }

  def toPos = Pos(Math.round(x.toDouble).toInt, Math.round(y.toDouble).toInt)
}

object Coor {
  def distance(a: Coor, b: Coor): Double = {
    val sq = (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y)
    Math.sqrt(sq)
  }
}

object Geom {
  def det(x: Coor, y: Coor): BigDecimal = {
    x.x * y.y - x.y * y.x
  }

  def intersect(p1: Coor, p2: Coor, p3: Coor, p4: Coor): Coor = {
    val d = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x)

    Coor(((p1.x * p2.y - p1.y * p2.x) * (p3.x - p4.x) - (p1.x - p2.x) * (p3.x * p4.y - p3.y * p4.x)) / d,
      ((p1.x * p2.y - p1.y * p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x * p4.y - p3.y * p4.x)) / d
    )
  }
}