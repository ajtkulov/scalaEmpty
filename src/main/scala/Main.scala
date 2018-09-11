package main

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import main.Handler._

import scala.collection.mutable
import scala.reflect.ClassTag

case class Color(r: Int, g: Int, b: Int) {}

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val r = read()
    val res = selectBasement(r)
    println(r)
    println(res)

    ImageIO.write(res, "jpg", new File("test.jpg"))
  }
}

case class Pos(x: Int, y: Int) {
  def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
}

class Image[C](values: Array[Array[C]]) {
  def apply(x: Int) = values(x)

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

  def bfs(pos: Pos, f: C => Boolean): (Int, Rect) = {
    val set = mutable.Set[Pos]()
    var size = 0
    var br: Pos = Pos(Int.MaxValue, Int.MaxValue)
    var rb: Pos = Pos(0, 0)
    var tl: Pos = Pos(0, 0)
    var lt: Pos = Pos(Int.MaxValue, Int.MaxValue)

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
        }
      }
    }

    (size, (br, rb, tl, lt))
  }
}

case class ColorImage(array: Array[Array[Color]]) extends Image[Color](array)

object Handler {
  type ColorImage = Image[Color]
  type Rect = (Pos, Pos, Pos, Pos)

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
  }

  def read(): BufferedImage = {
    ImageIO.read(new java.io.File("/Users/pavel/down/2.jpg"))
  }

  def rgb(c: Int): Color = {
    val red = (c & 0xff0000) / 65536
    val green = (c & 0xff00) / 256
    val blue = (c & 0xff)
    Color(red, green, blue)
  }

  def isGreen(color: Color): Boolean = {
    (color.g > color.r * 1.6)
  }

  def affine(f: BufferedImage, rect: Rect): BufferedImage = {
    val size = 1024
    val img = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)

    val p1 = rect._1
    val p2 = rect._2
    val l1 = rect._4
    val l2 = rect._3

    for {
      x <- 0 until size
      y <- 0 until size
    } {
      //      try {
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
          x <- 0 until  f.getWidth
          y <- 0 until  f.getHeight
          if isGreen(f.getColor(x, y))
        } f.setRGB(x, y, 0)

//    mutate(f, Pos(1823, 1093))
//    mutate(f, Pos(606, 1201))
//    mutate(f, Pos(1767, 351))
//    mutate(f, Pos(1856, 1515))


    //    mutate(f, rect._1)
    //    mutate(f, rect._2)
    //    mutate(f, rect._3)
    //    mutate(f, rect._4)
    ImageIO.write(f, "jpg", new File("output.jpg"))
  }

  def mutate(f: BufferedImage, pos: Pos) = {
    for {
      x <- 0 to 10
      y <- 0 to 10
    } f.setRGB(pos.x + x, pos.y + y, 0)

  }

  def selectBasement(f: BufferedImage) = {
    val img = ColorImage(f.getColors)

    val bool = img.map(isGreen)

    val center = bool.center(identity)

    val rr = Iterator.from(0).map(x => bool.bfs(center + Pos(x, x), identity)).filter { case (a, _) => a > 100 }.take(1).toList.head

//    mutateAll(f, rr._2)

    affine(f, rr._2)
  }

}

case class Coor(x: BigDecimal, y: BigDecimal) {
  def -->(dest: Coor): Coor = {
    Coor(dest.x - x, dest.y - y)
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