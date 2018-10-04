package main

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import main.Handler._

import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag
import MathUtils._
import IteratorUtils._

import scala.collection.mutable.ArrayBuffer
import scala.util.{Random, Success, Try}
import io.circe.syntax._
import io.circe._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import ItemStored._
import io.circe.parser.decode
import Matcher._

case class Color(r: Int, g: Int, b: Int) {}

object Main extends App {
  override def main(args: Array[String]): Unit = {
        Match.main()

    //    dir("/Users/pavel/input")
  }

  def base(input: String, output: String) = {
    val r = read(input)

    val res = selectBasement(r)
    val imgs = selectItems(res)

    imgs.zipWithIndex.foreach {
      case (img, idx) =>
        ImageIO.write(img, "png", new File(s"$output.$idx.jpg"))
        val item = selectItem(img)

        assert(item.edgePoints.size == 4)

        FileUtils.write(s"$output.$idx.meta", item.to.asJson.noSpaces)

        item.edgePoints.foreach {
          d => mutate(img, d)
        }

        ImageIO.write(img, "png", new File(s"$output.$idx.dot.jpg"))

    }
  }

  def dir(dirName: String) = {
    FileUtils.dir(dirName).filter(_.isFile).sortBy(_.getName).par.foreach { f =>
      Try {
        println(f)
        base(f.getAbsolutePath, s"output/${f.getName}")
        Thread.sleep(1000)
        //        import sys.process._
        //        s"mv ${f.getAbsolutePath} /Users/pavel/inputd" !
      }.getOrElse {
        println(s"********* $f")
        sys.exit(0)
        ???
      }
    }
  }
}

object Matcher {
  def tryMatch(fst: Item, snd: Item, suff: String) = {
    var mm = 0

    for {
      f <- 0 until 4
      s <- 0 until 4
      ff = fst.distance(f)
      ss = snd.distance(s)
      if Math.abs(ff - ss) / ff < 0.05
    } {
      val line1 = fst.line2(f)
      val line2 = snd.line2(s)

      val r1 = rotationAngle(line1.fst, line1.snd)
      val fstRotated = rotate(fst.f, r1, fst.center)

      val nline1 = line1.rotate(fst.center, -r1)

      val r2 = rotationAngle(line2.fst, line2.snd) + Math.PI
      val sndRotated = rotate(snd.f, r2, snd.center)

      val nline2 = line2.rotate(snd.center, -r2)

      val out = new BufferedImage(2048, 2048, BufferedImage.TYPE_INT_RGB)

      shift(fstRotated, out, nline1.fst.toPos, Pos(1024, 1024))
      val err = shift(sndRotated, out, nline2.snd.toPos, Pos(1024, 1024))
      val space = errorSpace(out, Coor.distance(nline1.fst, nline1.snd).toInt)

      if (err < 2000 && space < 1500) {
        println(s"$suff$mm")
        println(s"intersect: $err")
        println(s"space: ${space}")

        ImageIO.write(out, "png", new File(s"${suff}${mm}.jpg"))
        mm = mm + 1
      }
    }
  }

  def errorSpace(f: BufferedImage, width: Int): Int = {
    var err = 0
    for {
      x <- 1024 + 15 to 1024 + width - 15
      y <- 1024 - 110 to 1024 + 110
    } {
      if (isEmpty(f.getColor(x, y))) {
        f.setRGB(x, y, 0x004400)
        err = err + 1
      }
    }

    err
  }

  def shift(source: BufferedImage, dest: BufferedImage, sourcePos: Pos, destPos: Pos): Int = {
    var err = 0
    val delta = destPos - sourcePos
    for {
      x <- 0 until source.getWidth
      y <- 0 until source.getHeight
    } {
      val c = source.getRGB(x, y)
      val cc = source.getColor(x, y)
      val n = Pos(x, y) + delta
      if (dest.isInside(n)) {
        if (nonEmpty(dest.getColor(n.x, n.y))) {
          if (nonEmpty(cc)) {
            err = err + 1
            dest.setRGB(n.x, n.y, 0x00ffff)
          }
        } else {
          dest.setRGB(n.x, n.y, c)
        }
      }
    }
    err
  }

  def center(f: BufferedImage, center: Pos): (BufferedImage, Pos) = {
    val ff = new BufferedImage(f.getWidth, f.getHeight, BufferedImage.TYPE_INT_RGB)

    val c = Pos(f.getWidth / 2, f.getHeight / 2)

    val delta = c - center

    for {
      x <- 0 until f.getWidth
      y <- 0 until f.getHeight
    } {
      val pos = Pos(x, y)
      val on = pos - delta
      if (f.isInside(on)) {
        ff.setRGB(x, y, f.getRGB(on.x, on.y))
      }
    }

    (ff, c)
  }

}

trait LineOrder {
  def bools: Image[Boolean]

  def unorderedPoints: List[Pos]

  def lines: List[(Pos, Pos)] = {
    val res = ArrayBuffer[(Pos, Pos)]()

    for {
      i <- unorderedPoints.indices
      j <- unorderedPoints.indices
      if i != j
    } {
      val ff = unorderedPoints(i)
      val ss = unorderedPoints(j)
      val line = Line(ff.toCoor, ss.toCoor)
      if (interLine(line)) {
        res.append((ff, ss))
      }
    }

    res.toList
  }

  def interLine(line: Line): Boolean = {
    var cnt: Int = 0
    var sign: Int = 0

    for {
      x <- 0 until bools.width
      y <- 0 until bools.height
    } {

      val pos = Pos(x, y)
      if (bools(pos)) {
        cnt = cnt + 1
        if (line(pos.toCoor) < 0) {
          sign = sign + 1
        }
      }
    }
    sign * 6 < cnt
  }

  def order: List[Pos] = {
    val line = lines

    val stack = scala.collection.mutable.Stack[Pos]()
    val init = line.head._1

    stack.push(init)
    while (stack.size < line.size) {
      stack.push(lines.find { case (a, _) => a == stack.top }.get._2)
    }

    stack.toList
  }

}

case class Item(f: BufferedImage, center: Pos, edgePoints: List[Pos]) extends LineOrder {
  lazy val bools: Image[Boolean] = ColorImage(f.getColors).map(x => nonEmpty(x))

  override def unorderedPoints: List[Pos] = edgePoints

  def to: ItemStored = ItemStored(center, edgePoints)

  def distance(idx: Int): Double = {
    if (idx == edgePoints.size - 1) {
      Coor.distance(edgePoints.last.toCoor, edgePoints.head.toCoor)
    } else {
      Coor.distance(edgePoints(idx).toCoor, edgePoints(idx + 1).toCoor)
    }
  }

  def line2(idx: Int): Line2 = {
    if (idx == edgePoints.size - 1) {
      Line2(edgePoints.last.toCoor, edgePoints.head.toCoor)
    } else {
      Line2(edgePoints(idx).toCoor, edgePoints(idx + 1).toCoor)
    }
  }

  def toCenter: Item = {
    val (ff, newC) = Matcher.center(f, center)

    Item(ff, newC, edgePoints.map(x => x - center + newC))
  }
}

case class ItemStored(center: Pos, edgePoints: List[Pos]) {
  require(edgePoints.size == 4)
}

object ItemStored {
  implicit val encoder: Encoder[ItemStored] = deriveEncoder
  implicit val decoder: Decoder[ItemStored] = deriveDecoder
}

case class Basement(bools: Image[Boolean], unorderedPoints: List[Pos]) extends LineOrder {}

object Item {
  def create(f: BufferedImage, center: Pos, edgePoints: List[Pos]): Item = {
    val tmp = new Item(f, center, edgePoints.distinct)
    new Item(f, center, tmp.order)
  }
}

case class Pos(x: Int, y: Int) {
  def +(other: Pos): Pos = Pos(x + other.x, y + other.y)

  def -(other: Pos): Pos = Pos(x - other.x, y - other.y)

  def toCoor = Coor(x, y)

  def rotate(center: Pos, angle: Double) = {
    ((Coor(x, y) - center.toCoor).rotate(angle) + center.toCoor).toPos
  }
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

case class Params(eps: Double, threshold: Double, radius: Double) {}

class Image[C](values: Array[Array[C]]) {
  def apply(x: Int) = values(x)

  def apply(pos: Pos): C = values(pos.x)(pos.y)

  def update(pos: Pos, value: C): Unit = values(pos.x)(pos.y) = value

  def getOrElse(pos: Pos, default: C): C = {
    if (inside(pos)) {
      values(pos.x)(pos.y)
    } else {
      default
    }
  }

  def inside(pos: Pos): Boolean = {
    pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height
  }

  def width: Int = values.length

  def height: Int = values(0).length

  lazy val size = height * width

  def map[T: ClassTag](f: C => T): Image[T] = {
    val ar = Array.ofDim[T](width, height)

    for {
      x <- 0 until width
      y <- 0 until height
    } ar(x)(y) = f(values(x)(y))

    new Image(ar)
  }

  def center(f: C => Boolean): Pos = {
    var cx = 0L
    var cy = 0L
    var cnt = 0
    for {
      x <- 0 until width
      y <- 0 until height
    } if (f(values(x)(y))) {
      cx = cx + x
      cy = cy + y
      cnt = cnt + 1
    }

    Pos((cx / cnt).toInt, (cy / cnt).toInt)
  }

  lazy val shifts = List[Pos](Pos(1, 0), Pos(0, 1), Pos(-1, 0), Pos(0, -1))

  def bfs(pos: Pos, f: C => Boolean, sideEffect: Option[Pos => Unit] = None): (Int, Rect, Pos) = {
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
        if (inside(n) && !set.contains(n) && f(values(n.x)(n.y))) {
          size = size + 1
          set.add(n)
          q.enqueue(n)
          if (n.y > tl.y || (n.y == tl.y && n.x < tl.x)) tl = n
          if (n.y < br.y || (n.y == br.y && n.x > br.x)) br = n
          if (n.x < lt.x || (n.x == lt.x && n.y > lt.y)) lt = n
          if (n.x > rb.x || (n.x == rb.x && n.y < rb.y)) rb = n
          sx = sx + n.x
          sy = sy + n.y
          sideEffect.foreach(x => x(n))
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
      pos.x >= 0 && pos.y >= 0 && pos.x < f.getWidth && pos.y < f.getHeight
    }

    def copy: BufferedImage = {
      val res = new BufferedImage(f.getWidth, f.getHeight, BufferedImage.TYPE_INT_RGB)
      for {
        x <- 0 until f.getWidth
        y <- 0 until f.getHeight
      } res.setRGB(x, y, f.getRGB(x, y))

      res
    }
  }

  def read(file: String): BufferedImage = {
    ImageIO.read(new java.io.File(file))
  }

  def readItem(file: String): Item = {
    val json = FileUtils.read(s"$file.meta")
    val stored = decode[ItemStored](json).getOrElse(???)
    Item(ImageIO.read(new java.io.File(s"$file.jpg")), stored.center, stored.edgePoints)
  }

  def rgb(c: Int): Color = {
    val red = (c & 0xff0000) / 65536
    val green = (c & 0xff00) / 256
    val blue = (c & 0xff)
    Color(red, green, blue)
  }

  def isGreen(color: Color): Boolean = {
    color.g > color.r * 1.6 && color.b < color.g * 1.1
  }

  def isEmpty(color: Color): Boolean = {
    color.g == 0 && color.b == 0 && color.r == 0
  }

  def nonEmpty(color: Color): Boolean = {
    color.g > 0 || color.b > 0 || color.r > 0
  }

  def affine(f: BufferedImage, rect: List[Pos]): BufferedImage = {
    val img = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)

    val p1 = rect(0)
    val p2 = rect(1)
    val l1 = rect(3)
    val l2 = rect(2)

    for {
      x <- 0 until size
      y <- 0 until size
    } {
      val x1 = Coor(p1.x + x.toDouble / size * (p2.x - p1.x), p1.y + x.toDouble / size * (p2.y - p1.y))
      val x2 = Coor(l1.x + x.toDouble / size * (l2.x - l1.x), l1.y + x.toDouble / size * (l2.y - l1.y))

      val y1 = Coor(p1.x + y.toDouble / size * (l1.x - p1.x), p1.y + y.toDouble / size * (l1.y - p1.y))
      val y2 = Coor(p2.x + y.toDouble / size * (l2.x - p2.x), p2.y + y.toDouble / size * (l2.y - p2.y))

      val inter = Geom.intersect(x1, x2, y1, y2)

      val dot = Pos(Math.round(inter.x).toInt, Math.round(inter.y).toInt)
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

  def mutate(f: BufferedImage, pos: Pos, size: Int = 10) = {
    for {
      x <- 0 to size
      y <- 0 to size
      p = Pos(pos.x + x, pos.y + y)
      if f.isInside(p)
    } f.setRGB(p.x, p.y, 0xff0000)

  }

  def mutateR(f: BufferedImage, pos: Pos, rad: Int) = {
    for {
      x <- 0 until f.getWidth
      y <- 0 until f.getHeight()
      if (Coor.distance(pos.toCoor, Coor(x, y)) < rad)
    } f.setRGB(x, y, 123123123)

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


    var rad = 2000
    val buffer = ArrayBuffer[Pos]()


    val steps = 720
    while (rad > 100 && buffer.size < 4) {
      for (rr <- 0 until steps) yield {
        val angle = 2 * Math.PI * rr / steps

        val dot = center + Coor(Math.cos(angle) * rad, Math.sin(angle) * rad).toPos

        if (bool.getOrElse(dot, false) && buffer.size < 4 && buffer.forall(x => Coor.distance(dot.toCoor, x.toCoor) > 400)) {
          buffer.append(dot)
        }
      }

      rad = rad - 1
    }

    val base = Basement(bool, buffer.toList)
    val aff = affine(f, base.order)
    val res = replaceBase(aff)

    res
  }

  def normalize(f: BufferedImage, center: Pos): BufferedImage = {
    val res = new BufferedImage(f.getWidth, f.getHeight, BufferedImage.TYPE_INT_RGB)

    val img = ColorImage(f.getColors)
    img.bfs(center, x => nonEmpty(x), Some(pos => {
      res.setRGB(pos.x, pos.y, f.getRGB(pos.x, pos.y))
    }))

    res
  }

  case class Jump(before: AngleDist, after: AngleDist)

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

  def selectItems(f: BufferedImage): List[BufferedImage] = {
    val img: Image[Boolean] = ColorImage(f.getColors).map(x => !isEmpty(x))
    val rand = new Random(1)
    val all = Iterator.continually(1).map(x => Pos(rand.nextInt(size), rand.nextInt(size))).map(x => img.bfs(x, identity, Some(pos => {
      img(pos) = false
    }))).take(100).toList
      .filter { case (a, _, _) => a > 10000 }.groupBy(_._3).mapValues(x => x.head).values.toList

    all.map(x => normalize(f, x._3))
  }

  def checkCorner(img: Image[Boolean], rad: Int, rr: Int, center: Pos, angle: Double)(implicit params: Params): Boolean = {

    val (main, mc) = beamLengthOut(img, center, angle)
    val (main1, mc1) = beamLengthOut(img, center, angle - params.eps)
    val (main2, mc2) = beamLengthOut(img, center, angle + params.eps)
    val ang = Geom.angle(mc1 - mc, mc2 - mc)

    //    println(s"${mc.toPos} ${mc1.toPos} ${mc2.toPos} $ang")

    ang < params.threshold
  }

  def selectItem(f: BufferedImage) = {
    val list: List[Params] = List(
      Params(0.07, 2.3, 160)
      //      , Params(0.1, 2.35, 150)
    )

    list.toIterator.map(x => Try {
      selectItem1(f)(x)
    }).collect { case Success(x) => x }.take(1).toList.head
  }

  private def selectItem1(f: BufferedImage)(implicit params: Params) = {
    assert(f.getWidth == size)
    assert(f.getHeight == size)

    val img: Image[Boolean] = ColorImage(f.getColors).map(x => !isEmpty(x))
    val rand = new Random(1)
    val last = Iterator.continually(1).map(x => Pos(rand.nextInt(size), rand.nextInt(size))).map(x => img.bfs(x, identity)).filter { case (a, _, _) => a > 1000 }.take(1).toList.head

    val steps = 720

    var rad = 600

    val res = ArrayBuffer[Pos]()
    val buffer = ArrayBuffer[Pos]()
    val center = last._3

    while (rad > 100 && res.size < 4) {
      for (rr <- 0 until steps) yield {
        val angle = 2 * Math.PI * rr / steps

        val dot = center + Coor(Math.cos(angle) * rad, Math.sin(angle) * rad).toPos

        if (img.getOrElse(dot, false) && res.size < 4 && buffer.forall(x => Coor.distance(dot.toCoor, x.toCoor) > params.radius)) {
          buffer.append(dot)

          //          println(dot)

          if (checkCorner(img, rad, rr, center, angle)) {
            res.append(dot)
          }
        }
      }

      rad = rad - 1
    }

    Item.create(f, last._3, res.toList)
  }

  def beamLength(f: Image[Boolean], start: Pos, angle: Double): (Double, Coor) = {
    val beam = Coor(Math.sin(angle), Math.cos(angle))
    val st = Coor(start.x, start.y)
    val last = Iterator.iterate(st)(x => x + beam).takeWhile(x => inside(f, x.toPos)).last
    (Coor.distance(st, last), last)
  }

  def beamLengthOut(f: Image[Boolean], start: Pos, angle: Double): (Double, Coor) = {
    val beam = Coor(Math.cos(angle), Math.sin(angle))
    val st = Coor(start.x, start.y) + beam * 1000
    val last = Iterator.iterate(st)(x => x - beam).dropWhile(x => !f.getOrElse(x.toPos, false)).take(1).toList.head
    (Coor.distance(st, last), last)
  }

  def inside(f: Image[Boolean], pos: Pos, size: Int = 2): Boolean = {
    var c = 0
    for {
      x <- pos.x - size to pos.x + size
      y <- pos.y - size to pos.y + size
    } {
      if (f.inside(Pos(x, y)) && f(x)(y)) {
        c = c + 1
      }
    }

    c > 0
  }

  def rotationAngle(fst: Coor, snd: Coor): Double = {
    Math.atan2(snd.y - fst.y, snd.x - fst.x)
  }

  def rotate(f: BufferedImage, angle: Double, center: Pos): BufferedImage = {
    val ff = new BufferedImage(f.getWidth, f.getHeight, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 until f.getWidth
      y <- 0 until f.getHeight
    } {
      val pos = Pos(x, y).rotate(center, angle)
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

  def *(num: Int): Coor = {
    Coor(x * num, y * num)
  }

  def rotate(angle: Double): Coor = {
    val c = Math.cos(angle)
    val s = Math.sin(angle)
    Coor(x * c - y * s, x * s + y * c)
  }

  def toPos = Pos(Math.round(x.toDouble).toInt, Math.round(y.toDouble).toInt)

  def len: Double = Coor.distance(this, Coor(0, 0))
}

case class Line(a: Double, b: Double, c: Double) {
  def apply(coor: Coor): Double = {
    a * coor.x + b * coor.y + c
  }
}

object Line {
  def apply(fst: Coor, snd: Coor): Line = {
    Line(fst.y - snd.y, snd.x - fst.x, fst.x * snd.y - snd.x * fst.y)
  }
}

case class Line2(fst: Coor, snd: Coor) {
  def rotate(center: Pos, angle: Double): Line2 = {
    Line2(fst.toPos.rotate(center, angle).toCoor, snd.toPos.rotate(center, angle).toCoor)
  }
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

  def angle(v1: Coor, v2: Coor): Double = {
    val eps = 0.1
    if (v1.len < eps || v2.len < eps) {
      Math.PI
    } else {
      Math.acos((v1.x * v2.x + v1.y * v2.y) / v1.len / v2.len)
    }
  }
}
