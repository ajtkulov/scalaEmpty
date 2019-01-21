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
import javax.swing.JPanel
import Match.readData
import scala.sys.process._

case class Color(r: Int, g: Int, b: Int) {}

object Main extends App {
  override def main(args: Array[String]): Unit = {
//    Match.concave("/Users/pavel/code/scalaEmpty/center")
//    Match.mark("/Users/pavel/code/puzzleInput/centred")
    drawAll()
  }

  def clean() = {
    "rm *.jpg".!
  }

  def showAll() = {
    "open out.jpg".!
  }

  def show(idx: Int): Int = {
    val list = FileUtils.dir("/Users/pavel/code/puzzleMarked")
    val map = list.map(x => (x.getName.filter(_.isDigit).take(5).toInt, x)).toMap
    s"open ${map(idx).getAbsolutePath}".!
  }

  def show(idx: Int, id: Int): Int = {
    val (str, _) = Index.getByShort(idx)
    val out: Int = (str.filter(_.isDigit) + id.toString).toInt
    println(out)
    show(out)
  }

  def drawAll() = {
    val in = FileUtils.dir(".").filter(_.getName.contains("_")).filter(_.getName.contains(".jpg")).toParArray.map(x => (read(x.getName), x.getName.substring(6, 11))).toList.sortBy(_._2)

    val rr = draw(in, Pos(800, 600), Pos(1500, 1600), 3, 5)

    ImageIO.write(rr, "png", new File("out.jpg"))
  }

  def drawAll1() = {
    val in = FileUtils.dir(".").filter(_.getName.contains("_")).filter(_.getName.contains(".jpg")).toParArray.map(x => (read(x.getName), x.getName.substring(6, 11))).toList.sortBy(_._2)

    val rr = draw(in, Pos(600, 600), Pos(1500, 1600), 3, 5)

    ImageIO.write(rr, "png", new File("out.jpg"))
  }

  def drawItems(set: Set[Int])(implicit table: OnTable = RealOnTable()) = {
    val in = Holder.r.values.filter(w => !table.onTable(w) && RealMatcher.intMap(w.idx).intersect(set).nonEmpty).map(x => (x.item.f, x.idx.toString))

    val rr = draw(in, Pos(150, 150), Pos(850, 850), 5, 10)

    ImageIO.write(rr, "png", new File("out.jpg"))
  }

  def drawItemsHarder(set: Set[Int])(implicit table: OnTable = RealOnTable()) = {
    val in = Holder.r.values.filter(w => !table.onTable(w) && RealMatcher.intMap(w.idx).intersect(set) == set).map(x => (x.item.f, x.idx.toString))

    val rr = draw(in, Pos(150, 150), Pos(850, 850), 5, 10)

    ImageIO.write(rr, "png", new File("out.jpg"))
  }

  def draw(values: List[(BufferedImage, String)], posLU: Pos, posRB: Pos, scale: Int, perRow: Int): BufferedImage = {

    var r = 0
    var c = 0

    val rows = (values.size / perRow).ceil.toInt + 1


    val z: Pos = ((posRB - posLU).toCoor * (1.0 / scale)).toPos

    val res = new BufferedImage(z.x * perRow + 30, z.y * rows + 30, BufferedImage.TYPE_INT_RGB)

    val g2d = res.createGraphics()

    for (idx <- values.indices) {

      val (img, text) = values(idx)
      for {
        x <- posLU.x until posRB.x by scale
        y <- posLU.y until posRB.y by scale
      } {

        val xx = (x - posLU.x) / scale
        val yy = (y - posLU.y) / scale

        res.setRGB(c * z.x + xx, r * z.y + yy, img.getRGB(x, y))
      }
      g2d.drawString(s"$text/${Index.get(text.toInt)}", c * z.x, r * z.y + 10)

      c = c + 1
      if (c == perRow) {
        c = 0
        r = r + 1
      }
    }

    g2d.dispose()

    res
  }

  def manual(input: String) = {
    val item = readItem(input)

    mutate(item.f, item.center, color = 0x00ff00)

    item.edgePoints.foreach {
      d => mutate(item.f, d)
    }

    ImageIO.write(item.f, "png", new File(s"$input.output.jpg"))
  }

  def base(input: String, output: String) = {
    val r = read(input)

    val res = selectBasement(r)
    val imgs = selectItems(res)

    imgs.zipWithIndex.foreach {
      case (img, idx) =>
        Try {
          ImageIO.write(img, "png", new File(s"$output.$idx.jpg"))
          val item = selectItem(img)

          assert(item.edgePoints.size == 4)

          FileUtils.write(s"$output.$idx.meta", item.to.asJson.noSpaces)

          mutate(img, item.center, color = 0x00ff00)

          item.edgePoints.foreach {
            d => mutate(img, d)
          }

          ImageIO.write(img, "png", new File(s"$output.$idx.dot.jpg"))
        }
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

  def convexConcave(line: Line): (Int, Pos) = {
    var cnt: Int = 0
    var sign: Int = 0
    var cx = 0L
    var cy = 0L

    for {
      x <- 0 until bools.width
      y <- 0 until bools.height
    } {

      val pos = Pos(x, y)
      if (bools(pos)) {
        cnt = cnt + 1
        if (line(pos.toCoor) > 0) {
          sign = sign + 1
          cx = cx + x
          cy = cy + y
        }
      }
    }

    (sign, Pos((cx / sign).toInt, (cy / sign).toInt))
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

case class MinMax(minX: Int, maxX: Int, minY: Int, maxY: Int) {
  def mirror: MinMax = MinMax(minY, maxY, minX, maxX)
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

  def concave: List[ConcaveConvex] = {
    val x = minMax()
    val img = ColorImage(f.getColors).map(x => nonEmpty(x))

    (0 until 4).map { idx =>
      val line = line2(idx)
      val res = convexConcave(Line(line))
      if (res._1 > 6000) {
        ConcaveConvex(true, res._1, res._2)
      } else {
        val vect = (line.snd - line.fst) * 0.01
        val r = (20 to 80).toIterator.map { idx =>
          val c = (line.fst + vect * idx).toPos
          img.bfs(c, x => !x, None, Some(Line(line)))
        }.filter(_._1 > 2000).take(1).toList.headOption

        val rr = r.map(x => (x._1, x._3)).getOrElse((0, Pos(0, 0)))

        assert(rr._1 < 20000)
        ConcaveConvex(false, rr._1, rr._2)
      }
    }.toList
  }

  def metaData = {
    MetaData(concave, minMax())
  }

  def minMax(): MinMax = {
    var (minX, maxX, minY, maxY) = (Int.MaxValue, 0, Int.MaxValue, 0)
    for {
      x <- 0 until f.getWidth
      y <- 0 until f.getHeight
    } if (nonEmpty(f.getColor(x, y))) {
      if (x < minX) minX = x
      if (y < minY) minY = y
      if (x > maxX) maxX = x
      if (y > maxY) maxY = y
    }

    MinMax(minX, maxX, minY, maxY)
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

  def *(scale: Int): Pos = Pos(x * scale, y * scale)

  def toCoor = Coor(x, y)

  def rotate(center: Pos, angle: Double) = {
    ((Coor(x, y) - center.toCoor).rotate(angle) + center.toCoor).toPos
  }

  def mirror: Pos = Pos(y, x)
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

case class MatchParams(sizeDiff: Double, error: Int, space: Int, diffInConvex: Int, distConv: Int) {}

object MatchParams {
  val precise = MatchParams(0.025, 2000, 350, 800, 10)

  val standard = MatchParams(0.05, 2000, 1500, 1500, 10)

  val all = MatchParams(0.05, 20000, 15000, 15000, 15)

  val all1 = MatchParams(0.07, 20000, 15000, 15000, 20)

  val all2 = MatchParams(0.1, 20000, 15000, 15000, 30)

  val lol = MatchParams(0.07, 5000, 5000, 5000, 30)

  val lol1 = MatchParams(0.1, 5000, 5000, 5000, 30)

  val wide = MatchParams(0.05, 3000, 2500, 2500, 15)

  val medium = MatchParams(0.035, 2000, 1200, 1200, 10)
}

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

  def bfs(pos: Pos, f: C => Boolean, sideEffect: Option[Pos => Unit] = None, line: Option[Line] = None): (Int, Rect, Pos) = {
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
        if (inside(n) && !set.contains(n) && f(values(n.x)(n.y)) && line.forall(l => l.apply(n.toCoor) < 0)) {
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

    def mirror: BufferedImage = {
      val res = new BufferedImage(f.getHeight, f.getWidth, BufferedImage.TYPE_INT_RGB)
      for {
        x <- 0 until f.getWidth
        y <- 0 until f.getHeight
      } res.setRGB(y, x, f.getRGB(x, y))

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

  def mutate(f: BufferedImage, pos: Pos, size: Int = 10, color: Int = 0xff0000) = {
    assert(size < 1000)
    for {
      x <- 0 to size
      y <- 0 to size
      p = Pos(pos.x + x, pos.y + y)
      if f.isInside(p)
    } f.setRGB(p.x, p.y, color)

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

  def rotate(f: WItem, angle: Double, center: Pos): BufferedImage = {
    val qq = f.item.f
    val ff = new BufferedImage(qq.getWidth, qq.getHeight, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 until qq.getWidth
      y <- 0 until qq.getHeight
    } {
      val pos = Pos(x, y).rotate(center, angle)
      if (qq.isInside(pos)) {
        ff.setRGB(x, y, qq.getRGB(pos.x, pos.y))
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

  def *(num: Double): Coor = {
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

  def apply(line2: Line2): Line = {
    apply(line2.fst, line2.snd)
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

case class ConcaveConvex(convex: Boolean, size: Int, center: Pos) {
  def mirror: ConcaveConvex = copy(center = center.mirror)
}

case class MetaData(concave: List[ConcaveConvex], minMax: MinMax) {
  def mirror: MetaData = MetaData(concave.reverse.map(_.mirror), minMax.mirror)
}

object ConcaveConvex {
  implicit val encoder: Encoder[ConcaveConvex] = deriveEncoder
  implicit val decoder: Decoder[ConcaveConvex] = deriveDecoder
}

object MetaData {
  implicit val encoder: Encoder[MetaData] = deriveEncoder
  implicit val decoder: Decoder[MetaData] = deriveDecoder

  lazy val empty: MetaData = MetaData(List(), MinMax(0, 0, 0, 0))
}
