package main

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import main.Handler._

import scala.collection.mutable.ArrayBuffer

object Matcher {
  def basicMatch(fw: WItem, sw: WItem, fIdx: Int, sIdx: Int)(implicit params: MatchParams, mat: Mat): Boolean = {
    val fst = fw.item
    val snd = sw.item

    val res = for {
      f <- Some(fIdx)
      s <- Some(sIdx)
      if mat.mat(fw, sw)
      ff = fst.distance(f)
      ss = snd.distance(s)
      delta = Math.abs(ff - ss) / ff
      if delta < params.sizeDiff
      c1 = fw.metaData.concave(f)
      c2 = sw.metaData.concave(s)
      if c1.convex ^ c2.convex
      if Math.abs(c1.size - c2.size) < params.diffInConvex
      line1 = fst.line2(f)
      line2 = snd.line2(s)
      r1 = rotationAngle(line1.fst, line1.snd)
      newC1 = c1.center.rotate(fst.center, -r1)
      nline1 = line1.rotate(fst.center, -r1)

      r2 = rotationAngle(line2.fst, line2.snd) + Math.PI
      newC2 = c2.center.rotate(snd.center, -r2)

      nline2 = line2.rotate(snd.center, -r2)

      cc = Pos(1024, 1024)
      nnC1 = cc - nline1.fst.toPos + newC1
      nnC2 = cc - nline2.snd.toPos + newC2

      dd = Coor.distance(nnC1.toCoor, nnC2.toCoor)
      if dd < params.distConv
    } yield {
      ()
    }

    res.isDefined
  }

  case class RotateInfo(r1: Double, c1: Pos, r2: Double, c2: Pos) {}

  def tryOne(fw: WItem, sw: WItem, suff: String, fIdx: Int, sIdx: Int, save: Boolean = true)(implicit params: MatchParams, mat: Mat): Option[(String, String, RotateInfo)] = {
    val fst = fw.item
    val snd = sw.item

    for {
      f <- Some(fIdx)
      s <- Some(sIdx)
      if mat.mat(fw, sw)
      ff = fst.distance(f)
      ss = snd.distance(s)
      delta = Math.abs(ff - ss) / ff
      if delta < params.sizeDiff
      c1 = fw.metaData.concave(f)
      c2 = sw.metaData.concave(s)
      if c1.convex ^ c2.convex
      if Math.abs(c1.size - c2.size) < params.diffInConvex
      line1 = fst.line2(f)
      line2 = snd.line2(s)
      r1 = rotationAngle(line1.fst, line1.snd)
      newC1 = c1.center.rotate(fst.center, -r1)
      nline1 = line1.rotate(fst.center, -r1)

      r2 = rotationAngle(line2.fst, line2.snd) + Math.PI
      newC2 = c2.center.rotate(snd.center, -r2)

      nline2 = line2.rotate(snd.center, -r2)

      cc = Pos(1024, 1024)
      nnC1 = cc - nline1.fst.toPos + newC1
      nnC2 = cc - nline2.snd.toPos + newC2

      dd = Coor.distance(nnC1.toCoor, nnC2.toCoor)
      if dd < params.distConv

      fstRotated = rotate(fw, r1, fst.center)
      sndRotated = rotate(sw, r2, snd.center)
      out = new BufferedImage(2048, 2048, BufferedImage.TYPE_INT_RGB)

      sh = shift(fstRotated, out, nline1.fst.toPos, cc)
      err = shift(sndRotated, out, nline2.snd.toPos, cc)

      space = errorSpace(out, Coor.distance(nline1.fst, nline1.snd).toInt)

      mm = s"_${f}_${s}"
      if err < params.error && space < params.space
    } yield {
      val rr =
        s"""
           |$suff$mm
           |intersect: $err; space: ${space}; delta: ${delta}
           |centerDiff: $dd
           |sizes: ${c1.size} ${c2.size}
            """.stripMargin

      val output = s"${suff}${mm}.jpg"
      if (save) {
        ImageIO.write(out, "png", new File(output))
      }
      (output, rr, RotateInfo(r1, nline1.fst.toPos, r2, nline2.snd.toPos))
    }
  }

  def drawOne(fw: WItem, sw: WItem, fIdx: Int, sIdx: Int): BufferedImage = {
    val fst = fw.item
    val snd = sw.item

    val line1 = fst.line2(fIdx)
    val line2 = snd.line2(sIdx)
    val r1 = rotationAngle(line1.fst, line1.snd)
    val nline1 = line1.rotate(fst.center, -r1)

    val r2 = rotationAngle(line2.fst, line2.snd) + Math.PI

    val nline2 = line2.rotate(snd.center, -r2)

    val cc = Pos(1024, 1024)

    val fstRotated = rotate(fw, r1, fst.center)
    val sndRotated = rotate(sw, r2, snd.center)
    val out = new BufferedImage(2048, 2048, BufferedImage.TYPE_INT_RGB)

    val sh = shift(fstRotated, out, nline1.fst.toPos, cc)
    val err = shift(sndRotated, out, nline2.snd.toPos, cc)
    out
  }

  def drawOther(fw: WItem, sw: WItem, fIdx: Int, sIdx: Int, prev: BufferedImage): BufferedImage = {
    val fst = fw.item
    val snd = sw.item

    val line1 = fst.line2(fIdx)
    val line2: Line2 = snd.line2(sIdx)
    val line3: Line2 = fst.line2((fIdx + 1) % 4)
    val r1 = rotationAngle(line1.fst, line1.snd)
    val nline1 = line1.rotate(fst.center, -r1)

    val diff = rotationAngle(line3.fst, line3.snd)

    val r2 = rotationAngle(line2.fst, line2.snd) + r1 - diff

    val nline2 = line2.rotate(snd.center, -r2)

    val cc = Pos(1024, 1024)

    val sndRotated = rotate(sw, r2, snd.center)
    val err = shift(sndRotated, prev, nline2.fst.toPos, cc)
    prev
  }

  def tryMatch(fw: WItem, sw: WItem, suff: String)(implicit params: MatchParams, mat: Mat) = {
    for {
      f <- 0 until 4
      s <- 0 until 4
    } yield tryOne(fw, sw, suff, f, s)
  }

  def errorSpace(f: BufferedImage, width: Int): Int = {
    var err = 0
    for {
      x <- 1024 + 15 to 1024 + width - 15
      y <- 1024 - 90 to 1024 + 90
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

trait Mat {
  def mat(fst: WItem, snd: WItem): Boolean
}

object RealMatcher extends Mat {
  lazy val map: Map[String, Set[Int]] = scala.io.Source.fromFile("mark.txt").getLines().map { line =>
    val split = line.split(" ")
    val set = split.last.toList.map(_.toString.toInt).toSet
    val refSet = if (!set.contains(6)) {
      set + 7
    } else {
      set
    }
    (split.head.dropRight(3), refSet)
  }.toMap

  def mat(fst: WItem, snd: WItem): Boolean = {
    map(fst.name.dropRight(4)).intersect(map(snd.name.dropRight(4))).nonEmpty
  }

  lazy val intMap: Map[Int, Set[Int]] = {
    map.map {
      case (key, value) => (key.filter(_.isDigit).toInt, value)
    }
  }
}

object FakeMatcher extends Mat {
  def mat(fst: WItem, snd: WItem): Boolean = true
}

object Index {
  lazy val list: List[(String, Int)] = scala.io.Source.fromFile("list.txt").getLines().toList.zipWithIndex.map { case (name, idx) => (name, idx + 1) }

  def get(idx: Int): Int = list.filter { case (name, _) => name.contains((idx / 10).toString) }.head._2

  def getByShort(idx: Int): (String, Int) = list(idx - 1)
}

trait OnTable {
  def onTable(w: WItem): Boolean
}

case class RealOnTable() extends OnTable {
  val all: Set[Int] = scala.io.Source.fromFile("pairs").getLines().toList.flatMap(x => x.filterNot(c => c == ' ' || c == '_').split("-")).map(x => x.toInt).toSet

  def extra: Set[Int] = {
    val a: List[String] = scala.io.Source.fromFile("extra.txt").getLines().toList.map(_.toInt).map(Index.getByShort).map(_._1)
    val res = for (idx <- 0 until 3; item <- a; z = RealMatcher.map(s"$item.$idx."); if z.contains(1) && z.size == 2) yield {
      item.filter(_.isDigit).toInt * 10 + idx
    }
    res.toSet
  }

//  override def onTable(w: WItem): Boolean = all.contains(w.idx)
  override def onTable(w: WItem): Boolean = !extra.contains(w.idx)
}

object FakeOnTable extends OnTable {
  override def onTable(w: WItem): Boolean = false
}

