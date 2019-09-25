package main

import java.io.File

import javax.imageio.ImageIO
import io.circe.syntax._
import ItemStored._
import io.circe.parser.decode
import Handler._

object Match {
  def readFiles(dir: String, size: Int = Int.MaxValue): List[(String, Int, String)] = {
    FileUtils.dir(dir).filter(x => x.getName.endsWith(".meta")).take(size).map { x =>
      (x.getAbsolutePath.dropRight(5), (x.getName.substring(4, 8) + x.getName.substring(13, 14)).toInt, x.getName)
    }
  }

  def readData(dir: String, size: Int = Int.MaxValue): Data = {
    val res = readFiles(dir, size).par.map { x =>
      println(x)
      readWItem(x._1, x._2, x._3)
    }.toList

    Data(res)
  }

  def readWItem(fileName: String, idx: Int, name: String) = {
    val item = Handler.readItem(fileName)
    val meta = if (new java.io.File(s"${fileName}.meta.data").exists) {
      val json = FileUtils.read(s"${fileName}.meta.data")
      decode[MetaData](json).getOrElse(???)
    } else {
      MetaData.empty
    }

    WItem(item, idx, name, meta)
  }

  def oneMatch(idx: Int)(implicit params: MatchParams, mat: Mat): Unit = {
    val onTable: OnTable = RealOnTable()
    val r = Holder.r
    val m = r(idx)

    r.values.par.foreach { i =>
      if (!onTable.onTable(i))
        Matcher.tryMatch(m, i, s"${m.idx}_${i.idx}")
    }
  }

  def oneMatch(idx: Int, edge: Int)(implicit params: MatchParams, mat: Mat): Unit = {
    val onTable: OnTable = RealOnTable()
    val r = Holder.r
    val m = r(idx)

    r.values.par.foreach { i =>
      if (!onTable.onTable(i))
        for (edgeIdx <- 0 until 4) {
          Matcher.tryOne(m, i, s"${m.idx}_${i.idx}", edge, edgeIdx)
        }
    }
  }

  def oneMatch(idx: Int, edge: Int, typ: Set[Int])(implicit params: MatchParams, mat: Mat, onTable: OnTable) = {
    val r = Holder.r
    val m = r(idx)

    r.values.par.filter(witem => RealMatcher.intMap(witem.idx).intersect(typ).nonEmpty).foreach { i =>
      if (!onTable.onTable(i))
        for (edgeIdx <- 0 until 4) {
          Matcher.tryOne(m, i, s"${m.idx}_${i.idx}", edge, edgeIdx)
        }
    }
  }

  def oneMatchHarder(idx: Int, edge: Int, typ: Set[Int])(implicit params: MatchParams, mat: Mat, onTable: OnTable) = {
    val r = Holder.r
    val m = r(idx)

    r.values.par.filter(witem => RealMatcher.intMap(witem.idx).intersect(typ) == typ).foreach { i =>
      if (!onTable.onTable(i))
        for (edgeIdx <- 0 until 4) {
          Matcher.tryOne(m, i, s"${m.idx}_${i.idx}", edge, edgeIdx)
        }
    }
  }

  def twoMatchHarder(fst: Int, fstEdge: Int, snd: Int, sndEdge: Int, typ: Set[Int])(implicit params: MatchParams, mat: Mat, onTable: OnTable) = {
    val r = Holder.r
    val m = r(fst)
    val m2 = r(snd)

    r.values.par.filter(witem => RealMatcher.intMap(witem.idx).intersect(typ) == typ).foreach { i =>
      if (!onTable.onTable(i))
        for (edgeIdx <- 0 until 4) {
          if (Matcher.basicMatch(i, m, edgeIdx, fstEdge)) {
            val z: Option[(String, String, Matcher.RotateInfo)] = Matcher.tryOne(i, m, s"${i.idx}_${m.idx}", edgeIdx, fstEdge, false)
            if (z.isDefined) {
              val zz = Matcher.tryOne(i, m2, s"${i.idx}_${m2.idx}", (edgeIdx + 3) % 4, sndEdge, false)
              if (zz.isDefined) {

                val zzz = Matcher.drawOne(i, m, edgeIdx, fstEdge)
                val o = Matcher.drawOther(i, m2, edgeIdx, sndEdge, zzz)
                ImageIO.write(o, "png", new File(s"${m.idx}_${i.idx}.jpg"))
              }
            }
          }
        }
    }
  }

  def center(dir: String) = {
    readFiles(dir).foreach { x =>
      println(x)
      val prefix = "center/"
      val item = Handler.readItem(x._1).toCenter
      ImageIO.write(item.f, "png", new File(s"$prefix${x._3.dropRight(4)}jpg"))
      FileUtils.write(s"$prefix${x._3.dropRight(4)}meta", item.to.asJson.noSpaces)
    }
  }

  def concave(dir: String) = {
    readData(dir).values.foreach { item =>
      println(item.name)
      val metaData = item.item.metaData
      FileUtils.write(s"${item.name}.data", metaData.asJson.noSpaces)
    }
  }

  def mark(dir: String) = {
    readData(dir).values.foreach { item =>
      val img = item.item.f
      for (idx <- 0 until 4) {
        val line = item.item.line2(idx)
        val pos = ((line.snd + line.fst) * 0.5).toPos
        for (i <- 1 to idx + 1) {
          Handler.mutate(img, pos + Pos(i * 12, 0), size = 10, 0xffff00)
        }
      }

      ImageIO.write(img, "png", new File(s"${item.name}.mark.jpg"))
    }
  }
}

case class WItem(item: Item, idx: Int, name: String, metaData: MetaData) {
  def rotate(): WItem = {
    val edge = item.edgePoints.reverse.map(x => x.mirror)

    val i = Item(item.f.mirror, item.center.mirror, edge)
    WItem(i, idx, name, metaData.mirror)
  }

  def save(dir: String) = {
    val n = name.dropRight(5)
    ImageIO.write(item.f, "png", new File(s"$dir/$n.jpg"))
    FileUtils.write(s"$dir/$n.meta", item.to.asJson.noSpaces)
    FileUtils.write(s"$dir/$n.meta.data", metaData.asJson.noSpaces)
  }

}

case class Data(values: List[WItem]) {
  lazy val map = values.groupBy(_.idx).mapValues(_.head)
  lazy val arr = values.toArray

  def apply(idx: Int) = map(idx)

  def size = arr.size

  def byIdx(idx: Int): WItem = arr(idx)
}