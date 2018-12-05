package main

import java.io.File

import javax.imageio.ImageIO
import io.circe.syntax._
import ItemStored._
import io.circe.parser.decode

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

  def oneMatch(idx: Int)(implicit params: MatchParams, mat: Mat) = {
    val r = Holder.r
    val m = r(idx)

    r.values.par.foreach { i =>
      Matcher.tryMatch(m, i, s"${m.idx}_${i.idx}")
    }
  }

  def oneMatch(idx: Int, edge: Int)(implicit params: MatchParams, mat: Mat) = {
    val r = Holder.r
    val m = r(idx)

    r.values.par.foreach { i =>
      for (edgeIdx <- 0 until 4) {
        Matcher.tryOne(m, i, s"${m.idx}_${i.idx}", edge, edgeIdx)
      }
    }
  }

  def oneMatch(idx: Int, edge: Int, typ: Set[Int])(implicit params: MatchParams, mat: Mat) = {
    val r = Holder.r
    val m = r(idx)

    r.values.par.filter(witem => RealMatcher.intMap(witem.idx).intersect(typ).nonEmpty).foreach { i =>
      for (edgeIdx <- 0 until 4) {
        Matcher.tryOne(m, i, s"${m.idx}_${i.idx}", edge, edgeIdx)
      }
    }
  }

  def check(f2s: List[(Int, Int)], f2ff: List[(Int, Int)], ff2ss: List[(Int, Int)], ss2s: List[(Int, Int)]): Boolean = {
    val res = for {
      (f1, s1) <- f2s
      (f2, ff1) <- f2ff
      if f2 != f1
      (ff2, ss1) <- ff2ss
      if ff1 != ff2
      (ss2, s2) <- ss2s
      if ss1 != ss2 && s1 != s2
    } yield ()

    res.nonEmpty
  }

  def double(fst: Int, snd: Int, data: Data)(implicit params: MatchParams) = {
    var r = 0
    val f = data(fst)
    val s = data(snd)

    val f2s = Matcher.basicMatch(f, s)

    assert(f2s.nonEmpty)

    for {
      fi <- 0 until data.size
      si <- fi + 1 until data.size
      ff = data.byIdx(fi)
      ss = data.byIdx(si)
      if ff.idx != f.idx && ff.idx != s.idx && ss.idx != f.idx && ss.idx != s.idx

      f2ff = Matcher.basicMatch(f, ff)
      if f2ff.nonEmpty

      ss2s = Matcher.basicMatch(ss, s)
      if ss2s.nonEmpty

      ff2ss = Matcher.basicMatch(ff, ss)
      if ff2ss.nonEmpty
      if check(f2s, f2ff, ff2ss, ss2s)
    } {
      println(s"${ff.idx} ${ss.idx}")
      r = r + 1

    }
    println(r)
  }

  def dd() = {
    val data = readData("/Users/pavel/puzzle/center")
    double(16110, 16081, data)(MatchParams.precise)
  }

  def main1(): Unit = {

    val data = readData("/Users/pavel/puzzle/center")
    //    both(16081, 14872, data)(MatchParams.precise)
//    both(16081, 14872, data)(MatchParams.standard)
  }

  def both(fst: Int, snd: Int, data: Data)(implicit params: MatchParams, mat: Mat) = {
    val f = data(fst)
    val s = data(snd)
    var r = 0

    (0 until data.size).par.foreach { idx =>
      val other = data.byIdx(idx)
      val f1 = Matcher.basicMatch(f, other)
      val s1 = Matcher.basicMatch(s, other)
      if (f1.nonEmpty && s1.nonEmpty && (f1.map(_._2) ++ s1.map(_._2)).distinct.size > 1) {

        val z = Matcher.tryMatch(other, f, s"${other.idx}_${f.idx}")
        if (z.nonEmpty) {
          val z1 = Matcher.tryMatch(other, s, s"${other.idx}_${s.idx}")
          if (z1.isEmpty) {
            z.foreach(fileName => new File(fileName.get._1).delete())
          } else {
            println(other.idx)
            r = r + 1
          }
        }
        //        println(other.idx)
        //        r = r + 1
      }
    }

    println(s"=$r")
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

case class WItem(item: Item, idx: Int, name: String, metaData: MetaData) {}

case class Data(values: List[WItem]) {
  lazy val map = values.groupBy(_.idx).mapValues(_.head)
  lazy val arr = values.toArray

  def apply(idx: Int) = map(idx)

  def size = arr.size

  def byIdx(idx: Int): WItem = arr(idx)
}