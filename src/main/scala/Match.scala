package main

import java.io.File

import javax.imageio.ImageIO
import io.circe.syntax._
import ItemStored._

object Match {
  def readFiles(dir: String): List[(String, Int, String)] = {
    FileUtils.dir(dir).filter(x => x.getName.endsWith(".meta")).map { x =>
      (x.getAbsolutePath.dropRight(5), (x.getName.substring(4, 8) + x.getName.substring(13, 14)).toInt, x.getName)
    }
  }


  def read(dir: String): Data = {
    val res = readFiles(dir).par.map { x =>
      //    readFiles(dir).map { x =>
      println(x)
      val item = Handler.readItem(x._1)
      WItem(item, x._2, x._3)
    }.toList

    Data(res)
  }

  def findMatch(idx: Int) = {

  }


  def main() = {
    val idx = 16110
    val r = read("/Users/pavel/puzzle/center")
    val m = r(idx)

    r.values.par.foreach { i =>
      Matcher.tryMatch(m.item, i.item, s"${m.idx}_${i.idx}")
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

  def double(fst: Int, snd: Int, data: Data) = {
    var r = 0
    val f = data(fst)
    val s = data(snd)

    val f2s = Matcher.basicMatch(f.item, s.item)

    assert(f2s.nonEmpty)

    for {
      fi <- 0 until data.size
      si <- fi + 1 until data.size
      ff = data.byIdx(fi)
      ss = data.byIdx(si)
      if ff.idx != f.idx && ff.idx != s.idx && ss.idx != f.idx && ss.idx != s.idx

      f2ff = Matcher.basicMatch(f.item, ff.item)
      if f2ff.nonEmpty

      ss2s = Matcher.basicMatch(ss.item, s.item)
      if ss2s.nonEmpty

      ff2ss = Matcher.basicMatch(ff.item, ss.item)
      if ff2ss.nonEmpty
      if check(f2s, f2ff, ff2ss, ss2s)
    } {
      println(s"${ff.idx} ${ss.idx}")

    }
    println(r)
  }

  def dd() = {
    val data = read("/Users/pavel/puzzle/center")
    double(16110, 16081, data)
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
}

case class WItem(item: Item, idx: Int, name: String) {}

case class Data(values: List[WItem]) {
  lazy val map = values.groupBy(_.idx).mapValues(_.head)
  lazy val arr = values.toArray

  def apply(idx: Int) = map(idx)

  def size = arr.size

  def byIdx(idx: Int) = arr(idx)
}