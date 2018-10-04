package main

import java.io.File

import javax.imageio.ImageIO
import io.circe.syntax._
import io.circe._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import ItemStored._
import io.circe.parser.decode
import Matcher._

object Match {
  def readFiles(dir: String): List[(String, Int, String)] = {
    FileUtils.dir(dir).filter(x => x.getName.endsWith(".meta")).map { x =>
      (x.getAbsolutePath.dropRight(5), (x.getName.substring(4, 8) + x.getName.substring(13, 14)).toInt, x.getName)
    }
  }


  def read(dir: String): Map[Int, WItem] = {
    readFiles(dir).par.map { x =>
//    readFiles(dir).map { x =>
      println(x)
      val item = Handler.readItem(x._1)
      WItem(item, x._2, x._3)
    }.toList.groupBy(_.idx).mapValues(_.head)
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

  def double(fst: Int, snd: Int, data: Map[Int, WItem]) = {

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
