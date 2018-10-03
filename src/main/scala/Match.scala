package main

object Match {
  def readFiles(dir: String): List[(String, Int, String)] = {
    FileUtils.dir(dir).filter(x => x.getName.endsWith(".meta")).map { x =>
      (x.getAbsolutePath.dropRight(5), (x.getName.substring(4, 8) + x.getName.substring(13, 14)).toInt, x.getName)
    }
  }


  def read(dir: String): Map[Int, WItem] = {
    readFiles(dir).par.map { x =>
      println(x)
      val item = Handler.readItem(x._1)
      WItem(item, x._2, x._3)
    }.toList.groupBy(_.idx).mapValues(_.head)
  }

  def findMatch(idx: Int) = {

  }


  def main() = {
    val idx = 16110
    val r = read("/Users/pavel/puzzle/output")
    val m = r(idx)

    r.values.par.foreach { i =>
      Matcher.tryMatch(m.item, i.item, s"${m.idx}_${i.idx}")
    }
  }
}

case class WItem(item: Item, idx: Int, name: String) {}
