object Read {

  def writeOutput(size: Int): Unit = {
    val iter = scala.io.Source.fromFile("train_raw_correct.csv").getLines.take(size).drop(1)

    write("output.csv", iter.flatMap(parse))
  }


  case class M() {
    val a = scala.collection.mutable.HashMap[Double, Int]()

    def add(idx: Int, value: Double) = {
      if (a.contains(value)) {
        a(value) = a(value) + 1
      }
    }

    def count = a.size
  }



  def parse(s: String): Option[String] = {
    val dropColumnsIdx = Set(6, 9, 10, 11, 12, 21, 22, 23, 24, 25, 37, 38, 40, 41, 51, 54, 55, 56, 57, 66, 67, 72, 73, 75, 79, 84, 100, 116, 119, 122, 123, 125, 126, 127, 128, 129, 130, 131, 134, 137, 140, 141, 142, 144, 145, 147, 148, 149, 152, 154, 155, 158, 159, 160, 166)
    val split = s.split(",")
    val label = if (split.last == "conv" || split.last == """"conv"""") 1 else 0
    val drop = split.drop(1).dropRight(1)

    if (drop(6).toInt > 5 || label == 1) {

//      val values = drop.zipWithIndex.filterNot {case (_, idx) => idx == 23 || idx == 24 }
      val values = drop.zipWithIndex.filterNot {case (_, idx) => dropColumnsIdx.contains(idx + 1) }
        .map {
          case (item, idx) => s"${idx + 1}:$item"
        }

      Some(s"$label ${values.mkString(" ")}")
    } else {
      None
    }
  }

  def conv(s: String): Boolean = {
    val split = s.split(",")
    split.last == "conv" || split.last == """"conv""""
  }

  type FileName = String
  import java.io._
  def withFile[A](fileName: FileName)(func: PrintWriter => A): Unit = {
    val file = new File(fileName)
    val write = new PrintWriter(file)
    try {
      func(write)
    } finally {
      write.close()
    }
  }

  def write(fileName: FileName, iterator: Iterator[String]): Unit = {
    withFile(fileName) { output =>
      iterator.foreach(line => output.println(line))
    }
  }


  def writeTest: Unit = {
    val iter = scala.io.Source.fromFile("test_raw_correct.csv").getLines.take(10000000).drop(1)

    write("test.csv", iter.flatMap(parse))
  }
}
