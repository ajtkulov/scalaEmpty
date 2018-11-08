package main

import com.jujutsu.tsne.barneshut.ParallelBHTsne


object Main extends App {

  def parse(value: String): Array[Double] = {
    value.split(" ").filter(_.nonEmpty).map(_.toDouble)
  }

  def read(fileName: String): Array[Array[Double]] = {
    scala.io.Source.fromFile(fileName).getLines().map(parse).toArray
  }

  case class Some1(v: Int, g: String)

  override def main(args: Array[String]): Unit = {
    val tsne = new ParallelBHTsne()
    import com.jujutsu.utils.TSneUtils

    val X: Array[Array[Double]] = read("/Users/pavel/code/T-SNE-Java/tsne-demos/src/main/resources/datasets/mnist2500_X.txt")

    val config = TSneUtils.buildConfig(X, 3, 55, 20, 1000)

    val res = tsne.tsne(config)

    println(res.map(_.toList).toList.take(20))
  }
}
