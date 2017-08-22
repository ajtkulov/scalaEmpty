package main

import spark.SparkUtils


case class Model(keyword: String, impressions: Int, ctr: Double, cost: Double, position: Int, company: String, revenue: Double)

object Main extends App {
  override def main(args: Array[String]): Unit = {

    val data1 = Model("1", 2, 0.3, 0.5, 4, "wer", 0.6)
    val data2 = Model("2", 2, 0.3, 0.5, 4, "wer", 0.6)
    val data3 = Model("3", 2, 0.3, 0.5, 4, "wer", 0.6)

    val data11 = Model("1", 2, 0.3, 0.5, 4, "wer", 0.7)
    val data21 = Model("2", 2, 0.3, 0.5, 4, "wer", 0.6)
    val data31 = Model("3", 2, 0.3, 0.5, 4, "wer", 0.6)
    val data41 = Model("5", 2, 0.3, 0.5, 4, "wer", 0.6)

    val sc = SparkUtils.localSpark

    val rdd1 = sc.makeRDD(List(data1, data2, data3))
    val rdd2 = sc.makeRDD(List(data11, data21, data31, data41))

    println("********")

        val diff = rdd2.subtract(rdd1)
//
    val res = diff.collect()

    println(res.mkString("\n"))
//    println(sc.makeRDD(List(1,2,3,4,5)).sum())

    sc.stop()
  }
}
