package main

import org.apache.spark.mllib.regression
import org.apache.spark.mllib.stat.Statistics
import org.apache.spark.rdd.RDD


object Stats1 {
  def variance(data: RDD[regression.LabeledPoint]) = {
    val stats = Statistics.colStats(data.map(_.features))
    println(stats.mean)
    println(stats.variance)
    println(stats.numNonzeros)
    val dd = stats.variance.toArray.zipWithIndex.filter { case (v, idx) => v < 0.1 }.map { case (_, idx) => idx + 1 }.mkString(", ")
    println(dd)

    sys.exit(0)
  }
}
