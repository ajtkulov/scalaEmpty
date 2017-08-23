package spark

import infrastructure.ConfigUtils
import org.apache.spark.{SparkConf, SparkContext}

/**
  * Spark utils
  */
object SparkUtils {
  def localSpark: SparkContext = {
    val conf = new SparkConf()
      .setMaster("local[4]")
      .setAppName("test")
      .set("spark.local.dir", ConfigUtils.config().getString("spark.tmpdir"))
      .set("spark.executor.memory", ConfigUtils.config().getString("spark.executor.memory"))
    val sc = new SparkContext(conf)

    sc
  }

  def withSpark[A](func: SparkContext => A): A = {
    val sc = localSpark
    try {
      func(sc)
    } finally {
      sc.stop()
    }
  }
}
