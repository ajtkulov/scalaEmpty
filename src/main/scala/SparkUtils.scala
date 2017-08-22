package spark

import infrastructure.ConfigUtils
import org.apache.spark.{SparkConf, SparkContext}

object SparkUtils {
  def localSpark: SparkContext = {
    val conf = new SparkConf()
      .setMaster("local[1]")
      .setAppName("localTest")
      .set("spark.local.dir", ConfigUtils.config().getString("spark.tmpdir"))
      .set("spark.executor.memory", ConfigUtils.config().getString("spark.executor.memory"))
    val sc = new SparkContext(conf)

    sc
  }


}
