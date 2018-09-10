package main

import org.apache.spark.ml.feature.LabeledPoint
import org.apache.spark.mllib.regression
import org.apache.spark.rdd.RDD
import org.slf4j.LoggerFactory

object Main extends App {
  lazy val logger = LoggerFactory.getLogger("Main")

  override def main(args: Array[String]): Unit = {
    RandomForestClassificationExample.main()
  }
}


// scalastyle:off println

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.mllib.tree.RandomForest
import org.apache.spark.mllib.tree.model.RandomForestModel
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.mllib.stat.{MultivariateStatisticalSummary, Statistics}

object RandomForestClassificationExample {
  def main(): Unit = {
    val conf = new SparkConf().setAppName("RandomForestClassificationExample").setMaster("local[4]").set("spark.executor.memory", "4G")
    val sc = new SparkContext(conf)
    // $example on$
    // Load and parse the data file.
    //    val data = MLUtils.loadLibSVMFile(sc, "sample_libsvm_data.txt")
    val data: RDD[regression.LabeledPoint] = MLUtils.loadLibSVMFile(sc, "output.csv")
    val converted: RDD[regression.LabeledPoint] = MLUtils.loadLibSVMFile(sc, "1.csv")
//    val test = MLUtils.loadLibSVMFile(sc, "test.csv")
    // Split the data into training and test sets (30% held out for testing)
    val splits = data.randomSplit(Array(0.7, 0.3))
        val (trainingData, testData) = (splits(0), splits(1))
//    val (trainingData, testData1) = (splits(0), splits(1))
//    val testData = test

//    Stats1.variance(data)

    // Train a RandomForest model.
    // Empty categoricalFeaturesInfo indicates all features are continuous.
    val numClasses = 2
    val categoricalFeaturesInfo = Map[Int, Int]()
    //    val numTrees = 3 // Use more in practice.
    val numTrees = 500 // Use more in practice.
    val featureSubsetStrategy = "auto" // Let the algorithm choose.
    val impurity = "gini"
    //    val maxDepth = 4
    val maxDepth = 10
    //    val maxBins = 32
    val maxBins = 6

    val model = RandomForest.trainClassifier(trainingData, numClasses, categoricalFeaturesInfo,
      numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)

    // Evaluate model on test instances and compute test error
    val labelAndPreds = testData.map { point =>
      val prediction = model.predict(point.features)
      (point.label, prediction)
    }
    val testErr = labelAndPreds.filter(r => r._1 != r._2).count.toDouble / testData.count()
    val truePositive = labelAndPreds.filter(r => r._1 == r._2 && r._1 == 1).count.toDouble
    val trueNegative = labelAndPreds.filter(r => r._1 == r._2 && r._1 == 0).count.toDouble
    val falseNegative = labelAndPreds.filter(r => r._1 != r._2 && r._1 == 1).count.toDouble
    val falsePositive = labelAndPreds.filter(r => r._1 != r._2 && r._1 < 0.5).count.toDouble

    val tt = testData.count()
    val total = falsePositive + truePositive + trueNegative + falseNegative
    val precision = truePositive / (truePositive + falsePositive)
    val conversionRate = (truePositive + falseNegative) / total
    val accuracy = (truePositive + trueNegative) / total

    //    println(s"Learned classification forest model:\n ${model.toDebugString}")
    println(s"Test Error = $testErr")

    println(s"Total: $total")
    println(s"Total: $tt")
    println(s"truePositive = $truePositive")
    println(s"trueNegative = $trueNegative")
    println(s"falseNegative = $falseNegative")
    println(s"falsePositive = $falsePositive")
    println(s"precision = $precision")
    println(s"conversionRate = $conversionRate")
    println(s"accuracy = $accuracy")

    // Save and load model
    //    model.save(sc, "target/tmp/myRandomForestClassificationModel")
    //    val sameModel = RandomForestModel.load(sc, "target/tmp/myRandomForestClassificationModel")
    // $example off$

    Thread.sleep(5000)

    sc.stop()
  }
}

// scalastyle:on println
