package main

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import spark.SparkUtils

/**
  * Model
  *
  * @param keyword     keyword
  * @param impressions impressions
  * @param ctr         ctr
  * @param cost        cost
  * @param position    position
  * @param company     position
  * @param revenue     revenue
  */
case class Model(keyword: String, impressions: Double, ctr: Double, cost: Double, position: Int, company: String, revenue: Double) {
  def toSubModel: KeywordModel = KeywordModel(keyword)

  def toSubModelPair: KeyChange = KeyChange(keyword, company)
}

/**
  * Submodel for common keywords
  *
  * @param keyword keyword
  */
case class KeywordModel(keyword: String) {
  def prettyPrint: String = keyword
}

/**
  * Submodel for reaching changes
  * @param keyword keyword
  * @param company company
  */
case class KeyChange(keyword: String, company: String) {
  def prettyPrint: String = s"$keyword,$company"
}

case class Diff(key: KeyChange, db: Set[Model], input: Set[Model]) {
  def prettyString = s"${key.prettyPrint}: db: ${db.mkString("[", ", ", "]")}, input: ${input.mkString("[", ", ", "]")}"
}

object Main extends App {
  def readModel(value: String): Model = {
    val split = value.split(",")
    Model(split(0), split(1).toDouble, split(2).toDouble, split(3).toDouble, split(4).toInt, split(5), split(6).toDouble)
  }

  override def main(args: Array[String]): Unit = {

    val header = "Search keyword,Impressions,CTR,Cost,Position,Company,Revenue"

    def readFile(fileName: String)(implicit sc: SparkContext): RDD[Model] = {
      sc.textFile(fileName).filter(x => x != header).map(readModel)
    }

    SparkUtils.withSpark { implicit sc =>
      val db = readFile("db.csv")
      val input = readFile("input.csv")

      println("********")

      val inputSubModel = input.map(_.toSubModel).distinct()
      val dbSubModel = db.map(_.toSubModel).distinct()
      inputSubModel.subtract(dbSubModel).map(_.prettyPrint).saveAsTextFile("newKeywords")
      dbSubModel.subtract(inputSubModel).map(_.prettyPrint).saveAsTextFile("deletedKeywords")

      val inputSubModelPair = input.map(x => x.toSubModelPair -> x)
      val dbSubModelPair = db.map(x => x.toSubModelPair -> x)

      val inputGrouped: RDD[(KeyChange, Iterable[(KeyChange, Model)])] = inputSubModelPair.groupBy(_._1)
      val dbGrouped: RDD[(KeyChange, Iterable[(KeyChange, Model)])] = dbSubModelPair.groupBy(_._1)

      val join: RDD[(KeyChange, (Iterable[(KeyChange, Model)], Iterable[(KeyChange, Model)]))] = inputGrouped.join(dbGrouped)

      join.flatMap {
        case (key, (inputCollection, dbCollection)) =>
          val inputSet = inputCollection.map(_._2).toSet
          val dbSet = dbCollection.map(_._2).toSet

          if (inputSet.intersect(dbSet).isEmpty) {
            Some(Diff(key, dbSet, inputSet))
          } else {
            None
          }
      }.map(_.prettyString).saveAsTextFile("changes")


    }
  }
}
