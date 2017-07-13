package model

import org.joda.time.Instant

import scala.util.Random

import poc.CollectionUtils._

case class Pageview(appId: String, time: Instant, browserId: String, url: String) {

}


object Pageview {
  val r = new Random()
  lazy val apps = List("abc", "dfs", "xcv")
  def genAppId = apps(r.nextInt(apps.size))

  lazy val browserIds = List.range(1, 10).map(_.toString)
  def genBrowserId = browserIds(r.nextInt(browserIds.size))

  lazy val urls = List.range(1, 9).map(_.toString)
  def genUrl = urls(r.nextInt(urls.size))

  def gen: Pageview = Pageview(genAppId, new Instant(), genBrowserId, genUrl)

  def instantRounded(value: Instant): Instant = {
    new Instant(value.getMillis/5000 * 5000)
  }
}

case class StatKey(appId: String, begin: Instant)

case class StatValue(count: Int, topUrls: Map[String, Int]) {
  def +(other: StatValue): StatValue = {
    StatValue(count + other.count, (topUrls.toList ++ other.topUrls).countByValue)
  }
}

case class Stat(key: StatKey, value: StatValue)

object Aggregator {
  def handle(pageviews: Seq[Pageview]): Stat = {
    val appId = pageviews.head.appId
    val instant = Pageview.instantRounded(pageviews.head.time)

    val topUrls = pageviews.map(_.url).countValues[Int]

    val statValues = StatValue(pageviews.size, topUrls)

    Stat(StatKey(appId, instant), statValues)
  }
}