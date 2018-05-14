package request

import model.{Cloud, ModelReader}
import org.joda.time.Instant
import play.api.libs.json.Json


object Request {
  def get(time: Instant): String = {
    val roundTime = (time.getMillis / 1000 / 6000) * 6000
    val path = s"https://yandex.ru/pogoda/front/nowcast-prec?lon_min=10.453537087701307&lat_min=10.90468765955104&lon_max=62.40666208770129&lat_max=63.708333078668836&is_old=false&zoom=8&ts=${roundTime}"

    scala.io.Source.fromURL(path).getLines().mkString("")
  }

  def getModel(time: Instant): List[Cloud] = {
    ModelReader.readJson(Json.parse(get(time)))
  }
}
