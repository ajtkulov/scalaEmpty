import akka.actor.Actor

import scala.concurrent.duration._
import model.{Stat, StatKey, StatValue}
import org.joda.time.Instant

class AggregateActor extends Actor {
  type State = Map[StatKey, StatValue]

  context.system.scheduler.schedule(0 second, 20 seconds, self, RemoveOld())(context.dispatcher)

  override def receive: Receive =
    stat(Map())

  def stat(value: State): Receive = {
    case x: Stat => context.become(stat(updateState(value, x)))
    case RemoveOld() => context.become(stat(removeOld(value)))
    case Get(appId) => sender() ! get(appId, value)
  }

  def updateState(value: State, inc: Stat): State = {
    if (value.contains(inc.key)) {
      val current: StatValue = value(inc.key)
      value + (inc.key -> (inc.value + current))
    } else {
      value + (inc.key -> inc.value)
    }
  }

  def removeOld(value: State): State = {
    import com.github.nscala_time.time.Imports._
    val threshold = new Instant().minus(org.joda.time.Duration.standardSeconds(10))
    value.filterKeys(x => x.begin >= threshold)
  }

  def get(appId: String, value: State): Result = {
    val values = value.filterKeys(x => x.appId == appId).values.toList

    Result(appId, values.map(x => x.count).sum)
  }
}

case class Result(appId: String, count: Int)
