package some

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.actor.Props
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Directives._
import akka.pattern._
import akka.util.Timeout
import main.{Data, Match, MatchParams}
import main.Match.read

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util._

object Holder {
  lazy val r: Data = read("/Users/pavel/puzzle/center")
}

//object WebServer {
//  implicit val system = ActorSystem("some")
//  implicit val materializer = ActorMaterializer()
//  implicit val timeout: akka.util.Timeout = Timeout(100 seconds)
//  implicit val executionContext = system.dispatcher
//
//
//  def main(args: Array[String]) {
//
//    val route =
//      path("get") {
//        get {
//          parameters('idx.as[Int]) { idx =>
//            onComplete(Future {
//              Match.oneMatch(idx)(MatchParams.precise)
//            }) {
//              case Success(_) => complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, "done"))
//              case Failure(value) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, value.toString()))
//            }
//          }
//        }
//      } ~
//      path("get1") {
//        get {
//          parameters('idx.as[Int]) { idx =>
//            onComplete(Future {
//              Match.oneMatch(idx)(MatchParams.standard)
//            }) {
//              case Success(_) => complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, "done"))
//              case Failure(value) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, value.toString()))
//            }
//          }
//        }
//      }
//
//    Http().bindAndHandle(route, "localhost", 8080)
//  }
//}