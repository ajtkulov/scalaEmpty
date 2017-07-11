package main

import akka.actor.{Actor, ActorSystem, Props}
import akka.stream.ActorMaterializer

class SampleActor extends Actor {
  override def receive: Receive = {
    case "test" => println("test")
  }
}

object Main extends App {


  implicit val system = ActorSystem("QuickStart")
  implicit val materializer = ActorMaterializer()

  val ref = system.actorSelection("akka.tcp://QuickStart@192.168.50.82:2552/user/sampleActor")
//  val ref = system.actorOf(Props[SampleActor], "sampleActor")

  for (i <- 1 to 100) {
    ref ! "test"
  }
}