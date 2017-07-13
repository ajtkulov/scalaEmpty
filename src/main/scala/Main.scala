package main

import java.util.Properties

import akka.Done
import akka.actor.ActorSystem
import akka.kafka.ConsumerMessage.CommittableOffsetBatch
import akka.kafka.{ConsumerSettings, ProducerMessage, Subscriptions}
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.{ActorMaterializer, ThrottleMode}
import akka.stream.scaladsl.{Sink, Source}
import model.{Aggregator, Pageview}
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import org.apache.kafka.common.serialization.{ByteArrayDeserializer, StringDeserializer}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future

object Main extends App {

  implicit val system = ActorSystem("PlainSourceConsumerMain")
  implicit val materializer = ActorMaterializer()

  Source.cycle(() => Iterator.continually[Pageview](Pageview.gen))
    .throttle(1000, 800 milli, 2000, ThrottleMode.Shaping)
    .groupBy(100, x => (x.appId, Pageview.instantRounded(x.time)))
    .groupedWithin(500, 5 second)
    .map(x => {
//      println(x)
      Aggregator.handle(x)
    })
    .mergeSubstreams
    .runWith(Sink.foreach(println))

  val consumerSettings = ConsumerSettings(system, new StringDeserializer, new StringDeserializer)
    .withBootstrapServers("localhost:9092")
    .withGroupId("group1")
    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")

  def consume = {
    val done =
      Consumer.committableSource(consumerSettings, Subscriptions.topics("test"))
        .mapAsync(1) { msg =>
          println(s"BatchCommittableConsumer consume: $msg")
          Future.successful(Done).map(_ => msg.committableOffset)
        }
        .batch(20, first => CommittableOffsetBatch.empty.updated(first)) { (batch, elem) =>
          batch.updated(elem)
        }
        .mapAsync(3)(_.commitScaladsl())
        .runWith(Sink.ignore)
  }

  def produce = {
    val props = new Properties()
    props.put("bootstrap.servers", "localhost:9092")
    props.put("acks", "1")
    props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer")
    props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer")

    val producer = new KafkaProducer[String, String](props)

    val topic = "test"

    for (i <- 1 to 50) {
      val record = new ProducerRecord(topic, "key" + i, "value" + i)
      println(i)
      producer.send(record)
    }

    producer.close()
  }


}
