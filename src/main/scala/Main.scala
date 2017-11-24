package main

import scala.util.continuations._

object Main extends App {
  override def main(args: Array[String]): Unit = {


    val z = reset {
      shift { k: (Int ⇒ Int) ⇒

        k(k(k(7)))
      } + 1
    }

    println(1)
  }
}