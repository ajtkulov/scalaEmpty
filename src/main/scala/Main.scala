package main

import scala.util.continuations._

object Main extends App {
  override def main(args: Array[String]): Unit = {


    val z = reset {
      shift { k: (Int ⇒ Int) ⇒

        k(k(k(7)))
      } + 1
    }


    reset {
      println("A")
      shift { k1: (Unit => Unit) =>
        println("B")
        k1()
        println("C")
      }
      println("D")
      shift { k2: (Unit => Unit) =>
        println("E")
        k2()
        println("F")
      }
      println("G")
    }
    println(1)
  }
}