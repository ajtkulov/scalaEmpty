package main

import puzzle.State

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val state = State.fromList(List(
      List(1, 2, 3, 4),
      List(5, 6, 7, 8),
      List(9, 10, 11, 12),
      List(13, 14, 15, 0)
    ))

    println(state.neighbors.mkString("\n\n"))

  }
}