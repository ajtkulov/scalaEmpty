package main

import scala.collection.Iterator
import scala.collection.mutable.ArrayBuffer

object IteratorUtils {
  implicit class IteratorHelper[A](iterator: Iterator[A]) {
    def groupBy(predWhile: A => Boolean): Iterator[Seq[A]] = {
      val iter = iterator.buffered

      new Iterator[Seq[A]] {
        override def hasNext: Boolean = iter.hasNext

        override def next(): Seq[A] = {
          val res = ArrayBuffer[A]()

          while (iter.hasNext && predWhile(iter.head)) {
            res.append(iter.next())
          }

          while (iter.hasNext && ! predWhile(iter.head)) {
            iter.next()
          }

          res
        }
      }
    }

    def splitBy(splitWhen: A => Boolean): Iterator[Seq[A]] = {
      val iter = iterator.buffered

      new Iterator[Seq[A]] {
        override def hasNext: Boolean = iter.hasNext

        override def next(): Seq[A] = {
          val res = ArrayBuffer[A]()

          while (iter.hasNext && !splitWhen(iter.head)) {
            res.append(iter.next())
          }

          if (res.isEmpty && iter.hasNext) {
            res.append(iter.next())
          }

          res
        }
      }
    }
  }
}
