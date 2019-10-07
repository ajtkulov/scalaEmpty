package main

import com.google.common.hash.{BloomFilter, Funnels}
import scala.collection.mutable

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val trie = Trie(1e-9, 1000000)

    trie.addWord("microsoft")
    trie.addWord("microsoft research")
    trie.addWord("mikrotik")

    println(trie.transitions)
  }
}

case class Trie(error: Double, wordsAmount: Int) {
  lazy val charset = com.google.common.base.Charsets.US_ASCII
  val avgWordLength = 6

  val finalWordsBloom = BloomFilter.create[String](Funnels.stringFunnel(charset), wordsAmount, error)
  val prefixBloom = BloomFilter.create[String](Funnels.stringFunnel(charset), wordsAmount * avgWordLength, error)

  val transitions: mutable.Map[String, mutable.Set[Char]] = collection.mutable.Map[String, mutable.Set[Char]]()

  def addWord(word: String): Unit = {
    finalWordsBloom.put(word)
    for (i <- 1 to word.length) {
      val prefix = word.take(i)
      prefixBloom.put(prefix)

      val last = prefix.last
      val transitionPrefix = prefix.takeRight(5).dropRight(1)

      val set = if (transitions.contains(transitionPrefix)) {
        transitions(transitionPrefix)
      } else {
        mutable.Set[Char]()
      }

      set.add(last)
      transitions.put(transitionPrefix, set)
    }
  }

}