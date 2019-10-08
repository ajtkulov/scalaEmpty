package main

import com.google.common.hash.{BloomFilter, Funnels}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val trie = read("all_names.csv")

    println(trie.fuzzyMatchCont("LARRY", 0))
  }

  def read(fileName: String): Trie = {
    val trie = Trie(1000000)
    val iter = scala.io.Source.fromFile(fileName).getLines().take(1000).map(x => x.filterNot(ch => ch == '"').toUpperCase)
    iter.foreach(word => trie.addWord(word))

    trie
  }
}


case class Trie(wordsAmount: Int, error: Double = 1e-9) {
  lazy val charset = com.google.common.base.Charsets.US_ASCII
  val avgWordLength = 24

  val finalWordsBloom = BloomFilter.create[String](Funnels.stringFunnel(charset), wordsAmount, error)
  val prefixBloom = BloomFilter.create[String](Funnels.stringFunnel(charset), wordsAmount * avgWordLength, error)

  val transitions: mutable.Map[String, mutable.Set[Char]] = collection.mutable.Map[String, mutable.Set[Char]]()

  val transitionLength = 4

  def addWord(word: String): Unit = {
    finalWordsBloom.put(word)
    for (i <- 1 to word.length) {
      val prefix = word.take(i)
      prefixBloom.put(prefix)

      val last = prefix.last
      val transitionPrefix = prefix.takeRight(transitionLength + 1).dropRight(1)

      val set = if (transitions.contains(transitionPrefix)) {
        transitions(transitionPrefix)
      } else {
        mutable.Set[Char]()
      }

      set.add(last)
      transitions.put(transitionPrefix, set)
    }
  }

  private def getTransitions(ngram: String): mutable.Set[Char] = {
    if (transitions.contains(ngram)) {
      transitions(ngram)
    } else {
      mutable.Set[Char]()
    }
  }

  def exactMatch(word: String): Boolean = {
    finalWordsBloom.mightContain(word)
  }

  def fuzzyMatch(originWord: String, errors: Int): List[String] = {
    val res = mutable.Set[String]()
    fuzzyMatchInternal(originWord, 0, "", errors, res, _ - 1)
    res.toList
  }

  def fuzzyMatchCont(originWord: String, errors: Int): List[String] = {
    val res = mutable.Set[String]()
    fuzzyMatchInternal(originWord, 0, "", errors, res, identity[Int])
    res.toList
  }

  def fuzzyMatchInternal(originWord: String, curLength: Int, curPrefix: String, errors: Int, mutableResult: mutable.Set[String], errorFunc: Int => Int, limit: Int = 100): Unit = {
    if (errors >= 0 && finalWordsBloom.mightContain(curPrefix) && Math.abs(originWord.length - curLength) <= errors) {
      mutableResult.add(curPrefix)
    }

    if (errors >= 0 && mutableResult.size <= limit) {
      val next: mutable.Set[Char] = getTransitions(curPrefix.takeRight(transitionLength))
      for {
        c <- next
      } {
        val str = curPrefix + c
        if (prefixBloom.mightContain(str)) {
          if (curLength < originWord.length && originWord(curLength) == c) {
            fuzzyMatchInternal(originWord, curLength + 1, str, errors, mutableResult, errorFunc)
          } else if (curLength < originWord.length && originWord(curLength) != c) {
            fuzzyMatchInternal(originWord, curLength + 1, str, errors - 1, mutableResult, errorFunc)
            fuzzyMatchInternal(originWord, curLength, str, errors - 1, mutableResult, errorFunc)
          } else if (curLength >= originWord.length) {
            fuzzyMatchInternal(originWord, curLength, str, errorFunc(errors), mutableResult, errorFunc)
          }
        }
      }
    }
  }
}