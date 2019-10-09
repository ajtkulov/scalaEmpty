package main

import com.google.common.hash.{BloomFilter, Funnels}
import infrastructure.FileUtils
import infrastructure.FileUtils.FileName

import scala.collection.mutable

object Main extends App {
  override def main(args: Array[String]): Unit = {
    Stats.transform("all_names.csv", "output.txt")

    //    Stats.stats("all_names.csv")

    //    val trie = read("all_names.csv")
    //    println(trie.fuzzyMatchCont("LARRY", 0))
  }

  def read(fileName: String): Trie = {
    val trie = Trie(1000000)
    val iter = scala.io.Source.fromFile(fileName).getLines().take(1000).map(x => CompanyUtils.normalize(x))
    iter.foreach(word => trie.addWord(word))

    trie
  }
}

object CompanyUtils {
  def normalize(name: String): String = {
    name.map {
      case x if !x.isLetter => ' '
      case x => x
    }.toUpperCase.split(" ").filter(_ != "").mkString(" ")
  }
}

object Stats {
  def stats(fileName: String) = {
    val map = mutable.Map[String, Int]().withDefaultValue(0)
    scala.io.Source.fromFile(fileName).getLines().map(CompanyUtils.normalize).flatMap(_.split(" ")).foreach {
      word => map(word) = map(word) + 1
    }

    map.toList.sortBy(_._2)(Ordering[Int].reverse).take(1000).foreach(println)
  }

  def transform(source: FileName, dest: FileName): Unit = {
    FileUtils.write(dest, scala.io.Source.fromFile(source).getLines().map(CompanyUtils.normalize))
  }
}

case class Trie(wordsAmount: Int, error: Double = 1e-9, avgWordLength: Int = 24) {
  lazy val charset = com.google.common.base.Charsets.US_ASCII

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