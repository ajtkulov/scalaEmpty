package main

import info.folone.ddl.DDLParser
import IteratorUtils._

object Main extends App {
  override def main(args: Array[String]): Unit = {
  }

  def parseFile(fileName: String) = {
    val iter = scala.io.Source.fromFile(fileName).getLines().splitBy(line => line.trim.isEmpty || line.startsWith("INSERT INTO")).map(_.mkString("\n"))

    iter.map(sql => (sql, DDLParser.parse(sql)))
  }
}