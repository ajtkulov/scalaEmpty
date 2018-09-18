package main

import java.io.{File, PrintWriter}

object FileUtils {
  def write(fileName: String, value: String): Unit = {
    val file = new File(fileName)
    val write = new PrintWriter(file)
    try {
      write.print(value)
    } finally {
      write.close()
    }
  }

  def read(fileName: String): String = {
    scala.io.Source.fromFile(fileName).getLines().toList.mkString("\n")
  }
}
