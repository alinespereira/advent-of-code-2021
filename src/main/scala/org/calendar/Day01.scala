package org.calendar

import scala.io.Source

object Day01 extends App {
  val inputFile: String = "day01.txt"

  def getData(fileName: String): List[Int] = Source.fromURL(getClass.getResource(fileName))
    .getLines
    .map(_.toInt)
    .toList

  def increased(measures: List[Int]): Int = {
    measures.sliding(2, 1).map {
      case List(prev, curr) => if (curr > prev) 1 else 0
      case _ => 0
    }.sum
  }

  def increasedWindow(measures: List[Int]): Int = {
    measures.sliding(3, 1)
      .map(_.sum)
      .toList
      .sliding(2, 1)
      .map {
        case List(prev, curr) => if (curr > prev) 1 else 0
        case _ => 0
      }.sum
  }

  val data = getData(inputFile)
  println(increasedWindow(data))

  def main(args: List[String]): Unit = {

  }
}
