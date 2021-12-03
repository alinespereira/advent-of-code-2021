package org.calendar

import scala.io.Source

object Day01 extends App with PuzzleResource {
  val inputFile: String = "day01.txt"

  def increased(measures: List[Int]): Int =
    measures
      .sliding(2, 1)
      .map {
        case List(prev, curr) => if (curr > prev) 1 else 0
        case _                => 0
      }
      .sum

  def increasedWindow(measures: List[Int]): Int =
    measures
      .sliding(3, 1)
      .map(_.sum)
      .toList
      .sliding(2, 1)
      .map {
        case List(prev, curr) => if (curr > prev) 1 else 0
        case _                => 0
      }
      .sum

  val data = getData[Int](inputFile)
  println(increasedWindow(data))

  def main(args: List[String]): Unit = {}
}
