package org.calendar

import scala.math.abs

object Day07 extends PuzzleResource {
  override val inputFile: String = "day07.txt"

  def cheapestOutcome(positions: List[Int], cost: Int => Int): Int =
    (positions.min until positions.max).map { pos1 =>
      positions.filterNot(_ == pos1).map(pos2 => cost(abs(pos1 - pos2))).sum
    }.min

  def constantCost: Int => Int = identity

  def increasingCost: Int => Int = distance => (1 + distance) * distance / 2

  def main(args: Array[String]): Unit = {
    val data = getData(inputFile, _.split(",").map(_.trim.toInt).toList).head
    val part1 = cheapestOutcome(data, constantCost)
    val part2 = cheapestOutcome(data, increasingCost)

    println(s"The cheapest outcome is ${part1}")
    println(s"The cheapest outcome with increasing cost is ${part2}")
  }
}
