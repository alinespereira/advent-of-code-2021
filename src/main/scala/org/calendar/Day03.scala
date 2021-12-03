package org.calendar

import scala.annotation.tailrec
import scala.math.pow

object Day03 extends App with PuzzleResource {
  val inputFile = "day03.txt"

  private def reportCounter(data: List[String]): List[(Int, Int)] = {
    val countOnes: List[Int] = parseData(data, _.toList.map(_.asDigit))
      .foldLeft(List[Int]()) { (acc, record) =>
        acc.zipAll(record, 0, 0).map({ case (x, y) => x + y })
      }
    val countZeros: List[Int] = countOnes.map(data.length - _)

    countZeros.zip(countOnes)
  }

  private def toDecimal(bits: List[Int]): Int =
    bits.reverse.zipWithIndex
      .foldLeft(0)({
        case (acc, (bit, exponent)) =>
          acc + bit * pow(2, exponent).toInt
      })

  def powerConsumption(data: List[String]): Int = {
    val gammaRate = toDecimal(
      reportCounter(data)
        .map({ case (zero, one) => if (zero > one) 0 else 1 })
    )

    val epsilonRate = toDecimal(
      reportCounter(data)
        .map({ case (zero, one) => if (zero < one) 0 else 1 })
    )

    gammaRate * epsilonRate
  }

  def lifeSupport(data: List[String]): Int = {

    @tailrec
    def filterData(
      bitFilter: (Int, Int) => Int
    )(data: List[String], index: Int = 0): String =
      if (data.length == 1) data.head
      else {
        val counter = reportCounter(data)
        val bit = bitFilter(counter(index)._1, counter(index)._2)
        filterData(bitFilter)(
          data.filter(p => p(index).asDigit == bit),
          index + 1
        )
      }

    def oxygenFilter(zeros: Int, ones: Int): Int =
      if (zeros > ones) 0 else 1

    def scrubberFilter(zeros: Int, ones: Int): Int =
      if (zeros <= ones) 0 else 1

    val oxygenRating = toDecimal(
      filterData(oxygenFilter)(data).toList.map(_.asDigit)
    )
    val scrubberRating = toDecimal(
      filterData(scrubberFilter)(data).toList.map(_.asDigit)
    )

    oxygenRating * scrubberRating

  }

  val data = getData[String](inputFile)
  println(s"Power consumption: ${powerConsumption(data)}")
  println(s"Life support: ${lifeSupport(data)}")
}
