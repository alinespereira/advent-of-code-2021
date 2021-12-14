package org.calendar

object Day08 extends PuzzleResource {
  override val inputFile: String = "day08.txt"

  def countDigitsBySizes(data: List[String], digits: List[Int]): Int = {
    val sizes =
      digitSizes.filter({ case (num, _) => digits.contains(num) }).values.toList
    data
      .map(_.length)
      .count(sizes.contains)
  }

  def decodeDigits(encoded: List[String]): Int =
    encoded
      .map(s => getDigitFromPattern(s.toList).toString)
      .mkString("")
      .toInt

  def getDigitFromPattern(pattern: List[Char]): Int =
    digitPatterns
      .filter({ case (_, pat) => pattern.sorted.equals(pat) })
      .keys
      .head

  val digitSizes: Map[Int, Int] = Map(
    0 -> 6,
    1 -> 2,
    2 -> 5,
    3 -> 5,
    4 -> 4,
    5 -> 5,
    6 -> 6,
    7 -> 3,
    8 -> 7,
    9 -> 6
  )

  val digitPatterns: Map[Int, List[Char]] = Map(
    0 -> "cagedb".toList.sorted,
    1 -> "ab".toList.sorted,
    2 -> "gcdfa".toList.sorted,
    3 -> "fbcad".toList.sorted,
    4 -> "eafb".toList.sorted,
    5 -> "cdfbe".toList.sorted,
    6 -> "cdfgeb".toList.sorted,
    7 -> "dab".toList.sorted,
    8 -> "acedgfb".toList.sorted,
    9 -> "cefabd".toList.sorted
  )

  def main(args: Array[String]): Unit = {
    val data = getData(inputFile, _.split('|').tail.head.trim)
      .map(_.split(" ").map(_.trim).toList)

    val part1 = countDigitsBySizes(data.flatten, List(1, 4, 7, 8))
    println(s"Digits with unique sizes: $part1")

    val part2 = data.map(row => decodeDigits(row)).sum

    println(s"Sum of all digits: $part2")
  }
}
