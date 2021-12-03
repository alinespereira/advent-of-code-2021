import org.calendar.Day03
import scala.math.pow

val solution = Day03

val data = List("00100", "11110", "10110", "10111", "10101", "01111", "00111",
                "11100", "10000", "11001", "00010", "01010")

solution.parseReport(data, _.toList.map(_.asDigit)).length

val countOnes: List[Int] = solution.parseReport(data, _.toList.map(_.asDigit))
  .foldLeft(List[Int]())({ (acc, record) => {
    acc.zipAll(record, 0, 0).map({ case (x, y) => x + y})
  }})
val countZeros: List[Int] = countOnes.map(data.length - _)

val gammaRate = countZeros.zip(countOnes)
  .map({case (zero, one) => if (zero > one) 0 else 1})
  .reverse
  .zipWithIndex
  .foldLeft(0)({ case (acc, (bit, exponent)) =>
    acc + bit * pow(2, exponent).toInt
  })

val epsilonRate = countZeros.zip(countOnes)
  .map({case (zero, one) => if (zero < one) 0 else 1})
  .reverse
  .zipWithIndex
  .foldLeft(0)({ case (acc, (bit, exponent)) =>
      acc + bit * pow(2, exponent).toInt
  })

