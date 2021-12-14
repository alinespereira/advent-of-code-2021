import org.calendar.Day07._

import scala.math.abs

val positions = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

(0 until positions.max).map { pos1 =>
  positions
    .filterNot(_ == pos1)
    .map(pos2 => increasingCost(abs(pos1 - pos2)))
    .sum
}.min