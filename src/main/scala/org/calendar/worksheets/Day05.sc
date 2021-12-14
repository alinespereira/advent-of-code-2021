import org.calendar.Day05._

import scala.math.pow

val rows = 5
val cols = 5

val data =
  """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin.trim.split("\n").toList

val lines = data
  .map(Line.fromString)
  .map(_.get)
  .filter(l => l.isHorizontal || l.isVertical)

val point = Point(2, 3)

val line = lines.filter(_.destination == Point(2, 1)).head

val pointVector = point.sub(line.origin)
val dotProduct = pointVector.dot(line.direction)
val crossProduct = pointVector.cross(line.direction)

line.direction
point.sub(line.origin)
line.direction.projection(pointVector).magnitude
line.contains(point)

lines.filter(_.contains(point))

val grid = makeGrid()

grid(point)


