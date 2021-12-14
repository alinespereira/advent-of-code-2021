package org.calendar

import scala.math.{abs, atan2, pow, sqrt}
import scala.util.Try

object Day05 extends PuzzleResource {
  val inputFile: String = "day05.txt"

  case class Point(x: Int, y: Int) {

    def sub(point: Point): Vector =
      Vector(x - point.x, y - point.y)

    override def toString: String = s"($x, $y)"
  }

  case object Point {

    def fromString(point: String): Try[Point] = Try {
      point.trim.split(",").map(_.trim.toInt) match {
        case Array(x, y) => Point(x, y)
        case _ =>
          throw new IllegalArgumentException(s"Malformed point string: $point")
      }
    }
  }

  case class Vector(x: Double, y: Double) {
    val magnitude: Double = sqrt(pow(x, 2) + pow(y, 2))
    val angle: Double = atan2(y, x)

    def isHorizontal: Boolean = y == 0 && x != 0

    def isVertical: Boolean = x == 0 && y != 0

    def normalize: Vector = Vector(x / magnitude, y / magnitude)

    def cross(vector: Vector): Double =
      (x * vector.y) - (y * vector.x)

    def dot(vector: Vector): Double =
      x * vector.x + y * vector.y

    def collinear(vector: Vector): Boolean = cross(vector) == 0

    def scale(factor: Double): Vector =
      Vector(x * factor, y * factor)

    def projection(vector: Vector): Vector =
      scale(vector.dot(this) / dot(this))
  }

  case class Line(origin: Point, destination: Point) {
    val direction: Vector = destination.sub(origin)

    def isHorizontal: Boolean = direction.isHorizontal

    def isVertical: Boolean = direction.isVertical

    def contains(point: Point): Boolean = {
      val pointVector = point.sub(origin)
      val vectorProjection = direction.projection(pointVector)
      vectorProjection.magnitude <= direction.magnitude && pointVector
        .collinear(direction) && pointVector.dot(direction) >= 0
    }

    override def toString: String = s"$origin -> $destination"
  }

  case object Line {

    def fromString(line: String): Try[Line] = Try {
      line.trim
        .split("->")
        .map(pointString => Point.fromString(pointString).get) match {
        case Array(x: Point, y: Point) => Line(x, y)
        case _ =>
          throw new IllegalArgumentException(s"Malformed line string: $line")
      }
    }
  }

  type Grid = Map[Point, Option[Int]]

  val rows = 10
  val cols = 10

  def makeGrid(rows: Int = rows, cols: Int = cols): Grid =
    List
      .fill(rows * cols)(None)
      .zipWithIndex
      .map({ case (value, index) => Point(index / rows, index % rows) -> value }
      )
      .toMap

  def countCrossingVents(ventLines: List[Line], grid: Grid): Grid =
    grid.map({
      case (point, value) =>
        val crossingVentsPoint = ventLines.count(_.contains(point))
        (
          point,
          if (value.isEmpty) Option(crossingVentsPoint)
          else value.map(_ + crossingVentsPoint)
        )
    })

  def main(args: Array[String]): Unit = {
    val data = getData(inputFile, Line.fromString)
      .map(_.get)

    val xBoundary = data.flatMap { line =>
      List(line.origin.x, line.destination.x)
    }.max

    val yBoundary = data.flatMap { line =>
      List(line.origin.y, line.destination.y)
    }.max

    val grid = makeGrid(xBoundary, yBoundary)
    val resGrid = countCrossingVents(
      data.filter(line => line.isVertical || line.isHorizontal),
      grid
    )
    println(s"Crossing vents: ${resGrid.values.count(_.getOrElse(0) >= 2)}")

    val resGridDiagonals = countCrossingVents(data, grid)
    println(
      s"Crossing vents: ${resGridDiagonals.values.count(_.getOrElse(0) >= 2)}"
    )
  }
}
