package org.calendar

import org.scalatest.funspec.AnyFunSpec

class Day05Test extends AnyFunSpec {

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

  import Day05._

  describe("parser") {
    it("should get a Point from a string of coordinates") {
      val examples = List("0, 9", "0,9", "   0,9   ", "  0, 9", "0, 9  ")
      assert(
        examples.forall(pointString => Point.fromString(pointString).isSuccess)
      )
      assert(
        examples.forall(pointString =>
          Point.fromString(pointString).get == Point(0, 9)
        )
      )
    }

    it("should fail when a string representing the point is malformed") {
      val examples = List("0, 9,0", "", "0,9,18", "0,,9", "9")
      assert(
        examples.forall(pointString => Point.fromString(pointString).isFailure)
      )
      examples.map(pointString =>
        assertThrows[IllegalArgumentException](
          Point.fromString(pointString).get
        )
      )
    }

    it("should get a Line from a string of points") {
      val examples = List(
        "0, 9 -> 5,9",
        "0,9->5,9",
        "0, 9   ->  5,9",
        "     0, 9 -> 5  ,9",
        "0, 9 -> 5,9   "
      )
      assert(
        examples.forall(lineString => Line.fromString(lineString).isSuccess)
      )
      assert(
        examples.forall(lineString =>
          Line.fromString(lineString).get == Line(Point(0, 9), Point(5, 9))
        )
      )
    }

    it("should fail when a string representing the line is malformed") {
      val examples = List(
        "0, 9 --> 5,9",
        "0,9->",
        "->  5,9",
        "0, 9 ->> 5  ,9",
        "0, 9 5,9   "
      )
      assert(
        examples.forall(pointString => Point.fromString(pointString).isFailure)
      )
      examples.map(lineString =>
        assertThrows[IllegalArgumentException](
          Line.fromString(lineString).get
        )
      )
    }
  }

  describe("hydrothermal vents") {
    it("should get the total number of crossing vents in each point") {
      val grid = makeGrid()
      val parsedData = data.map(Line.fromString).map(_.get)
      val filteredLines =
        parsedData.filter(line => line.isHorizontal || line.isVertical)
      val resGrid = countCrossingVents(filteredLines, grid)

      assertResult(5)(resGrid.values.count(_.getOrElse(0) >= 2))
    }

    it(
      "should get the total number of crossing vents in each point (considering diagonals)"
    ) {
      val grid = makeGrid()
      val parsedData = data.map(Line.fromString).map(_.get)
      val resGrid = countCrossingVents(parsedData, grid)

      assertResult(12)(resGrid.values.count(_.getOrElse(0) >= 2))
    }
  }
}
