package org.calendar

import org.scalatest.funspec.AnyFunSpec

class Day07Test extends AnyFunSpec {
  val data = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

  import Day07._

  describe("Part 1") {
    it("should calculate the cheapest possible outcome with constant cost") {
      val result: Int = cheapestOutcome(data, constantCost)
      assertResult(37)(result)
    }
  }

  describe("Part 2") {
    it("should calculate costs in an arithmetic progression way") {
      (1 to 10).map(n => assertResult(Range(1, n + 1).sum)(increasingCost(n)))
    }

    it("should calculate the cheapest possible outcome with increasing cost") {
      val result: Int = cheapestOutcome(data, increasingCost)
      assertResult(168)(result)
    }
  }
}
