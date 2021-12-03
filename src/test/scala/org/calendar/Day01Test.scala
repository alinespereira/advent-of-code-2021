package org.calendar

import org.scalatest.funspec.AnyFunSpec

import org.calendar.Day01

class Day01Test extends AnyFunSpec {
  val data = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
  val solution = Day01

  describe("increased") {
    it("should count when a measure is bigger than the previous") {
      assertResult(7)(solution.increased(data))
    }
  }

  describe("increasedWindow") {
    it(
      "should count when the sum of 3 measures is bigger than the sum of previous 3"
    ) {
      assertResult(5)(solution.increasedWindow(data))
    }
  }
}
