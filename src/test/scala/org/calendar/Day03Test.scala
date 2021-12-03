package org.calendar

import org.scalatest.funspec.AnyFunSpec

class Day03Test extends AnyFunSpec {

  val data = List(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  val solution = Day03

  describe("powerConsumption") {
    it("should perform a submarine power consumption diagnostics") {
      assertResult(198)(solution.powerConsumption(data))
    }
  }

  describe("lifeSupport") {
    it("should perform a submarine life support diagnostics") {
      assertResult(230)(solution.lifeSupport(data))
    }
  }

}
