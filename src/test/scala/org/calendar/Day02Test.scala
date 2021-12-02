package org.calendar

import org.scalatest.funspec.AnyFunSpec

import org.calendar.Day02

class Day02Test extends AnyFunSpec {
  val data = List("forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2")

  val solution = Day02

  describe("dive") {
    it("should get the product of final coordinates") {
      assert(solution.dive(data) == 150)
    }
  }

  describe("diveAim") {
    it("should get the product of final coordinates") {
      assert(solution.diveAim(data) == 900)
    }
  }
}
