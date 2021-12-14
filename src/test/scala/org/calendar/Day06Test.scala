package org.calendar

import org.scalatest.funspec.AnyFunSpec

class Day06Test extends AnyFunSpec {
  val data = "3,4,3,1,2"

  import org.calendar.Day06._

  val fish = data
    .split(",")
    .map(fish => LanternFish(fish.toInt))
    .toList
    .groupBy(_.timer)
    .map({
      case (timer: Int, fish: List[LanternFish]) =>
        LanternFish(timer) -> fish.length
    })

  describe("LanterFish") {
    it("evolves in 1 day") {
      val result = evolve(fish, 1)
      assertResult(5)(result.values.sum)
    }

    it("evolves in 4 days") {
      val result = evolve(fish, 4)
      assertResult(9)(result.values.sum)
    }

    it("evolves a lot") {
      val examples = List(
        (0, 5),
        (1, 5),
        (2, 6),
        (3, 7),
        (4, 9),
        (5, 10),
        (6, 10),
        (7, 10),
        (8, 10),
        (9, 11)
      )

      examples.map({
        case (days, expected) =>
          val result = evolve(fish, days)
          assertResult(expected)(result.values.sum)
      })
    }

    it("evolves in 7 days") {
      val result = evolve(fish, 7)
      assertResult(10)(result.values.sum)
    }

    it("evolves in 18 days") {
      val result = evolve(fish, 18)
      assertResult(26)(result.values.sum)
    }

    it("evolves in 80 days") {
      val result = evolve(fish, 80)
      assertResult(5934)(result.values.sum)
    }

    it("evolves in 256 days") {
      val result = evolve(fish, 256)
      assertResult(26984457539L)(result.values.sum)
    }
  }
}
