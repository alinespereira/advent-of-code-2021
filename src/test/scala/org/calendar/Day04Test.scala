package org.calendar

import org.scalatest.funspec.AnyFunSpec

class Day04Test extends AnyFunSpec {

  val data =
    """
      |7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7
      |""".stripMargin.trim.split("\n").toList

  import Day04._

  describe("bingo") {
    it("should get a list of non negative drawn numbers") {
      val allNonNegative = !parseDrawnNumbers(data).exists(_ < 0)
      assert(allNonNegative)
    }

    it("should get no score at start of the game") {
      val boards = parseBoards(data)
      val scores = boards.map(_.score)
      assert(scores.forall(_.isEmpty))
    }

    it("should get the final scores of the game") {
      val drawnNumbers = parseDrawnNumbers(data)
      val boards = parseBoards(data)
      val (winnerScore, worstScore) = runGame(drawnNumbers, boards)
      assertResult(Some(4512))(winnerScore)
      assertResult(Some(1924))(worstScore)
    }
  }
}
