package org.calendar

import scala.annotation.tailrec

object Day04 extends PuzzleResource {
  val inputFile: String = "day04.txt"

  case class BoardNumber(number: Int, checked: Boolean = false) {

    override def toString: String =
      if (!checked) f" $number%02d " else f"[$number%02d]"
  }

  case class Board(
    numbers: List[List[BoardNumber]],
    lastChecked: Option[Int] = None
  ) {

    override def toString: String =
      numbers
        .map(_.mkString("|", "|", "|"))
        .mkString(
          "|----|----|----|----|----|\n",
          "\n",
          "\n|----|----|----|----|----|"
        )

    def simulate(drawnNumbers: List[Int]): (Option[Int], Int) =
      drawnNumbers
        .scanLeft(this)((newBoard, drawn) => newBoard.check(drawn))
        .map(_.score)
        .zipWithIndex
        .dropWhile(_._1.isEmpty)
        .head

    def won: Boolean =
      numbers.exists(_.forall(_.checked)) ||
        numbers.transpose.exists(_.forall(_.checked))

    def check(number: Int): Board =
      Board(
        numbers.map(row =>
          row.map({
            case BoardNumber(num, checked) if num == number && !checked =>
              BoardNumber(number, checked = true)
            case boardNumber => boardNumber
          })
        ),
        Some(number)
      )

    def score: Option[Int] = {
      if (won)
        lastChecked.map(lastNumber =>
          numbers.flatten.filter(!_.checked).map(_.number).sum * lastNumber
        )
      else
        None
    }
  }

  def parseDrawnNumbers(data: List[String]): List[Int] =
    data.head.split(",").map(_.trim.toInt).toList

  def parseBoard(data: List[String]): Board = {
    val boardData = data
      .map(
        _.trim.split(raw"\s+").toList.map(number => BoardNumber(number.toInt))
      )
    Board(boardData)
  }

  def parseBoards(data: List[String]): List[Board] =
    data.tail
      .grouped(6)
      .toList
      .map(boardData => parseBoard(boardData.tail))

  def runGame(drawnNumbers: List[Int], boards: List[Board]): Option[Int] =
    boards.map(_.simulate(drawnNumbers)).minBy(_._2)._1

  def main(args: Array[String]): Unit = {
    val data = getData(inputFile, identity)
    val drawnNumbers = parseDrawnNumbers(data)
    val boards = parseBoards(data)
    val winnerScore = runGame(drawnNumbers, boards)
    winnerScore match {
      case Some(score) => println(s"Winner score is ${score}")
      case None => println("Nobody won")
    }
  }
}
