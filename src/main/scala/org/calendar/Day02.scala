package org.calendar

object Day02 extends App with PuzzleResource {
  val inputFile: String = "day02.txt"

  def parseCommand(command: String): (Int, Int) =
    command.split(" ") match {
      case Array("forward", value) => (value.toInt, 0)
      case Array("down", value)    => (0, value.toInt)
      case Array("up", value)      => (0, -value.toInt)
      case _                       => (0, 0)
    }

  def parseCommandAim(command: String): (Int, Int, Int, Boolean) =
    command.split(" ") match {
      case Array("forward", value) => (value.toInt, 0, 0, true)
      case Array("down", value)    => (0, 0, value.toInt, false)
      case Array("up", value)      => (0, 0, -value.toInt, false)
      case _                       => (0, 0, 0, false)
    }

  def parseCommands[T](commands: List[String], parser: String => T): List[T] =
    commands.map(parser)

  def dive(commands: List[String]): Int = {
    val finalPosition = parseCommands(commands, parseCommand)
      .foldLeft((0, 0))({
        case ((xAcc, yAcc), (xCommand, yCommand)) =>
          (xAcc + xCommand, yAcc + yCommand)
      })
    finalPosition._1 * finalPosition._2
  }

  def diveAim(commands: List[String]): Int = {
    val finalPosition = parseCommands(commands, parseCommandAim)
      .foldLeft((0, 0, 0))({
        case ((xAcc, yAcc, aimAcc), (xCommand, yCommand, aim, isForward)) =>
          if (isForward) {
            (xAcc + xCommand, yAcc + aimAcc * xCommand, aimAcc)
          } else {
            (xAcc, yAcc, aimAcc + aim)
          }
      })
    finalPosition._1 * finalPosition._2
  }

  val data = getData[String](inputFile)
  println(dive(data))

  println(diveAim(data))
}
