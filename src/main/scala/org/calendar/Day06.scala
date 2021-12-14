package org.calendar

import scala.annotation.tailrec

object Day06 extends PuzzleResource {
  override val inputFile: String = "day06.txt"

  case class LanternFish(timer: Int = 6) {

    def evolve: LanternFish = timer match {
      case 0 => LanternFish()
      case _ => LanternFish(timer - 1)
    }
  }

  @tailrec
  def evolve(fish: Map[LanternFish, Int], days: Int): Map[LanternFish, Int] =
    if (days == 0) fish
    else {
      val newFish = fish.getOrElse(LanternFish(0), 0)
      val evolvedFish = fish.filter(_._2 != 0).map {
        case (f: LanternFish, count: Int) =>
          LanternFish(f.timer - 1) -> count
      }
      val resultingFish =
        evolvedFish +
          (LanternFish(8) -> newFish) +
          (LanternFish(6) -> (evolvedFish.getOrElse(LanternFish(6), 0) + fish.getOrElse(LanternFish(0), 0)))
      //      println(s"days: $days")
      //      println((evolvedFish + (LanternFish(8) -> newFish)).mkString("\n"))
      evolve(evolvedFish + (LanternFish(8) -> newFish), days - 1)
    }

  def main(args: Array[String]): Unit = {
    val base = getData(
      inputFile,
      _.split(",")
        .map(fish => LanternFish(fish.toInt))
        .toList
    ).head

    val data = base
      .groupBy(_.timer)
      .map({
        case (timer: Int, fish: List[LanternFish]) =>
          LanternFish(timer) -> fish.length
      })

    val result = evolve(data, 80)

    println(s"After 80 days: ${result.values.sum}")

    val result2 = evolve(data, 256)

    println(s"After 256 days: ${result2.values.sum}")

  }
}
