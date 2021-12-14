package org.calendar

import scala.annotation.tailrec

object Day06 extends PuzzleResource {
  override val inputFile: String = "day06.txt"

  val newFishTimer: Int = 8

  case class LanternFish(timer: Int = newFishTimer) {

    val timeToReplicate: Int = 6

    def evolve: LanternFish = timer match {
      case 0 => LanternFish(timeToReplicate)
      case _ => LanternFish(timer - 1)
    }
  }

  @tailrec
  def evolve(fish: Map[LanternFish, Long], days: Int): Map[LanternFish, Long] =
    if (days == 0) fish
    else {
      val resultingFish: Map[LanternFish, Long] = Map(
        LanternFish(8) -> fish.getOrElse(LanternFish(0), 0L),
        LanternFish(7) -> fish.getOrElse(LanternFish(8), 0L),
        LanternFish(6) -> (fish.getOrElse(LanternFish(7), 0L) + fish
          .getOrElse(LanternFish(0), 0L)),
        LanternFish(5) -> fish.getOrElse(LanternFish(6), 0L),
        LanternFish(4) -> fish.getOrElse(LanternFish(5), 0L),
        LanternFish(3) -> fish.getOrElse(LanternFish(4), 0L),
        LanternFish(2) -> fish.getOrElse(LanternFish(3), 0L),
        LanternFish(1) -> fish.getOrElse(LanternFish(2), 0L),
        LanternFish(0) -> fish.getOrElse(LanternFish(1), 0L)
      )
      evolve(resultingFish, days - 1)
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
          LanternFish(timer) -> fish.length.toLong
      })

    val result = evolve(data, 80)

    println(s"After 80 days: ${result.values.sum}")

    val result2 = evolve(data, 256)

    println(s"After 256 days: ${result2.values.sum}")

  }
}
