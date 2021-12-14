import org.calendar.Day06._

//val data = "3,4,3,1,2"
val data = "3,4,3,1,2,0,7"

val fish = data
  .split(",")
  .map(fish => LanternFish(fish.toInt))
  .toList
  .groupBy(_.timer)
  .map({
    case (timer: Int, fish: List[LanternFish]) =>
      LanternFish(timer) -> fish.length
  })

val agedFish = fish
  .filter(_._1.timer != 0)
  .map {
    case (f, n) => f.evolve -> n
  }

val newFish = fish.filter(_._1.timer == 0).values.headOption.getOrElse(0)

val evolvedFish = agedFish + (LanternFish() -> newFish)

println(fish.mkString("\n"))

println(agedFish.mkString("\n"))

println(evolvedFish.mkString("\n"))

println("abc")

val newEmptyGen: Map[LanternFish, Int] = List
  .fill(8 + 1)(0)
  .zipWithIndex
  .map(p => (LanternFish(p._2), p._1))
  .toMap

evolve(fish, 1)