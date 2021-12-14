package org.calendar

import org.scalatest.funspec.AnyFunSpec

class Day08Test extends AnyFunSpec {

  val data1: List[List[String]] =
    """
      |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
      |""".stripMargin
      .split("\n")
      .tail
      .map(_.split('|').tail.head.trim)
      .map(_.split(" ").map(_.trim).toList)
      .toList

  val data2: List[String] = List("cdfeb", "fcadb", "cdfeb", "cdbaf")

  import Day08._

  describe("Part 1") {
    it("should count the number of 1, 4, 7, and 8s that appear") {
      val result = countDigitsBySizes(data1.flatten, List(1, 4, 7, 8))
      assertResult(26)(result)
    }
  }

  describe("Part 2") {
    it("should parse digits") {
      assertResult(5)(getDigitFromPattern(data2(0).toList))
      assertResult(3)(getDigitFromPattern(data2(3).toList))
      assertResult(5)(getDigitFromPattern(data2(0).toList))
      assertResult(3)(getDigitFromPattern(data2(3).toList))
    }

    it("should decode numbers") {
      assertResult(5353)(decodeDigits(data2))
    }
  }
}
