package org.calendar

import scala.io.Source
import scala.util.Using

trait PuzzleResource {
  val inputFile: String

  def getData[T](fileName: String, converter: String => T): List[T] =
    Using(Source.fromURL(getClass.getResource(fileName))) { source =>
      source
        .getLines()
        .map(converter)
        .toList
    }.get

  def parseData[T](data: List[String], parser: String => T): List[T] =
    data.map(parser)
}
