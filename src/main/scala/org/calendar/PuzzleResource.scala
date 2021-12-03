package org.calendar

import scala.io.Source
import scala.util.Using

trait PuzzleResource {

  def getData[T](fileName: String): List[T] =
    Using(Source.fromURL(getClass.getResource(fileName))) { source =>
      source.getLines
        .map(_.asInstanceOf[T])
        .toList
    }.get

  def parseData[T](data: List[String], parser: String => T): List[T] =
    data.map(parser)
}
