package org.calendar

import scala.io.Source

trait PuzzleResource {
  def getData[T](fileName: String): List[T] = Source.fromURL(getClass.getResource(fileName))
    .getLines
    .map(_.asInstanceOf[T])
    .toList
}
