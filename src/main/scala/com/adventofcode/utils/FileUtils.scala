package com.adventofcode.utils

object FileUtils {
  def getLines[T](resource : String)(f : String => T) : List[T] =
    io.Source.fromResource(resource).getLines().map(f).toList
}
