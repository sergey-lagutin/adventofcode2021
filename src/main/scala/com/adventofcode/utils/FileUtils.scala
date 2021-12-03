package com.adventofcode.utils

object FileUtils {
  def getLines[T](resource: String)(f: String => T): List[T] =
    io.Source.fromResource(resource).getLines().map(f).toList

  def getInts(resource: String): List[Int] =
    getLines(resource)(_.toInt)

  def getStrings(resource: String): List[String] =
    getLines(resource)(identity)
}
