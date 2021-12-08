package com.adventofcode

import com.adventofcode.utils.FileUtils.getStrings

object Day07 extends App {
  val positions = getStrings("input07.txt").flatMap(_.split(",").map(_.toInt).toList)

  val minX = positions.min
  val maxX = positions.max

  private def getFuel1(pos: Int): Int =
    positions.map(i => (pos - i).abs).sum

  val pos1 = (minX to maxX).minBy(getFuel1)
  println(getFuel1(pos1))

  private def sumN(n: Int): Int =
    n * (n + 1) / 2

  private def getFuel2(pos: Int): Int =
    positions
      .map(i => (pos - i).abs)
      .map(sumN)
      .sum

  val pos2 = (minX to maxX).minBy(getFuel2)
  println(getFuel2(pos2))
}
