package com.adventofcode

import com.adventofcode.utils.FileUtils.getStrings

object Day17 extends App {
  case class Probe(x: Int, y: Int, dx: Int, dy: Int) {
    def next: Probe = {
      Probe(
        x + dx,
        y + dy,
        if (dx > 0) dx - 1 else if (dx < 0) dx + 1 else 0,
        dy - 1
      )
    }

    def target(xRange: Range, yRange: Range): Boolean =
      xRange.contains(x) && yRange.contains(y)
  }

  val target = "target area: x=(\\d+)..(\\d+), y=(-\\d+)..(-\\d+)".r

  val target(xFromStr, xToStr, yFromStr, yToStr) = getStrings("input17.txt").head

  val xFrom = xFromStr.toInt
  val xTo = xToStr.toInt
  val xRange = xFrom to xTo
  val yFrom = yFromStr.toInt
  val yTo = yToStr.toInt
  val yRange = yFrom to yTo

  private def getHighest(x: Int, y: Int): Option[Int] = {
    val probe = Probe(0, 0, x, y)
    val probes = LazyList.iterate(probe)(_.next).takeWhile(p => p.x <= xTo && p.y >= yFrom)
    if (probes.exists(_.target(xRange, yRange))) {
      Some(probes.maxBy(_.y).y)
    } else None
  }

  val result = for {
    i <- 0 to xTo
    j <- yFrom to 1000
    highest <- getHighest(i, j)
  } yield (i, j, highest)

  println(result.maxByOption(_._3))
  println(result.size)
}
