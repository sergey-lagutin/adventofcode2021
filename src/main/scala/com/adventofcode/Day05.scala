package com.adventofcode

import com.adventofcode.utils.CollectionUtils.SeqOps
import com.adventofcode.utils.FileUtils.getLines

object Day05 extends App {
  case class Point(x: Int, y: Int)

  case class Line(start: Point, end: Point) {
    def isHorizontal: Boolean = start.x == end.x
    def isVertical: Boolean = start.y == end.y

    def points: Seq[Point] = {
      val length = (start.x - end.x).abs.max((start.y - end.y).abs) + 1
      val dx = if (start.x < end.x) 1 else if (start.x > end.x) -1 else 0
      val dy = if (start.y < end.y) 1 else if (start.y > end.y) -1 else 0
      for {
        i <- 0 until length
      } yield Point(start.x + i * dx, start.y + i * dy)
    }
  }

  val lineRegex = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r

  val lines = getLines("input05.txt") { case lineRegex(x1, y1, x2, y2) =>
    Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
  }

  val result1 = lines
    .filter(l => l.isHorizontal || l.isVertical)
    .flatMap(_.points)
    .frequency
    .count {
      _._2 > 1
    }

  println(result1)

  val result2 = lines
    .flatMap(_.points)
    .frequency
    .count {
      _._2 > 1
    }

  println(result2)
}
