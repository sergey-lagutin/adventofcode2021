package com.adventofcode

import com.adventofcode.utils.FileUtils.getLines

object Day11 extends App {
  val octopuses: Array[Array[Int]] = getLines("input11.txt")(_.split("").map(_.toInt)).toArray

  val iRange = octopuses.indices
  val jRange = octopuses.head.indices

  var flashCount = 0

  val adjacent = (for {
    i <- -1 to 1
    j <- -1 to 1
  } yield i -> j).toList

  private def adjacentXY(i: Int, j: Int): List[(Int, Int)] =
    adjacent
      .map { case (dx, dy) =>
        (i + dx, j + dy)
      }
      .filter((inRange _).tupled)
      .filter(_ != (i, j))

  private def inRange(i: Int, j: Int): Boolean =
    iRange.contains(i) && jRange.contains(j)

  private def flash(i: Int, j: Int): Unit = {
    octopuses(i)(j) = octopuses(i)(j) + 1
    flashCount += 1

    adjacentXY(i, j).foreach { case (ni, nj) =>
      octopuses(ni)(nj) = octopuses(ni)(nj) + 1
      if (octopuses(ni)(nj) == 10)
        flash(ni, nj)
    }
  }

  private def step(): Unit = {
    for {
      i <- iRange
      j <- jRange
    } {
      if (octopuses(i)(j) == 9)
        flash(i, j)
      else octopuses(i)(j) = octopuses(i)(j) + 1
    }

    for {
      i <- iRange
      j <- jRange
      if octopuses(i)(j) > 9
    } {
      octopuses(i)(j) = 0
    }
  }

  (1 to 100).foreach(_ => step())

  println(flashCount)

  val result2 = LazyList.from(101).find { _ =>
    step()
    octopuses.forall(_.forall(_ == 0))
  }
  println(result2)
}
