package com.adventofcode

import com.adventofcode.utils.CollectionUtils.{FrequencyOps, SeqOps}
import com.adventofcode.utils.FileUtils.getLines

import scala.annotation.tailrec

object Day09 extends App {
  val points = getLines("input09.txt") { line =>
    line.split("").map(_.toInt)
  }.toArray

  val iRange = points.indices
  val jRange = points.head.indices

  val neighboursDxDy = List(
    -1 -> 0,
    1 -> 0,
    0 -> -1,
    0 -> 1
  )

  private def inRange(i: Int, j: Int): Boolean =
    iRange.contains(i) && jRange.contains(j)

  private def isLowPoint(i: Int, j: Int): Boolean = {
    val x = points(i)(j)

    neighboursDxDy
      .collect { case (dx, dy) if inRange(i + dx, j + dy) => points(i + dx)(j + dy) }
      .forall(_ > x)
  }

  val lowPoints = for {
    i <- points.indices
    j <- points.head.indices
    if isLowPoint(i, j)
  } yield points(i)(j)

  val riskPoints = lowPoints.map(_ + 1)
  println(riskPoints.sum)

  for {
    i <- iRange
    j <- jRange
    x = points(i)(j)
    newValue = if (x == 9) -1 else 0
  } points(i)(j) = newValue

  private def findPointWithoutBasin(): Option[(Int, Int)] =
    (for {
      i <- iRange
      j <- jRange
      if points(i)(j) == 0
    } yield i -> j).headOption

  @tailrec
  private def buildBasins(n: Int, i: Int, j: Int): Unit = {

    def feel(i: Int, j: Int): Unit = {
      points(i)(j) = n

      val freeNeighbours = for {
        (di, dj) <- neighboursDxDy
        ni = i + di
        nj = j + dj
        if inRange(ni, nj) && points(ni)(nj) == 0
      } yield (ni, nj)

      freeNeighbours.foreach((feel _).tupled)
    }

    feel(i, j)

    findPointWithoutBasin() match {
      case None         => // exit
      case Some((i, j)) => buildBasins(n + 1, i, j)
    }
  }

  findPointWithoutBasin().foreach { case (x, y) =>
    buildBasins(1, x, y)
  }

  val biggestBasins = points.flatten.toList.frequency.filter(_._1 != -1).mostFrequentN(3)

  println(biggestBasins.values.product)
}
