package com.adventofcode

import com.adventofcode.utils.FileUtils.getLines

import scala.annotation.tailrec

object Day15 extends App {
  case class Cell(x: Int, y: Int)

  case class State(cell: Cell, value: Int)

  val cavern = getLines("input15.txt")(_.split("").map(_.toInt)).toArray

  private def repeat(tile: Array[Array[Int]], n: Int): Array[Array[Int]] = {
    val tiles: Map[Int, Array[Array[Int]]] = (for {
      i <- 0 until (n * 2)
    } yield i -> tile.transpose.transpose.map(_.map { e =>
      val newE = e + i
      if (newE <= 9) newE else newE - 9
    })).toMap

    val maxX = tile.length
    val maxY = tile.head.length

    val result = Array.fill(maxX * n)(Array.fill(maxY * n)(0))

    for {
      ai <- 0 until n
      aj <- 0 until n
      i <- 0 until maxX
      j <- 0 until maxY
    } {
      result(ai * maxX + i)(aj * maxY + j) = tiles(ai + aj)(i)(j)
    }

    result
  }

  private def printArray(a: Array[Array[Int]]): Unit = {
    println(a.map(_.mkString(",")).mkString("\n"))
    println()
  }

  private def solve(matrix: Array[Array[Int]]): Int = {
    val maxX = matrix.length
    val maxY = matrix.head.length
    val result = Array.fill(maxX)(Array.fill(maxY)(Int.MaxValue))

    @tailrec
    def loop(toVisit: Set[Cell], n: Int): Unit = {
      def inRange(x: Int, y: Int): Boolean =
        0 <= x && x < n && 0 <= y && y < n

      def getNeighbourStates(cell: Cell): List[State] =
        List(0 -> 1, 0 -> -1, 1 -> 0, -1 -> 0)
          .map { case (dx, dy) =>
            (cell.x + dx) -> (cell.y + dy)
          }
          .collect {
            case (x, y) if inRange(x, y) =>
              State(Cell(x, y), result(cell.x)(cell.y) + matrix(x)(y))
          }
          .filter(ns => result(ns.cell.x)(ns.cell.y) > ns.value)

      val changes = toVisit
        .flatMap(getNeighbourStates)
        .groupBy(_.cell)
        .view
        .mapValues(_.minBy(_.value).value)
        .toMap

      changes.foreach { case (Cell(x, y), i) =>
        result(x)(y) = i
      }

      if (changes.isEmpty) {
        if (n == maxX) {
          ()
        } else {
          val borders =
            (0 until n).flatMap(i => List(Cell(i, n - 1), Cell(n - 1, i))).toSet
          loop(borders, n + 1)
        }
      } else loop(changes.keySet, n)
    }

    result(0)(0) = matrix(0)(0)
    loop(Set(Cell(0, 0)), 1)

    result(maxX - 1)(maxY - 1) - result(0)(0)
  }

  println(solve(repeat(cavern, 1)))
  println(solve(repeat(cavern, 5)))

}
