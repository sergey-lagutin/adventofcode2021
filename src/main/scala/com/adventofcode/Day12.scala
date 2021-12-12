package com.adventofcode

import com.adventofcode.utils.CollectionUtils.{FrequencyOps, SeqOps}
import com.adventofcode.utils.FileUtils.getLines

import scala.annotation.tailrec

object Day12 extends App {
  sealed trait Cave {
    def code: String
    def isSmall: Boolean
  }

  case class BigCave(code: String) extends Cave {
    override def isSmall: Boolean = false
  }

  case class SmallCave(code: String) extends Cave {
    override def isSmall: Boolean = true
  }

  object Cave {
    def apply(code: String): Cave =
      if (code.toLowerCase == code) SmallCave(code) else BigCave(code)
  }

  case class Edge(from: Cave, to: Cave)

  type Path = List[Cave]

  val edges = getLines("input12.txt") { line =>
    val Array(from, to) = line.split("-")
    Edge(Cave(from), Cave(to))
  }

  val edgeMap = edges
    .flatMap { edge =>
      List(
        edge,
        Edge(edge.to, edge.from)
      )
    }
    .groupBy(_.from)
    .map { case (from, list) =>
      from.code -> list.map(_.to)
    }

  private def visitSmallOnlyOnce(path: Path): List[Cave] = {
    val visitedSmallCaves = path.filter(_.isSmall).toSet
    val head = path.head
    edgeMap(head.code).filterNot(visitedSmallCaves)
  }

  private def visitOneSmallTwiceAndRestOnlyOnce(start: Cave)(path: Path): List[Cave] = {
    val visitedSmallCavesFrequency = path.filter(_.isSmall).frequency
    val visitedSmallCaves = path.filter(_.isSmall)
    val mostVisited = visitedSmallCavesFrequency(visitedSmallCavesFrequency.mostFrequent)
    val head = path.head
    edgeMap(head.code)
      .filter(_ != start)
      .filter(cave => !cave.isSmall || mostVisited == 1 || !visitedSmallCaves.contains(cave))
  }

  private def findPaths(start: Cave, end: Cave, candidateCaves: Path => List[Cave]): List[Path] = {
    @tailrec
    def loop(paths: List[Path]): List[Path] = {
      val (pathsToProlong, completedPaths) = paths.partition(_.head != end)
      if (pathsToProlong.isEmpty)
        paths
      else {
        val newPaths = pathsToProlong.flatMap { path =>
          val candidates = candidateCaves(path)
          candidates.map(_ :: path)
        }
        loop(newPaths ++ completedPaths)
      }
    }

    loop(List(List(start)))
  }

  val startCave = Cave("start")
  val endCave = Cave("end")

  val paths = findPaths(startCave, endCave, visitSmallOnlyOnce)
  println(paths.size)

  val paths2 = findPaths(startCave, endCave, visitOneSmallTwiceAndRestOnlyOnce(startCave))
  println(paths2.size)
}
