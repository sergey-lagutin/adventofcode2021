package com.adventofcode

import com.adventofcode.utils.CollectionUtils.{FrequencyOps, SeqOps}
import com.adventofcode.utils.FileUtils.getLines

import scala.annotation.tailrec

object Day12 extends App {
  case class Cave(code: String) {
    lazy val isSmall: Boolean = code.toLowerCase == code
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
    lazy val visitedSmallCaves = path.filter(_.isSmall)
    lazy val visitedSmallCavesFrequency = path.filter(_.isSmall).frequency
    lazy val mostVisited = visitedSmallCavesFrequency(visitedSmallCavesFrequency.mostFrequent)
    val head = path.head
    edgeMap(head.code)
      .filter(_ != start)
      .filter(cave => !cave.isSmall || !visitedSmallCaves.contains(cave) || mostVisited == 1)
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
