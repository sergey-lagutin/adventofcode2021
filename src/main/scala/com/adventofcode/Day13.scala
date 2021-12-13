package com.adventofcode

import com.adventofcode.utils.FileUtils.getStrings

import scala.annotation.tailrec

object Day13 extends App {
  case class Point(x: Int, y: Int)

  object Command {
    private val foldAlongX = "fold along x=(\\d+)".r
    private val foldAlongY = "fold along y=(\\d+)".r

    def apply(str: String): Command = str match {
      case foldAlongX(x) => FoldAlongX(x.toInt)
      case foldAlongY(y) => FoldAlongY(y.toInt)
    }
  }

  sealed trait Command

  case class FoldAlongX(y: Int) extends Command
  case class FoldAlongY(y: Int) extends Command

  val lines = getStrings("input13.txt")

  val points = lines
    .takeWhile(_.nonEmpty)
    .map { line =>
      val Array(x, y) = line.split(",")
      Point(x.toInt, y.toInt)
    }

  val commands = lines
    .drop(points.size + 1)
    .map(Command.apply)

  @tailrec
  private def applyCommands(points: Set[Point], commands: Seq[Command]): Set[Point] = {
    def applyCommand(c: Command)(p: Point): Point = {
      def update(i: Int, foldLine: Int): Int =
        if (i < foldLine) i
        else 2 * foldLine - i

      c match {
        case FoldAlongX(x) => Point(update(p.x, x), p.y)
        case FoldAlongY(y) => Point(p.x, update(p.y, y))
      }
    }

    commands.headOption match {
      case Some(head) => applyCommands(points.map(applyCommand(head)), commands.tail)
      case None       => points
    }
  }

  val result1 = applyCommands(points.toSet, commands.take(1))
  println(result1.size)

  private def printResult(points: Set[Point]): Unit = {
    val maxX = points.map(_.x).max
    val maxY = points.map(_.y).max

    val array = Array.fill(maxX + 1)(Array.fill(maxY + 1)(" "))
    points.foreach(p => array(p.x)(p.y) = "*")
    println(array.transpose.map(_.mkString("")).mkString("\n"))
  }

  printResult(applyCommands(points.toSet, commands))
}
