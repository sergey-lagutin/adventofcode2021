package com.adventofcode

import com.adventofcode.utils.FileUtils.getStrings

import scala.annotation.tailrec

object Day04 extends App {

  object Board {
    def apply(lines: List[String]): Board =
      Board(
        for {
          line <- lines.toArray
        } yield line.trim.split("\\s+").map(_.toInt)
      )
  }

  case class Board(field: Array[Array[Int]]) {
    def feed(number: Int): Board = {
      val newField = field.transpose.transpose
      for {
        i <- field.indices
        j <- field.head.indices
        if field(i)(j) == number
      } newField(i)(j) = -1
      Board(newField)
    }

    def bingo: Boolean = {
      field.exists(_.forall(_ == -1)) ||
      field.head.indices.exists(j => field.indices.forall(i => field(i)(j) == -1))
    }

    def missedNumbers: List[Int] =
      field.flatten.filterNot(_ == -1).toList

    override def toString: String = {
      field.map(_.mkString(",")).mkString("\n", "\n", "\n")
    }
  }

  private def readBoards(lines: List[String]): List[Board] = {
    @tailrec
    def loop(acc: List[Board], lines: List[String]): List[Board] =
      if (lines.size < 5) acc
      else
        loop(Board(lines.take(5)) :: acc, lines.drop(6))

    loop(Nil, lines)
  }

  @tailrec
  private def winFirst(numbers: List[Int], boards: List[Board]): (Int, Board) =
    numbers match {
      case Nil => throw new IllegalStateException("no more numbers")
      case number :: rest =>
        val newBoards: List[Board] = boards.map(_.feed(number))
        newBoards.find(_.bingo) match {
          case None        => winFirst(rest, newBoards)
          case Some(board) => number -> board
        }
    }

  private def winLast(numbers: List[Int], boards: List[Board]): (Int, Board) = {
    @tailrec
    def loop(winner: Option[(Int, Board)], numbers: List[Int], boards: List[Board]): (Int, Board) =
      numbers match {
        case Nil => winner.getOrElse(throw new IllegalStateException("no more numbers"))
        case number :: rest =>
          val newBoards: List[Board] = boards.map(_.feed(number))
          val bingo = newBoards.filter(_.bingo).toSet
          val noBingo = newBoards.filterNot(bingo)
          bingo.size match {
            case 1 => loop(Some(number -> bingo.head), rest, noBingo)
            case _ => loop(winner, rest, noBingo)
          }
      }

    loop(None, numbers, boards)
  }

  val lines = getStrings("input04.txt")

  val numbers = lines.head.split(",").map(_.toInt).toList

  val boards = readBoards(lines.drop(2))

  val resultWinFirst = winFirst(numbers, boards)
  println(resultWinFirst._1 * resultWinFirst._2.missedNumbers.sum)

  val resultWinLast = winLast(numbers, boards)
  println(resultWinLast._1 * resultWinLast._2.missedNumbers.sum)

}
