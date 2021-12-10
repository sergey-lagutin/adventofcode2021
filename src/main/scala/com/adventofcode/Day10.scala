package com.adventofcode

import com.adventofcode.utils.FileUtils.getStrings

import scala.annotation.tailrec

object Day10 extends App {
  val lines = getStrings("input10.txt")

  val openChars = "([{<".toSet

  val pairs = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  private def getScore(str: String): (List[Char], Option[Char]) = {
    @tailrec
    def loop(acc: List[Char], str: String): (List[Char], Option[Char]) = {
      if (str.isEmpty) acc -> None
      else {
        val head = str.head
        if (openChars(head))
          loop(head :: acc, str.tail)
        else if (pairs(acc.head) == head)
          loop(acc.tail, str.tail)
        else acc -> Option(head)
      }
    }

    loop(Nil, str)
  }

  val scores = lines.map(getScore)

  val corruptedScores = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val result1 = scores.flatMap(_._2).map(corruptedScores).sum
  println(result1)

  val completeScores: Map[Char, BigInt] = Map(
    '(' -> 1,
    '[' -> 2,
    '{' -> 3,
    '<' -> 4
  )

  private def getAutocompleteScore(list: List[Char]): BigInt =
    list.foldLeft(0: BigInt) { case (acc, e) =>
      acc * 5 + completeScores(e)
    }

  val autocompleteScores = scores
    .collect {
      case (list, corrupted) if corrupted.isEmpty => list
    }
    .map(getAutocompleteScore)
    .sorted
  val result2 = autocompleteScores(autocompleteScores.size / 2)
  println(result2)
}
