package com.adventofcode

import com.adventofcode.utils.FileUtils.getLines
import com.adventofcode.utils.PredicateUtils.Predicate

import scala.collection.mutable
object Day08 extends App {
  case class Row(patterns: Set[Set[Char]], digits: List[Set[Char]])

  val rows = getLines("input08.txt") { line =>
    val Array(patternStr, digitStr) = line.split("\\|")
    Row(
      patterns = patternStr.trim.split(" ").map(_.toSet).toSet,
      digits = digitStr.trim.split(" ").map(_.toSet).toList
    )
  }

  private def isOne(digit: Set[Char]) = digit.size == 2
  private def isFour(digit: Set[Char]) = digit.size == 4
  private def isSeven(digit: Set[Char]) = digit.size == 3
  private def isEight(digit: Set[Char]) = digit.size == 7

  val predicate1 = isOne _ or isFour or isSeven or isEight

  val result1 = rows
    .map(_.digits.count(s => predicate1(s)))
    .sum
  println(result1)

  private def decode(row: Row): Int = {
    def findDigit(p: Set[Char] => Boolean): Set[Char] =
      row.patterns.find(p).get

    val digits = mutable.Map.empty[Int, Set[Char]]

    digits += 1 -> findDigit(isOne)
    digits += 4 -> findDigit(isFour)
    digits += 7 -> findDigit(isSeven)
    digits += 8 -> findDigit(isEight)

    digits += 9 ->
      findDigit(s =>
        s.size == 6 &&
          digits(4).intersect(s) == digits(4) &&
          digits(7).intersect(s) == digits(7)
      )

    digits += 0 ->
      findDigit(s =>
        s.size == 6 &&
          digits(7).intersect(s) == digits(7) &&
          digits(9) != s
      )

    digits += 6 ->
      findDigit(s =>
        s.size == 6 &&
          digits(0) != s &&
          digits(9) != s
      )

    digits += 3 ->
      findDigit(s =>
        s.size == 5 &&
          digits(7).intersect(s) == digits(7)
      )

    digits += 5 ->
      findDigit(s =>
        s.size == 5 &&
          digits(3) != s &&
          digits(4).intersect(s).size == 3
      )

    digits += 2 ->
      findDigit(s =>
        s.size == 5 &&
          digits(3) != s &&
          digits(5) != s
      )

    val patternMap: Map[Set[Char], Int] =
      digits.map { case (n, pattern) =>
        pattern -> n
      }.toMap

    val List(a, b, c, d) = row.digits
    patternMap(a) * 1000 +
      patternMap(b) * 100 +
      patternMap(c) * 10 +
      patternMap(d)
  }

  println(rows.map(decode).sum)
}
