package com.adventofcode

import com.adventofcode.utils.CollectionUtils.{FrequencyOps, SeqOps}
import com.adventofcode.utils.FileUtils.getLines

import scala.annotation.tailrec
import scala.language.implicitConversions

object Day03 extends App {
  val diagnostics = getLines("input03_1.txt")(s => s)

  private def inverse(str: Seq[Char]) = str.map {
    case '0' => '1'
    case '1' => '0'
  }

  private implicit def charsToNum(value: Seq[Char]): Int =
    Integer.parseInt(value.mkString, 2)

  val gammaBits = for {
    i <- diagnostics.head.indices
    bits = diagnostics.map(_(i))
  } yield bits.frequency.mostFrequent

  val gamma: Int = gammaBits
  val epsilon: Int = inverse(gammaBits)

  println(gamma * epsilon)

  private def findMostCommonBit(input: List[String], idx: Int): Char =
    input.map(_(idx)).frequency.mostFrequentOpt.getOrElse('1')

  private def findLeastCommonBit(input: List[String], idx: Int): Char =
    input.map(_(idx)).frequency.leastFrequentOpt.getOrElse('0')

  private def filterDiagnostics(f: (List[String], Int) => Char): Int = {
    @tailrec
    def loop(diagnostics: List[String], idx: Int): String =
      diagnostics match {
        case head :: Nil => head
        case xs =>
          val bit = f(xs, idx)
          loop(xs.filter(_(idx) == bit), idx + 1)
      }

    val res = loop(diagnostics, 0)
    Integer.parseInt(res, 2)
  }

  val oxygen = filterDiagnostics(findMostCommonBit)
  val co2 = filterDiagnostics(findLeastCommonBit)

  println(oxygen * co2)
}
