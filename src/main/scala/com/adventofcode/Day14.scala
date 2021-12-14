package com.adventofcode

import com.adventofcode.utils.CollectionUtils.{FrequencyOps, SeqOps, FrequencySeqOps}
import com.adventofcode.utils.FileUtils.getStrings

import scala.annotation.tailrec

object Day14 extends App {
  val lines = getStrings("input14.txt")

  val pattern = lines.head

  val ruleRegex = "(\\w+) -> (\\w+)".r
  val rules = lines
    .drop(2)
    .map { case ruleRegex(pair, el) =>
      pair -> el
    }
    .toMap

  private def step(input: String, n: Int) = {
    @tailrec
    def loop(pairs: Map[String, BigInt], n: Int): Map[String, BigInt] =
      n match {
        case 0 => pairs
        case _ =>
          val newPairs = pairs.toList
            .flatMap { case (pair, count) =>
              val el = rules(pair)
              List((pair.head + el) -> count, (el + pair.last) -> count)
            }
          loop(newPairs.compactFrequency, n - 1)
      }

    val pairs = loop(input.sliding(2).toList.frequency, n).toList

    val frequency = ((pattern.last -> (1: BigInt)) ::
      pairs.map { case (pair, count) =>
        pair.head -> count
      }).compactFrequency

    val mostFrequent = frequency.mostFrequentPair
    val leastFrequent = frequency.leastFrequentPair
    mostFrequent._2 - leastFrequent._2
  }

  println(step(pattern, 10))
  println(step(pattern, 40))
}
