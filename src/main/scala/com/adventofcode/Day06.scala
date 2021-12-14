package com.adventofcode

import com.adventofcode.utils.CollectionUtils.{FrequencyOps, SeqOps}
import com.adventofcode.utils.FileUtils.getStrings

import scala.annotation.tailrec

object Day06 extends App {

  private type DaysLeft = Int
  private type Count = BigInt

  @tailrec
  private def live(fishes: Map[DaysLeft, Count], daysLeft: DaysLeft): Map[DaysLeft, Count] = {
    if (daysLeft == 0) fishes
    else {
      val newFishes = fishes.toList
        .flatMap { case (daysLeft, count) =>
          daysLeft match {
            case 0 => List(6 -> count, 8 -> count)
            case x => List((x - 1) -> count)
          }
        }
        .foldLeft(Map.empty[DaysLeft, Count]) { case (acc, (daysLeft, count)) =>
          acc + (daysLeft -> (acc.getOrElse(daysLeft, 0: BigInt) + count))
        }

      live(newFishes, daysLeft - 1)
    }
  }

  val fishes = getStrings("input06.txt").flatMap(_.split(",").toList).map(_.toInt)

  val fishFrequency = fishes.frequency
  val result1: Map[DaysLeft, Count] = live(fishFrequency, 80)
  println(result1.values.sum)

  val result2: Map[DaysLeft, Count] = live(fishes.frequency, 256)
  println(result2.values.sum)
}
