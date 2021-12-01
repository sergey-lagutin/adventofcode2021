package com.adventofcode

import com.adventofcode.utils.FileUtils.getLines

object Day01 extends App {
  val measurements = getLines("input01_1.txt")(_.toInt)

  val increases = measurements
    .sliding(2, 1)
    .count { case List(i, j) =>
      i < j
    }
  println(increases)

  val increases3D = measurements
    .sliding(4, 1)
    .count { case List(a, _, _, d) =>
      a < d
    }
  println(increases3D)
}
