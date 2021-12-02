package com.adventofcode

import com.adventofcode.utils.FileUtils.getLines

object Day02 extends App {

  case class Command(dir: String, value: Int)

  val commands = getLines("input02_1.txt") { line =>
    val Array(dir, value) = line.split(" ")
    Command(dir, value.toInt)
  }

  case class State1(distance: Int, depth: Int) {
    def feed(command: Command): State1 = command.dir match {
      case "forward" => copy(distance = distance + command.value)
      case "up"      => copy(depth = depth - command.value)
      case "down"    => copy(depth = depth + command.value)
    }

    lazy val product: Int = distance * depth
  }

  val state1 = commands.foldLeft(State1(0, 0))(_ feed _)
  println(state1.product)

  case class State2(distance: Int, aim: Int, depth: Int) {
    def feed(command: Command): State2 = command.dir match {
      case "forward" => copy(distance = distance + command.value, depth = depth + aim * command.value)
      case "up"      => copy(aim = aim - command.value)
      case "down"    => copy(aim = aim + command.value)
    }

    lazy val product: Int = distance * depth
  }

  val state2 = commands.foldLeft(State2(0, 0, 0))(_ feed _)
  println(state2.product)
}
