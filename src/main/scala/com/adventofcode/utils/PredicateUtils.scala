package com.adventofcode.utils

object PredicateUtils {
  implicit class Predicate[A](val p1: A => Boolean) extends AnyVal {
    def or[B >: A](p2: B => Boolean): A => Boolean = (a: A) => p1(a) || p2(a)
  }
}
