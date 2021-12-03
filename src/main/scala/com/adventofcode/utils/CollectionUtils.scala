package com.adventofcode.utils

object CollectionUtils {
  implicit class SeqOps[T](seq: Seq[T]) {

    def frequency: Map[T, Int] =
      seq.foldLeft(Map.empty[T, Int]) { case (res, e) =>
        res + (e -> (res.getOrElse(e, 0) + 1))
      }
  }

  implicit class FrequencyOps[T](map: Map[T, Int]) {
    def mostFrequent: T =
      map.maxBy(_._2)._1

    def leastFrequent: T =
      map.minBy(_._2)._1

    def mostFrequentOpt: Option[T] =
      findUniqueByCount(_.maxByOption(_._2))

    def leastFrequentOpt: Option[T] =
      findUniqueByCount(_.minByOption(_._2))

    private def findUniqueByCount(predicate: Map[T, Int] => Option[(T, Int)]): Option[T] = {
      val candidates = for {
        (_, n) <- predicate(map)
      } yield map.collect {
        case (t1, n1) if n1 == n => t1
      }

      candidates.flatMap { seq =>
        if (seq.size == 1) Some(seq.head)
        else None
      }
    }
  }
}
