package com.adventofcode.utils

object CollectionUtils {
  implicit class SeqOps[T](seq: Seq[T]) {

    def frequency: Map[T, BigInt] =
      seq.foldLeft(Map.empty[T, BigInt]) { case (res, e) =>
        res + (e -> (res.getOrElse(e, 0: BigInt) + 1))
      }
  }

  implicit class FrequencyOps[T](map: Map[T, BigInt]) {
    def mostFrequent: T =
      mostFrequentPair._1

    def mostFrequentPair: (T, BigInt) =
      map.maxBy(_._2)

    def mostFrequentN(n: Int): Map[T, BigInt] = {
      map.toList
        .sortBy(_._2)(Ordering.BigInt.reverse)
        .take(n)
        .toMap
    }

    def leastFrequentPair: (T, BigInt) =
      map.minBy(_._2)

    def leastFrequent: T =
      leastFrequentPair._1

    def mostFrequentOpt: Option[T] =
      findUniqueByCount(_.maxByOption(_._2))

    def leastFrequentOpt: Option[T] =
      findUniqueByCount(_.minByOption(_._2))

    private def findUniqueByCount(predicate: Map[T, BigInt] => Option[(T, BigInt)]): Option[T] = {
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

  implicit class FrequencySeqOps[T](list: Seq[(T, BigInt)]) {
    def compactFrequency: Map[T, BigInt] =
      list
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).sum)
        .toMap
  }
}
