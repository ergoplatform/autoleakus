package org.eroplatform.autoleakus

import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class WagnerAlg(k: Int, N: Int, q: BigInt) extends ScorexLogging {

  val halfP: BigInt = q / 2
  val n: Int = bitLength(q)
  val lgK: Int = lg(k)

  def prove(elementGen: (Int, Int) => BigInt, b: BigInt): Seq[PrivateSolution] = {
    val h = calcH(b)
    val randoms: Map[Int, BigInt] = (0 until lgK).map(k => k -> hash.hash(scorex.utils.Random.randomBytes(32))).toMap
    val randByEll: Map[Int, BigInt] = (0 until k).map { l =>
      l -> (0 until lgK).map { i =>
        if (((i >> l) & 1) == 0) {
          randoms(i)
        } else {
          -randoms(i)
        }
      }.sum.mod(q)
    }.toMap

    val lists: Seq[Seq[(BigInt, Seq[Int])]] = (0 until k).map(l => (0 until N).map { i =>
      (elementGen(l, i) + randByEll(l)).mod(q) -> Seq(i)
    }.sortBy(_._1))

    def join(list1: Seq[(BigInt, Seq[Int])], list2: Seq[(BigInt, Seq[Int])], round: Int): Seq[(BigInt, Seq[Int])] = {
      val (atMost, atLeast) = calcInterval(round, h, b)
      log(s"Round $round: search sums 0-$atMost || $atLeast-$q from ${list1.size}|${list2.size} elements")
      val resp: ArrayBuffer[(BigInt, Seq[Int])] = ArrayBuffer[(BigInt, Seq[Int])]()
      // todo make efficient
      list1.foreach{ x =>
        list2.foreach{ y =>
          val sum = (x._1 + y._1).mod(q)
          if (sum >= atLeast || sum <= atMost) {
            resp += sum -> (x._2 ++ y._2)
          }
        }
      }
      resp
    }

    @tailrec
    def loop(lists: Seq[Seq[(BigInt, Seq[Int])]]): Seq[(BigInt, Seq[Int])] = if (lists.length == 1) {
      lists.head
    } else {
      val round = lgK - lg(lists.size)
      val nextLev = lists.grouped(2).map(l => join(l.head, l.last, round)).toSeq
      loop(nextLev)
    }

    loop(lists).map(l => PrivateSolution(l._2))
  }

  private def log(str: String): Unit = logger.debug(str)

  private def bitLength(bi: BigInt): Int = {
    assert(bi >= 0, "only positive numbers are allowed")
    bi.bigInteger.bitLength()
  }

  def calcH(finalH: BigInt): Int = bitLength(q) - bitLength(finalH)

  def calcInterval(round: Int, h: Int, finalH: BigInt): (BigInt, BigInt) = {
    val atMost: BigInt = if (round != k) BigInt(2).pow(n - round * (h / (k + 1))) else finalH
    val atLeast: BigInt = if (round != k) q - atMost else q
    (atMost, atLeast)
  }

  def lg(x: Int):Int = (Math.log(x) / Math.log(2)).toInt
    .ensuring(s => Math.pow(2,s) == x)
}
