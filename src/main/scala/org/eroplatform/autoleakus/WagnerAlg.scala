package org.eroplatform.autoleakus

import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class WagnerAlg(k: Int, N: Int) extends ScorexLogging {

  val halfP: BigInt = q / 2
  val n: Int = bitLength(q)
  val lgK: Int = lg(k)

  def solve(elementGen: (Int, Int) => BigInt, b: BigInt): Seq[PrivateSolution] = {
    val h = calcH(b)
    val randoms: Map[Int, BigInt] = (0 until lgK).map(k => k -> randomNumber).toMap
    val randByEll: Map[Int, BigInt] = (0 until k).map { l =>
      l -> (0 until lgK).map { i =>
        if (((l >> i) & 1) == 1) {
          randoms(i)
        } else {
          -randoms(i)
        }
      }.sum.mod(q)
    }.toMap

    val lists: Seq[Seq[(BigInt, Seq[Int])]] = (0 until k).map { l =>
      log(s"Generating list $l of $k with $N elements")
      (0 until N).map { i =>
        (elementGen(l, i) + randByEll(l)).mod(q) -> Seq(i)
      }
    }


    def join(list: Seq[(BigInt, Seq[Int], Boolean)], round: Int): Seq[(BigInt, Seq[Int])] = {
      val size = list.size
      val (atMost, atLeast) = calcInterval(round, h, b)
      log(s"Round $round: search sums 0-$atMost || $atLeast-$q from $size elements")
      val resp: ArrayBuffer[(BigInt, Seq[Int])] = ArrayBuffer[(BigInt, Seq[Int])]()

      list.indices.foreach { i =>
        val x = list(i)

        def increment(a: Int): Int = (a + 1) % size

        var j: Int = increment(i)
        var continue: Boolean = true

        do {
          val y = list(j)
          if (x._3 ^ y._3) {
            val xVal = if (x._3) -x._1 else x._1
            val yVal = if (y._3) -y._1 else y._1
            val sum = (xVal + yVal).mod(q)
            if (sum <= atMost || sum >= atLeast) {
              val indices = if (x._3) x._2 ++ y._2 else y._2 ++ x._2
              resp += sum -> indices
            } else {
              continue = false
            }
          }
          j = increment(j)
        } while (continue)
      }

      resp
    }

    @tailrec
    def loop(lists: Seq[Seq[(BigInt, Seq[Int])]]): Seq[(BigInt, Seq[Int])] = if (lists.length == 1) {
      log(s"${lists.head.size} solutions found")
      lists.head
    } else {
      val round = lgK - lg(lists.size) + 1
      val nextLev = (0 until lists.size by 2).map { i =>
        val list = (lists(i).map(l => ((-l._1).mod(q), l._2, true)) ++
          lists(i + 1).map(l => (l._1, l._2, false)))
          .sortBy(_._1)

        join(list, round)
      }
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

  def calcInterval(round: Int, h: Int, b: BigInt): (BigInt, BigInt) = {
    val atMost: BigInt = if (round != lgK) BigInt(2).pow(n - round * (h / (lgK + 1))) else b
    val atLeast: BigInt = q - atMost
    assert(atLeast > atMost, s"Incorrect $atLeast > $atMost")
    (atMost, atLeast)
  }

  def lg(x: Int): Int = (Math.log(x) / Math.log(2)).toInt
    .ensuring(s => Math.pow(2, s) == x)

}
