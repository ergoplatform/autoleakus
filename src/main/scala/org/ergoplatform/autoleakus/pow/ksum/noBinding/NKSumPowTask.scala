package org.ergoplatform.autoleakus.pow.ksum.noBinding

import com.google.common.primitives.Ints
import org.ergoplatform.autoleakus._
import org.ergoplatform.autoleakus.pow.Nonce
import org.ergoplatform.autoleakus.pow.ksum.KSumPowTask

import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.immutable.ParSeq
import scala.util.{Failure, Success, Try}

/**
  * k-SUM solver via Wagners algorithm.
  * Validation does not contain any algorithm binding, so puzzle solving
  * may be done by any algorithm.
  */
class NKSumPowTask(k: Int, N: Int) extends KSumPowTask {

  assert((k & (k - 1)) == 0, s"k should be a power of 2, $k given")

  private val n: Int = bitLength(q)
  private val lgK: Int = lg(k)

  override def solve(m: Array[Byte], sk: BigInt, b: BigInt): Seq[NKSumSolution] = {
    val x: BigInt = randomSecret()
    val pk = genPk(sk)
    val pkBytes = pkToBytes(pk)
    val w = genPk(x)
    val wBytes = pkToBytes(w)

    val h = bitLength(q) - bitLength(b)
    val randoms: Map[Int, BigInt] = (0 until lgK).map(k => k -> randomSecret()).toMap
    val randByEll: Map[Int, BigInt] = (0 until k).map { l =>
      l -> (0 until lgK).map { i =>
        if (((l >> i) & 1) == 1) {
          randoms(i)
        } else {
          -randoms(i)
        }
      }.sum.mod(q)
    }.toMap

    def initialLists: ParSeq[Seq[(BigInt, Seq[Int])]] = (0 until k).par.map { l =>
      log(s"Generating list ${l + 1} of $k with $N elements")
      (0 until N).map { i =>
        val element = {
          val sk1Shift: BigInt = if (l == 0) -sk else 0
          genElement(m, pkBytes, wBytes, indexBytes(i, l), 0) +
            x * genElement(m, pkBytes, wBytes, indexBytes(i, l), 1) + sk1Shift
        }
        (element + randByEll(l)).mod(q) -> Seq(i)
      }
    }

    def join(list: Seq[(BigInt, Seq[Int], Boolean)], round: Int): Seq[(BigInt, Seq[Int])] = {
      val size = list.size
      val atMost: BigInt = if (round != lgK) BigInt(2).pow(n - round * (h / (lgK + 1))) else b
      val atLeast: BigInt = q - atMost
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
          if (i == j) continue = false
        } while (continue)
      }

      resp
    }

    @tailrec
    def loop(lists: GenSeq[Seq[(BigInt, Seq[Int])]]): Seq[(BigInt, Seq[Int])] = if (lists.length == 1) {
      log(s"${lists.head.size} solutions found")
      lists.head
    } else {
      val round = lgK - lg(lists.size) + 1
      val nextLev = (0 until lists.size by 2).map { i =>
        log(s"prepare lists $i,${i + 1} for round $round")
        val list = (lists(i).map(l => ((-l._1).mod(q), l._2, true)) ++
          lists(i + 1).map(l => (l._1, l._2, false)))
          .sortBy(_._1)

        join(list, round)
      }
      loop(nextLev)
    }

    loop(initialLists).map { case (d, nonce) =>
      NKSumSolution(m, pk, w, NKSumNonce(nonce), d)
    }
  }

  override def nonceIsCorrect(nonce: Nonce): Try[Unit] = nonce match {
    case n: NKSumNonce if n.J.length == k && n.J.forall(_ < N) => Success()
    case _ => Failure(new Error(s"Nonce $nonce is incorrect for N=$N"))
  }

  protected def genIndexesBytes(m: Array[Byte], nonce: Nonce): Seq[Array[Byte]] = nonce match {
    case n: NKSumNonce =>
      val indexByLevel = n.J.zipWithIndex.map(_.swap).toMap
      (0 until k).map(l => indexBytes(indexByLevel(l), l))
    case msg => throw new Error(s"Incorrect task nonce $msg")
  }

  private def bitLength(bi: BigInt): Int = {
    assert(bi >= 0, "only positive numbers are allowed")
    bi.bigInteger.bitLength()
  }

  private def indexBytes(i: Int, l: Int): Array[Byte] = Ints.toByteArray(i) ++ Ints.toByteArray(l)

}

object NKSumPowTask {

  def expeсtedListSize(k: Int, b: BigInt): Int = {
    val n = q.bigInteger.bitLength() - b.bigInteger.bitLength()
    BigInt(2).pow(n / (1 + lg(k))).toInt
  }

}