package org.ergoplatform.autoleakus.pow.chainsum

import java.math.BigInteger

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.autoleakus._
import org.ergoplatform.autoleakus.pow.{Nonce, PowTask}
import scorex.crypto.hash.Blake2b256
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Task to find such a nonce, that generates 2 `k`-length sequences (s1,s2)
  * of elements from the list of size `N`, such that `s1.sum + sk * s2.sum < b`
  */
case class CSumPowTask(k: Int, N: Int) extends PowTask with ScorexLogging {

  assert(k > 0, s"Incorrect k=$k")
  private val NBigInteger: BigInteger = BigInt(N).bigInteger

  override def solve(m: Array[Byte], x: BigInt, sk: BigInt, b: BigInt): Seq[CSumSolution] = {
    val pk = genPk(sk)
    val w = genPk(x)
    val p1 = pkToBytes(pk)
    val p2 = pkToBytes(w)
    log(s"Generate list of $N elements")
    val list: IndexedSeq[BigInt] = (0 until N).map { i =>
      if (i % 1000000 == 0 && i > 0) log(s"$i generated")
      genElement(m, p1, p2, i)
    }

    def fastGetElement(m: Array[Byte], p1: Array[Byte], p2: Array[Byte], i: Int): BigInt = list(i)

    @tailrec
    def loop(i: Long): Option[CSumSolution] = if (i == -1) {
      None
    } else {
      if (i % 1000000 == 0 && i > 0) log(s"$i nonce tested")
      val nonce = Longs.toByteArray(i)
      val d = (calcChain(m, nonce, p1, p2, 0: Byte, fastGetElement) +
        x * calcChain(m, nonce, p1, p2, 1: Byte, fastGetElement) + sk).mod(q)
      if (d <= b) {
        log(s"solution found at $i")
        Some(CSumSolution(m, pk, w, CSumNonce(nonce), d))
      } else {
        loop(i + 1)
      }
    }

    log(s"Start search of solution with $k elements from list with $N elements")
    loop(0).toSeq
  }

  override def nonceIsCorrect(nonce: Nonce): Try[Unit] = nonce match {
    case n: CSumNonce if n.nonceBytes.length == 8 => Success()
    case _ => Failure(new Error(s"Nonce $nonce is incorrect for N=$N"))
  }

  override def f1(m: Array[Byte], pk: ECPoint, w: ECPoint, nonce: Nonce): BigInt = nonce match {
    case n: CSumNonce => calcChain(m, n.nonceBytes, pkToBytes(pk), pkToBytes(w), 1: Byte, genElement)
    case e => throw new Error(s"Incorrect task nonce $e")
  }

  override def f2(m: Array[Byte], pk: ECPoint, w: ECPoint, nonce: Nonce): BigInt = nonce match {
    case n: CSumNonce => calcChain(m, n.nonceBytes, pkToBytes(pk), pkToBytes(w), 1: Byte, genElement)
    case e => throw new Error(s"Incorrect task nonce $e")
  }

  private def calcChain(m: Array[Byte],
                        nonceBytes: Array[Byte],
                        p1: Array[Byte],
                        p2: Array[Byte],
                        orderByte: Byte,
                        getElement: (Array[Byte], Array[Byte], Array[Byte], Int) => BigInt): BigInt = {
    val indexes = genIndexes(m, nonceBytes, 1)
    indexes.map(i => getElement(m: Array[Byte], p1: Array[Byte], p2: Array[Byte], i: Int)).sum.mod(q)
  }

  private def genIndexes(m: Array[Byte], nonceBytes: Array[Byte], orderByte: Byte): Seq[Int] = {
    @tailrec
    def loop(seed: Array[Byte], acc: Seq[Int]): Seq[Int] = {
      val hash = Blake2b256(seed)
      val nextBatch = hash.grouped(4).map(b => BigIntegers.fromUnsignedByteArray(b).mod(NBigInteger).intValue())
      val newAcc = (acc ++ nextBatch).distinct
      if (newAcc.length >= k) {
        newAcc.take(k)
      } else {
        loop(hash, newAcc)
      }
    }

    loop(Bytes.concat(Array(orderByte), m, nonceBytes), Seq())
  }.ensuring(_.length == k)

  private def log(str: String): Unit = logger.debug(str)

  private def genElement(m: Array[Byte], p1: Array[Byte], p2: Array[Byte], i: Int): BigInt = {
    hash(Bytes.concat(m, p1, p2, Ints.toByteArray(i)))
  }

}

