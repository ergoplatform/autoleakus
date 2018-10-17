package org.ergoplatform.autoleakus.pow.ksum.hashBinding

import java.math.BigInteger

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.autoleakus._
import org.ergoplatform.autoleakus.pow.Nonce
import org.ergoplatform.autoleakus.pow.ksum.KSumPowTask
import scorex.crypto.hash.Blake2b256

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * k-SUM solver via Brute-force.
  * Brute-force is required by algorithm validation, as soon as indexes of elements are
  * generated in a pseudo-random way as a hash(message || nonce).
  */
case class HKSumPowTask(k: Int, N: Int) extends KSumPowTask {

  private val NBigInteger: BigInteger = BigInt(N).bigInteger

  override def solve(m: Array[Byte], x: BigInt, sk: BigInt, b: BigInt): Seq[HKSumSolution] = {
    val pk = genPk(sk)
    val w = genPk(x)
    val p1 = pkToBytes(pk)
    val p2 = pkToBytes(w)
    log(s"Generate list of $N elements")
    val list: IndexedSeq[BigInt] = (0 until N).map { i =>
      if (i % 1000000 == 0 && i > 0) log(s"$i generated")
      val indexBytes = Ints.toByteArray(i)
      genElement(m, p1, p2, indexBytes, 0: Byte) + x * genElement(m, p1, p2, indexBytes, 1: Byte)
    }

    @tailrec
    def loop(i: Long): Option[HKSumSolution] = if (i == -1) {
      None
    } else {
      if (i % 1000000 == 0 && i > 0) log(s"$i nonce tested")
      val nonce = Longs.toByteArray(i)
      val d = (genIndexes(m, nonce).map(i => list(i)).sum - sk).mod(q)
      if (d <= b) {
        log(s"Solution found at $i")
        Some(HKSumSolution(m, pk, w, HKSumNonce(nonce), d))
      } else {
        loop(i + 1)
      }
    }

    log(s"Start search of solution with $k elements from list with $N elements")
    loop(0).toSeq
  }

  override def nonceIsCorrect(nonce: Nonce): Try[Unit] = nonce match {
    case n: HKSumNonce if n.nonceBytes.length == 8 => Success()
    case _ => Failure(new Error(s"Nonce $nonce is incorrect for N=$N"))
  }

  override protected def genIndexesBytes(m: Array[Byte], nonce: Nonce): Seq[Array[Byte]] = nonce match {
    case n: HKSumNonce => genIndexes(m, n.nonceBytes).map(Ints.toByteArray)
    case e => throw new Error(s"Incorrect task nonce $e")
  }

  private def genIndexes(m: Array[Byte], nonceBytes: Array[Byte]): Seq[Int] = {
    val seed = Bytes.concat(m, nonceBytes)
    val hashesRequired = (k.toDouble / 8).ceil.toInt
    val indexes = (0 until hashesRequired) flatMap { i =>
      val hash = Blake2b256(Bytes.concat(seed, Ints.toByteArray(i)))
      hash.grouped(4).map(b => BigIntegers.fromUnsignedByteArray(b).mod(NBigInteger).intValue())
    }
    indexes.take(k)
  }.ensuring(_.length == k)


}

