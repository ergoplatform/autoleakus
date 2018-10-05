package org.ergoplatform.autoleakus.pow.chainsum

import com.google.common.primitives.{Bytes, Ints}
import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.autoleakus._
import org.ergoplatform.autoleakus.pow.{Nonce, PowTask}
import scorex.crypto.hash.Blake2b256
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class CSumPowTask(k: Int, N: Int) extends PowTask with ScorexLogging {

  override def solve(m: Array[Byte], x: BigInt, sk: BigInt, b: BigInt): Seq[CSumSolution] = {
    val pk = genPk(sk)
    val w = genPk(x)

    @tailrec
    def loop(seed: Int): Option[CSumSolution] = if (seed == -1) {
      None
    } else {
      val n = CSumNonce(Ints.toByteArray(seed))
      val d = (f1(m, pk, w, n) + x * f2(m, pk, w, n) + sk).mod(q)
      if (d <= b) {
        log(s"solution found at seed ${seed}")
        Some(CSumSolution(m, pk, w, n, d))
      } else {
        loop(seed + 1)
      }
    }

    loop(0).toSeq
  }

  override def nonceIsCorrect(nonce: Nonce): Try[Unit] = nonce match {
    case n: CSumNonce => Success()
    case _ => Failure(new Error(s"Nonce $nonce is incorrect for N=$N"))
  }

  override def f1(m: Array[Byte], pk: ECPoint, w: ECPoint, nonce: Nonce): BigInt = nonce match {
    case n: CSumNonce =>
      val p1 = pkToBytes(pk)
      val p2 = pkToBytes(w)
      val indexes = indexesBySeed(n.seed ++ m, 0)
      indexes.map(i => getElement(m: Array[Byte], p1: Array[Byte], p2: Array[Byte], i: Int)).sum.mod(q)
    case e => throw new Error(s"Incorrect task nonce $e")
  }

  override def f2(m: Array[Byte], pk: ECPoint, w: ECPoint, nonce: Nonce): BigInt = nonce match {
    case n: CSumNonce =>
      val p1 = pkToBytes(pk)
      val p2 = pkToBytes(w)
      val indexes = indexesBySeed(n.seed ++ m, 1)
      indexes.map(i => getElement(m: Array[Byte], p1: Array[Byte], p2: Array[Byte], i: Int)).sum.mod(q)
    case e => throw new Error(s"Incorrect task nonce $e")
  }

  def indexesBySeed(seed: Array[Byte], orderByte: Byte): Seq[Int] = {
    // todo this function should be changed
    assert(k <= 256 / 4, "Current implementation allows only k <= 32")
    Blake2b256(orderByte +: seed).grouped(4).take(k).toList.map(b => BigInt(b).mod(N).toInt)
  }

  private def log(str: String): Unit = logger.debug(str)

  private def getElement(m: Array[Byte], p1: Array[Byte], p2: Array[Byte], i: Int): BigInt = {
    hash(Bytes.concat(m, p1, p2, Ints.toByteArray(i)))
  }

}

