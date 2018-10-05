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
    val p1 = pkToBytes(pk)
    val p2 = pkToBytes(w)
    log(s"Generate list of $N elements")
    val list: IndexedSeq[BigInt] = (0 until N).map{i =>
      if(i % 1000000 == 0 && i > 0) log(s"$i generated")
      getElement(m, p1, p2, i)
    }

    def fastGetElement(m: Array[Byte], p1: Array[Byte], p2: Array[Byte], i: Int): BigInt = list(i)

    @tailrec
    def loop(i: Int): Option[CSumSolution] = if (i == -1) {
      None
    } else {
      if(i % 1000000 == 0 && i > 0) log(s"$i nonce tested")
      val seed = Ints.toByteArray(i)
      val d = (calcChain(m, seed, p1, p2, 0: Byte, fastGetElement) +
        x * calcChain(m, seed, p1, p2, 1: Byte, fastGetElement) + sk).mod(q)
      if (d <= b) {
        log(s"solution found at $i")
        Some(CSumSolution(m, pk, w, CSumNonce(seed), d))
      } else {
        loop(i + 1)
      }
    }

    log(s"Start solution search")
    loop(0).toSeq
  }

  override def nonceIsCorrect(nonce: Nonce): Try[Unit] = nonce match {
    case n: CSumNonce => Success()
    case _ => Failure(new Error(s"Nonce $nonce is incorrect for N=$N"))
  }

  override def f1(m: Array[Byte], pk: ECPoint, w: ECPoint, nonce: Nonce): BigInt = nonce match {
    case n: CSumNonce => calcChain(m, n.seed, pkToBytes(pk), pkToBytes(w), 1: Byte, getElement)
    case e => throw new Error(s"Incorrect task nonce $e")
  }

  override def f2(m: Array[Byte], pk: ECPoint, w: ECPoint, nonce: Nonce): BigInt = nonce match {
    case n: CSumNonce => calcChain(m, n.seed, pkToBytes(pk), pkToBytes(w), 1: Byte, getElement)
    case e => throw new Error(s"Incorrect task nonce $e")
  }

  private def calcChain(m: Array[Byte],
                        seed: Array[Byte],
                        p1: Array[Byte],
                        p2: Array[Byte],
                        orderByte: Byte,
                        getElement: (Array[Byte], Array[Byte], Array[Byte], Int) => BigInt): BigInt = {
    val indexes = indexesBySeed(seed ++ m, 1)
    indexes.map(i => getElement(m: Array[Byte], p1: Array[Byte], p2: Array[Byte], i: Int)).sum.mod(q)
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

