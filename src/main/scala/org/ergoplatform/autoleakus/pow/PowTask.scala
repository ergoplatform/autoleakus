package org.ergoplatform.autoleakus.pow

import org.bouncycastle.math.ec.ECPoint

import scala.util.Try

trait PowTask {

  def solve(m: Array[Byte], x: BigInt, sk: BigInt, b: BigInt): Seq[Solution]

  def nonceIsCorrect(nonce: Nonce): Try[Unit]

  def f1(m: Array[Byte], pk: ECPoint, w: ECPoint, n: Nonce): BigInt

  def f2(m: Array[Byte], pk: ECPoint, w: ECPoint, n: Nonce): BigInt

}
