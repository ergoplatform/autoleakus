package org.ergoplatform.autoleakus.pow

import org.bouncycastle.math.ec.ECPoint

case class KSumSolution(m: Array[Byte], pk: ECPoint, w: ECPoint, n: KSumNonce, d: BigInt) {
  val J: Seq[Int] = n.J
}
