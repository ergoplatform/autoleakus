package org.ergoplatform.autoleakus.pow

import org.bouncycastle.math.ec.ECPoint

trait Solution {
  val m: Array[Byte]
  val pk: ECPoint
  val w: ECPoint
  val n: Nonce
  val d: BigInt
}
