package org.ergoplatform.autoleakus.pow

import org.bouncycastle.math.ec.ECPoint

/**
  * Abstract interface fot PoWTask solution.
  */
trait Solution {
  /**
    * PoW task message
    */
  val m: Array[Byte]
  /**
    * Curve point, corresponding to secret `sk`
    */
  val pk: ECPoint
  /**
    * Curve point, corresponding to secret `x`
    */
  val w: ECPoint
  /**
    * Task difficulty
    */
  val d: BigInt
  /**
    * Nonce, that solves the puzzle
    */
  val n: Nonce
}
