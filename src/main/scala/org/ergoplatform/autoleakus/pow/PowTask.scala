package org.ergoplatform.autoleakus.pow

import org.bouncycastle.math.ec.ECPoint
import scorex.util.ScorexLogging

import scala.util.Try

/**
  * Abstract interface for PoW puzzle, that can be used in `Autoleakus`
  */
trait PowTask extends ScorexLogging {

  /**
    * Solve the puzzle
    */
  def solve(m: Array[Byte], x: BigInt, sk: BigInt, b: BigInt): Seq[Solution]

  /**
    * Check, `nonce` is the correct nonce for current puzzle
    */
  def nonceIsCorrect(nonce: Nonce): Try[Unit]

  /**
    * Calculate `f1` function for `Autoleakus`
    */
  def f1(m: Array[Byte], pk: ECPoint, w: ECPoint, n: Nonce): BigInt

  /**
    * Calculate `f2` function for `Autoleakus`
    */
  def f2(m: Array[Byte], pk: ECPoint, w: ECPoint, n: Nonce): BigInt

  protected def log(str: String): Unit = logger.debug(str)

}
