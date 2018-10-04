package org.ergoplatform

import org.bouncycastle.math.ec.ECPoint
import scapi.sigma.BcDlogFp
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType

package object autoleakus {

  type PrivateKey = BigInt
  type SecretsSum = BigInt

  val group: BcDlogFp[EcPointType] = CryptoConstants.dlogGroup
  val q: BigInt = group.q

  val hashFn: NumericHash = new NumericHash(q)

  def hash(in: Array[Byte]): BigInt = hashFn.hash(in)

  def genPk(s: PrivateKey): ECPoint = group.exponentiate(group.generator, s.bigInteger)

  def randomNumber(): PrivateKey = hash(scorex.utils.Random.randomBytes(32))

  def lg(x: Int): Int = (Math.log(x) / Math.log(2)).toInt.ensuring(s => Math.pow(2, s) == x)

  def pkToBytes(pk: ECPoint): Array[Byte] = pk.getEncoded(true)

}