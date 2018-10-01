package org.eroplatform

import org.bouncycastle.math.ec.ECPoint
import scapi.sigma.BcDlogFp
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType

package object autoleakus {

  type PrivateKey = BigInt
  type SecretsSum = BigInt

  val group: BcDlogFp[EcPointType] = CryptoConstants.dlogGroup
  val q = group.q

  def genPk(s: PrivateKey): ECPoint = group.exponentiate(group.generator, s.bigInteger)
  val hash: NumericHash = new NumericHash(q)

}