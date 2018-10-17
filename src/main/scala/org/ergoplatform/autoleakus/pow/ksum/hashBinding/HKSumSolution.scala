package org.ergoplatform.autoleakus.pow.ksum.hashBinding

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.autoleakus.pow.Solution

case class HKSumSolution(m: Array[Byte], pk: ECPoint, w: ECPoint, n: HKSumNonce, d: BigInt) extends Solution