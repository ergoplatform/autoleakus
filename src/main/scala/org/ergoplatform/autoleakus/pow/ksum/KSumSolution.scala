package org.ergoplatform.autoleakus.pow.ksum

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.autoleakus.pow.Solution

case class KSumSolution(m: Array[Byte], pk: ECPoint, w: ECPoint, n: KSumNonce, d: BigInt) extends Solution