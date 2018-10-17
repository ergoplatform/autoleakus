package org.ergoplatform.autoleakus.pow.ksum.noBinding

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.autoleakus.pow.Solution

case class NKSumSolution(m: Array[Byte], pk: ECPoint, w: ECPoint, n: NKSumNonce, d: BigInt) extends Solution