package org.ergoplatform.autoleakus.pow.chainsum

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.autoleakus.pow.Solution

case class CSumSolution(m: Array[Byte], pk: ECPoint, w: ECPoint, n: CSumNonce, d: BigInt) extends Solution