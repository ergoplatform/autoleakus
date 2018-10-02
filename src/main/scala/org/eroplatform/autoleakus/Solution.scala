package org.eroplatform.autoleakus

import org.bouncycastle.math.ec.ECPoint

case class Solution(m: Array[Byte], pk: ECPoint, randPk: ECPoint, J: Seq[Int], d: BigInt)
