package org.ergoplatform.autoleakus.pow.ksum.hashBinding

import org.ergoplatform.autoleakus.pow.Nonce

case class HKSumNonce(nonceBytes: Array[Byte]) extends Nonce
