package org.ergoplatform.autoleakus.pow.chainsum

import org.ergoplatform.autoleakus.pow.Nonce

case class CSumNonce(nonceBytes: Array[Byte]) extends Nonce
