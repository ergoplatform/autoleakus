package org.ergoplatform.autoleakus

import org.ergoplatform.autoleakus.pow.KSumSolver

/**
  * object to test autoleakus in realistic conditions
  */
object AutoleakusBenchmark extends App {


  val numbersBitLength = 20
  // 2 levels of Wagner alg
  val k: Int = 4
  // 1 solution
  val b: BigInt = BigInt(2).pow(250 - numbersBitLength * (1 + lg(k)))
  // numbers encoded with numbersBitLength bits
  val N: Int = KSumSolver.expeсtedListSize(k, b)

  println(s"N = $N")
  println(s"Minimum memory consumption: ${BigDecimal(N * k * (32 + 4)) / 1024 / 1024 / 1024} Gb")

  val m: Array[Byte] = "test".getBytes()
  val sk = randomNumber()

  val alg = new Autoleakus(k, N)
  val sols = alg.prove(m, b, sk)
  val st = System.currentTimeMillis()
  println(s"start solution verification")
  sols.foreach { s =>
    alg.verify(s, b).get
  }
  println(s"${sols.size} solutions verified in ${System.currentTimeMillis()-st} ms")

}
