package org.ergoplatform.autoleakus

import org.ergoplatform.autoleakus.pow.chainsum.CSumPowTask
import org.ergoplatform.autoleakus.pow.ksum.KSumPowTask

/**
  * object to test autoleakus in realistic conditions
  */
object AutoleakusBenchmark extends App {

  withCSUM()

  def withCSUM(): Unit = {
    // 1M attempts:
    // pre-generated list: 29.5 ms list generation, 136 s solution search
    // recalulate elements on fly: 410 s

    val N: Int = 10000000
    val k: Int = 32
    val difficulty = BigInt("10000000")
    val b: BigInt = q / difficulty

    println(s"N = $N")
    println(s"Minimum memory consumption: ${BigDecimal(N * (32 + 4)) / 1024 / 1024 / 1024} Gb")
    println(s"Average element hits: ${BigDecimal(difficulty * k) / N}")
    println(s"At least one hit after difficulty: ${N / k}")

    val m: Array[Byte] = scorex.util.Random.randomBytes()
    val sk = randomSecret()

    val alg = new Autoleakus(CSumPowTask(k, N))
    val st = System.currentTimeMillis()
    val sols = alg.prove(m, b, sk)
    val st2 = System.currentTimeMillis()
    sols.foreach { s =>
      alg.verify(s, b).get
    }
    println(s"${sols.size} solutions found in ${st2 - st}, verified in ${System.currentTimeMillis() - st2} ms")
  }

  def withKSUM(): Unit = {
    val numbersBitLength = 20
    // 2 levels of Wagner alg
    val k: Int = 4
    // 1 solution
    val b: BigInt = BigInt(2).pow(250 - numbersBitLength * (1 + lg(k)))
    // numbers encoded with numbersBitLength bits
    val N: Int = KSumPowTask.expeсtedListSize(k, b)

    println(s"N = $N")
    println(s"Minimum memory consumption: ${BigDecimal(N * k * (32 + 4)) / 1024 / 1024 / 1024} Gb")

    val m: Array[Byte] = scorex.util.Random.randomBytes()
    val sk = randomSecret()

    val alg = new Autoleakus(KSumPowTask(k, N))
    val sols = alg.prove(m, b, sk)
    val st = System.currentTimeMillis()
    println(s"start solution verification")
    sols.foreach { s =>
      alg.verify(s, b).get
    }
    println(s"${sols.size} solutions verified in ${System.currentTimeMillis() - st} ms")
  }

}
