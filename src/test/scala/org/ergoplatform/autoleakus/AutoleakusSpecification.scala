package org.ergoplatform.autoleakus

import org.ergoplatform.autoleakus.pow.ksum.hashBinding.{HKSumPowTask, HKSumSolution}
import org.ergoplatform.autoleakus.pow.ksum.noBinding.NKSumPowTask
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class AutoleakusSpecification extends PropSpec with PropertyChecks with TableDrivenPropertyChecks with Matchers {
  protected implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val b: BigInt = q / BigInt("1000000000000")

  property("Sum to interval") {
    forAll(Arbitrary.arbitrary[Array[Byte]], kGen) { (m: Array[Byte], k: Int) =>
      val solver = new NKSumPowTask(k, NFromKandB(k, b))
      val sk = hash(m)
      val pk = genPk(sk)
      val x = hash(m ++ Array(1.toByte))
      val w = genPk(x)

      solver.solve(m, sk, b).take(100).foreach { s =>
        solver.nonceIsCorrect(s.n) shouldBe 'success
        (s.d < b || s.d > (q - b)) shouldBe true
      }
    }
  }

  property("Autoleakus with ksum") {
    forAll(Arbitrary.arbitrary[Array[Byte]], kGen) { (m: Array[Byte], k: Int) =>
      val sk = hash(m)
      val alg = new Autoleakus(new NKSumPowTask(k, NFromKandB(k, b)))
      val sols = alg.prove(m, b, sk)
      sols.take(100).foreach { s =>
        alg.verify(s, b) shouldBe 'success
      }
    }
  }

  property("Autoleakus with csum") {
    val b: BigInt = q / BigInt("1000")
    val N = 10000
    forAll(Arbitrary.arbitrary[Array[Byte]], kGen) { (m: Array[Byte], k: Int) =>
      val sk = hash(m)
      val alg = new Autoleakus(new HKSumPowTask(k, N))
      val sols = alg.prove(m, b, sk)
      sols.take(100).foreach { s =>
        alg.verify(s, b) shouldBe 'success
      }
    }
  }

  property("Allow to check specified nonces in HKSumPowTask") {
    val b: BigInt = q / BigInt("1000")
    val N = 10000
    val k = 128
    val pow = new HKSumPowTask(k, N)
    forAll { m: Array[Byte] =>
      val sk = hash(m)
      pow.initialize(m, sk)
      var solution: Option[HKSumSolution] = None
      var i = 0
      while (solution.isEmpty) {
        i += 100
        solution = pow.checkNonces(m, sk, b, i, i + 100)
      }
    }
  }

  def kGen: Gen[Int] = Gen.choose(2, 5).map(p => BigInt(2).pow(p).toInt)

  def NFromKandB(k: Int, b: BigInt): Int = {
    val n = q.bigInteger.bitLength() - b.bigInteger.bitLength()
    BigInt(2).pow(n / (1 + lg(k))).toInt * 40 / 64
  }


  private def lg(x: Int): Int = (Math.log(x) / Math.log(2)).toInt
    .ensuring(s => Math.pow(2, s) == x)

}
