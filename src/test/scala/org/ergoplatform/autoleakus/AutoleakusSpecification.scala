package org.ergoplatform.autoleakus

import org.ergoplatform.autoleakus.pow.KSumSolver
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class AutoleakusSpecification extends PropSpec with PropertyChecks with TableDrivenPropertyChecks with Matchers {

  val b: BigInt = q / BigInt("1000000000000")

  property("Sum to interval") {
    forAll(Arbitrary.arbitrary[Array[Byte]], kGen) { (m: Array[Byte], k: Int) =>
      val solver = KSumSolver(k, NFromKandB(k, b))
      val sk = hash(m)
      val pk = genPk(sk)
      val x = hash(m ++ Array(1.toByte))
      val w = genPk(x)

      solver.solve(m, x, sk, b).take(100).foreach { n =>
        solver.nonceIsCorrect(n) shouldBe 'success
        val d = solver.f1(m, pk, w, n) + x * solver.f2(m, pk, w, n) - sk
        (d < b || d > (q - b)) shouldBe true
      }
    }
  }

  property("Autoleakus should generate valid solutions") {
    forAll(Arbitrary.arbitrary[Array[Byte]], kGen) { (m: Array[Byte], k: Int) =>
      val sk = hash(m)
      val alg = new Autoleakus(k, NFromKandB(k, b))
      val sols = alg.prove(m, b, sk)
      sols.take(100).foreach { s =>
        alg.verify(s, b) shouldBe 'success
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
