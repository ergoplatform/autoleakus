package org.ergoplatform.autoleakus

import com.google.common.primitives.Ints
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class AutoleakusSpecification extends PropSpec with PropertyChecks with TableDrivenPropertyChecks with Matchers {

  val b: BigInt = q / BigInt("1000000000000")

  property("Sum to interval") {
    forAll(Arbitrary.arbitrary[Array[Byte]], kGen) { (m: Array[Byte], k: Int) =>
      val wagner = WagnerAlg(k, NFromKandB(k, b))

      def elementGen(l: Int, i: Int): BigInt = {
        assert(l < k)
        hash(m ++ Ints.toByteArray(l) ++ Ints.toByteArray(i)).mod(q)
      }

      wagner.solve(elementGen, b).take(100).foreach { J =>
        val sum: BigInt = J.zipWithIndex.map(li => elementGen(li._2, li._1)).sum.mod(q)
        require(sum <= b || sum >= q - b, s"Incorrect sum $sum <= $b || $sum >= ${q - b} | ${J.indices} | ${J.indices.zipWithIndex.map(li => elementGen(li._2, li._1))}")
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
