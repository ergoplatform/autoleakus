package org.ergoplatform.autoleakus

import com.google.common.primitives.Ints
import org.eroplatform.autoleakus._
import org.scalatest.prop.{PropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class AutoleakusSpecification extends PropSpec with PropertyChecks with TableDrivenPropertyChecks with Matchers {

  val N: Int = 10000
  val k: Int = 4
  val b: BigInt = q / BigInt("1000000000000")

  property("Sum to interval") {
    val wagner = WagnerAlg(k, N)

    forAll { m: Array[Byte] =>
      def elementGen(l: Int, i: Int): BigInt = {
        assert(l < k)
        hash(m ++ Ints.toByteArray(l) ++ Ints.toByteArray(i)).mod(q)
      }

      wagner.solve(elementGen, b).foreach { J =>
        val sum: BigInt = J.zipWithIndex.map(li => elementGen(li._2, li._1)).sum.mod(q)
        require(sum <= b || sum >= q - b, s"Incorrect sum $sum <= $b || $sum >= ${q - b} | ${J.indices} | ${J.indices.zipWithIndex.map(li => elementGen(li._2, li._1))}")
      }
    }
  }

  property("Autoleakus should generate valid solutions") {
    forAll { (sk0: BigInt, m: Array[Byte]) =>
      whenever(sk0 > 0) {
        val sk = sk0.mod(q)
        val alg = new Autoleakus(k, N)
        val sols = alg.prove(m, b, sk)
        sols.foreach { s =>
          alg.verify(s, b) shouldBe 'success
        }
      }
    }
  }


}
