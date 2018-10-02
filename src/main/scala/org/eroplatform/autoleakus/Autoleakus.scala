package org.eroplatform.autoleakus

import com.google.common.primitives.Ints
import org.bouncycastle.math.ec.ECPoint

import scala.util.Try

class Autoleakus(k: Int, N: Int, sk: BigInt) {

  val pk: ECPoint = genPk(sk)
  private val kSumSolver: WagnerAlg = WagnerAlg(k, N)

  def prove(m: Array[Byte], b: BigInt): Seq[PublicSolution] = {
    val randSk = randomNumber
    val randPk = genPk(randSk)

    def elementGen(l: Int, i: Int): BigInt = {
      assert(l <= k, s"incorrect params $l, $i")
      val sk1Shift: BigInt = if (l == 0) -sk else 0
      h(m, pk, randPk, l, i, 0) + randSk * h(m, pk, randPk, l, i, 1) + sk1Shift
    }.mod(q)

    kSumSolver.solve(elementGen, b).map { ps =>
      val J = ps.indices
      val d = J.zipWithIndex.map { il =>
        elementGen(il._2, il._1)
      }.sum.mod(q)

      assert(d <= b || d >= (q - b), s"$d <= $b || $d > ${q - b} for $J")
      PublicSolution(m, pk, randPk, J, d)
    }
  }


  def verify(s: PublicSolution, b: BigInt): Try[Unit] = Try {
    require(s.d < b || s.d > (q - b), s"Incorrect d=${s.d} for b=$b")
    require(s.J.length == k && s.J.forall(_ < N), s"Incorrect indices ${s.J} for N=$N")
    //TODO is 2 lines blow enough?
    require(s.pk.getCurve == group.curve, "pk is not on curve")
    require(s.randPk.getCurve == group.curve, "randPk is not on curve")
    val indexByLevel = s.J.zipWithIndex.map(_.swap).toMap
    val gSum = ((0 until k).map(l => h(s.m, s.pk, s.randPk, l, indexByLevel(l), 0)).sum - s.d).mod(q)
    val pkSum = (0 until k).map(l => h(s.m, s.pk, s.randPk, l, indexByLevel(l), 1)).sum.mod(q)
    val gExp = group.exponentiate(group.generator, gSum.bigInteger)
    val pkExp = s.randPk.multiply(pkSum.bigInteger)
    require(gExp.add(pkExp) == pk, "Incorrect points")
  }

  private def h(m: Array[Byte], p1: ECPoint, p2: ECPoint, l: Int, i: Int, orderByte: Byte) = {
    assert(l <= k)
    hash(m ++ pkToBytes(p1) ++ pkToBytes(p2) ++
      Ints.toByteArray(i) ++ Ints.toByteArray(l) ++ Array(orderByte))
  }

  private def pkToBytes(pk: ECPoint): Array[Byte] = pk.getEncoded(true)


}
