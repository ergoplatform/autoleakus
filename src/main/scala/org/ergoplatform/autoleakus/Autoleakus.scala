package org.ergoplatform.autoleakus

import org.ergoplatform.autoleakus.pow.{KSumSolution, KSumSolver}

import scala.util.Try

/**
  * Non-outsourceable task of the form `f1(m, pk, w, n) + w * f2(m, pk, w, n) + sk < b`
  */
class Autoleakus(k: Int, N: Int) {

  private val solver: KSumSolver = KSumSolver(k, N)

  def prove(m: Array[Byte], b: BigInt, sk: BigInt): Seq[KSumSolution] = {
    val pk = genPk(sk)
    val x = randomNumber()
    val w = genPk(x)
    solver.solve(m, x, sk, b).map { n =>
      val d = solver.f1(m, pk, w, n) + x * solver.f2(m, pk, w, n) - sk
      KSumSolution(m, pk, w, n, d)
    }
  }

  def verify(s: KSumSolution, b: BigInt): Try[Unit] = solver.nonceIsCorrect(s.n) orElse Try {
    require(s.d < b || s.d > (q - b), s"Incorrect d=${s.d} for b=$b")
    require(s.pk.getCurve == group.curve && !s.pk.isInfinity, "pk is incorrect")
    require(s.w.getCurve == group.curve && !s.w.isInfinity, "w is incorrect")
    val gExp = group.exponentiate(group.generator, (solver.f1(s.m, s.pk, s.w, s.n) - s.d).mod(q).bigInteger)
    val pkExp = s.w.multiply(solver.f2(s.m, s.pk, s.w, s.n).bigInteger)

    require(gExp.add(pkExp) == s.pk, "Incorrect points")
  }

}
