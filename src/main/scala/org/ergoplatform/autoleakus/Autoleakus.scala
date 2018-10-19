package org.ergoplatform.autoleakus

import org.ergoplatform.autoleakus.pow.{PowTask, Solution}

import scala.util.Try

/**
  * Non-outsourceable task of the form `f1(m, pk, w, n) + x * f2(m, pk, w, n) - sk < b`
  */
class Autoleakus(powTask: PowTask) {

  def prove(m: Array[Byte], b: BigInt, sk: BigInt): Seq[Solution] = powTask.solve(m, sk, b)

  def verify(s: Solution, b: BigInt): Try[Unit] = Try {
    powTask.nonceIsCorrect(s.n).get
    require(s.d < b || s.d > (q - b), s"Incorrect d=${s.d} for b=$b")
    require(s.pk.getCurve == group.curve && !s.pk.isInfinity, "pk is incorrect")
    require(s.w.getCurve == group.curve && !s.w.isInfinity, "w is incorrect")
    val gExp = group.exponentiate(group.generator, (powTask.f1(s.m, s.pk, s.w, s.n) - s.d).mod(q).bigInteger)
    val pkExp = s.w.multiply(powTask.f2(s.m, s.pk, s.w, s.n).bigInteger)

    require(gExp.add(pkExp) == s.pk, "Incorrect points")
  }

}
