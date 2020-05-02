// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

class ApBuilder2[+A, +B](pa: Parser[A], pb: => Parser[B]) {

  def ~[C](pc: => Parser[C]): ApBuilder3[A, B, C] =
    new ApBuilder3(pa, pb, pc)

  def tupled: Parser[(A, B)] =
    mapN(Tuple2.apply)

  def mapN[C](f: (A, B) => C): Parser[C] =
    new Parser[C] {

           val paʹ = pa
      lazy val pbʹ = pb

      override def void: Parser[Unit] =
        paʹ.void ~>
        pbʹ.void

      def mutParse(mutState: MutState): C = {
        val a = paʹ.mutParse(mutState); if (mutState.isErrored) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isErrored) return dummy
        f(a, b)
      }

    }

}

class ApBuilder3[+A, +B, +C](pa: Parser[A], pb: => Parser[B], pc: => Parser[C]) {

  def ~[D](pd: => Parser[D]): ApBuilder4[A, B, C, D] =
    new ApBuilder4(pa, pb, pc, pd)

  def tupled: Parser[(A, B, C)] =
    mapN(Tuple3.apply)

  def mapN[D](f: (A, B, C) => D): Parser[D] =
    new Parser[D] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc

      override def void: Parser[Unit] =
        paʹ.void ~>
        pbʹ.void ~>
        pcʹ.void

      def mutParse(mutState: MutState): D = {
        val a = paʹ.mutParse(mutState); if (mutState.isErrored) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isErrored) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isErrored) return dummy
        f(a, b, c)
      }

    }
}

class ApBuilder4[+A, +B, +C, +D](pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D]) {

  def tupled: Parser[(A, B, C, D)] =
    mapN(Tuple4.apply)

  def mapN[E](f: (A, B, C, D) => E): Parser[E] =
    new Parser[E] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd

      override def void: Parser[Unit] =
        paʹ.void ~>
        pbʹ.void ~>
        pcʹ.void ~>
        pdʹ.void

      def mutParse(mutState: MutState): E = {
        val a = paʹ.mutParse(mutState); if (mutState.isErrored) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isErrored) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isErrored) return dummy
        val d = pdʹ.mutParse(mutState); if (mutState.isErrored) return dummy
        f(a, b, c, d)
      }

    }

}

