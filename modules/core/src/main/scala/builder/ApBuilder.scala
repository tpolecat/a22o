// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

class ApBuilder2[+A, +B](pa: Parser[A], pb: => Parser[B]) {

  def ~[C](pc: => Parser[C]): ApBuilder3[A, B, C] =
    new ApBuilder3(pa, pb, pc)

  def void: Parser[Unit] =
    new ApBuilder2(pa.void, pb.void).mapN((_, _) => ())

  def inputText: Parser[String] =
    void.inputText

  def tupled: Parser[(A, B)] =
    mapN(Tuple2.apply)

  def mapN[C](f: (A, B) => C): Parser[C] =
    new Parser[C] {

           val paʹ = pa
      lazy val pbʹ = pb

      override lazy val void: Parser[Unit] =
        paʹ.void ~>
        pbʹ.void

      def mutParse(mutState: MutState): C = {
        val a = paʹ.mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        f(a, b)
      }

    }

}

class ApBuilder3[+A, +B, +C](pa: Parser[A], pb: => Parser[B], pc: => Parser[C]) {

  def void: Parser[Unit] =
    new ApBuilder3(pa.void, pb.void, pc.void).mapN((_, _, _) => ())

  def ~[D](pd: => Parser[D]): ApBuilder4[A, B, C, D] =
    new ApBuilder4(pa, pb, pc, pd)

  def tupled: Parser[(A, B, C)] =
    mapN(Tuple3.apply)

  def mapN[D](f: (A, B, C) => D): Parser[D] =
    new Parser[D] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc

      override lazy val void: Parser[Unit] =
        paʹ.void ~>
        pbʹ.void ~>
        pcʹ.void

      def mutParse(mutState: MutState): D = {
        val a = paʹ.mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        f(a, b, c)
      }

    }
}

class ApBuilder4[+A, +B, +C, +D](pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D]) {

  def void: Parser[Unit] =
    new ApBuilder4(pa.void, pb.void, pc.void, pd.void).mapN((_, _, _, _) => ())

  def ~[E](pe: => Parser[E]): ApBuilder5[A, B, C, D, E] =
    new ApBuilder5(pa, pb, pc, pd, pe)

  def inputText: Parser[String] =
    void.inputText

  def tupled: Parser[(A, B, C, D)] =
    mapN(Tuple4.apply)

  def mapN[E](f: (A, B, C, D) => E): Parser[E] =
    new Parser[E] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd

      override lazy val void: Parser[Unit] =
        paʹ.void ~>
        pbʹ.void ~>
        pcʹ.void ~>
        pdʹ.void

      def mutParse(mutState: MutState): E = {
        val a = paʹ.mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        val d = pdʹ.mutParse(mutState); if (mutState.isError) return dummy
        f(a, b, c, d)
      }

    }

}

class ApBuilder5[+A, +B, +C, +D, +E](pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D], pe: Parser[E]) {

  def void: Parser[Unit] =
    new ApBuilder5(pa.void, pb.void, pc.void, pd.void, pe.void).mapN((_, _, _, _, _) => ())

  def inputText: Parser[String] =
    void.inputText

  def tupled: Parser[(A, B, C, D, E)] =
    mapN(Tuple5.apply)

  def mapN[F](f: (A, B, C, D, E) => F): Parser[F] =
    new Parser[F] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      lazy val peʹ = pe

      override lazy val void: Parser[Unit] =
        paʹ.void ~>
        pbʹ.void ~>
        pcʹ.void ~>
        pdʹ.void ~>
        peʹ.void

      def mutParse(mutState: MutState): F = {
        val a = paʹ.mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        val d = pdʹ.mutParse(mutState); if (mutState.isError) return dummy
        val e = peʹ.mutParse(mutState); if (mutState.isError) return dummy
        f(a, b, c, d, e)
      }

    }

}

