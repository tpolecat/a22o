// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

class AltBuilder2[+A, +B](pa: Parser[A], pb: => Parser[B]) {

  // special case if there are only 2
  def either: Parser[Either[A, B]] =
    coproduct

  def |[C](pc: => Parser[C]): AltBuilder3[A, B, C] =
    new AltBuilder3(pa, pb, pc)

  def coproduct: Parser[Either[A, B]] =
    foldN(
      a => Left(a),
      b => Right(b)
    )

  def foldN[C](fa: A => C, fb: B => C): Parser[C] =
    new Parser[C] {

           val paʹ = pa
      lazy val pbʹ = pb

      override def void: Parser[Unit] =
        new AltBuilder2(pa.void, pb.void).foldN(_ => (), _ => ())

      def mutParse(mutState: MutState): C = {
        val pos = mutState.getPoint
                              val a = paʹ.mutParse(mutState); if (mutState.isOk) return fa(a)
        mutState.reset(pos);  val b = pbʹ.mutParse(mutState); if (mutState.isOk) return fb(b)
        dummy
      }

    }

}

object AltBuilder2 {

  implicit class MergeableAltBuilder2[A](ab: AltBuilder2[A, A]) {
    def merge: Parser[A] =
      ab.foldN(identity, identity)
  }

}

class AltBuilder3[+A, +B, +C](pa: Parser[A], pb: => Parser[B], pc: => Parser[C]) {

  def coproduct: Parser[Either[A, Either[B, C]]] =
    foldN(
      a => Left(a),
      b => Right(Left(b)),
      c => Right(Right(c))
    )

  def foldN[D](fa: A => D, fb: B => D, fc: C => D): Parser[D] =
    new Parser[D] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc

      override def void: Parser[Unit] =
        new AltBuilder3(pa.void, pb.void, pc.void).foldN(_ => (), _ => (), _ => ())

      def mutParse(mutState: MutState): D = {
        val pos = mutState.getPoint
                              val a = paʹ.mutParse(mutState); if (mutState.isOk) return fa(a)
        mutState.reset(pos);  val b = pbʹ.mutParse(mutState); if (mutState.isOk) return fb(b)
        mutState.reset(pos);  val c = pcʹ.mutParse(mutState); if (mutState.isOk) return fc(c)
        dummy
      }

    }

}




