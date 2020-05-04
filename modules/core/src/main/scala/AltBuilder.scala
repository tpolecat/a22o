// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

class AltBuilder2[+A, +B] private[a22o] (pa: Parser[A], pb: => Parser[B]) {

  /** @group eliminators */
  def either: Parser[Either[A, B]] =
    coproduct

  /** @group alternation */
  def |[C](pc: => Parser[C]): AltBuilder3[A, B, C] =
    new AltBuilder3(pa, pb, pc)

  /** @group eliminators */
  def coproduct: Parser[Either[A, B]] =
    foldN(
      a => Left(a),
      b => Right(b)
    )

  /** @group eliminators */
  def foldN[C](fa: A => C, fb: B => C): Parser[C] =
    new Parser[C] {

           val paʹ = pa
      lazy val pbʹ = pb

      override lazy val void: Parser[Unit] =
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
    /**
     * Merge alternatives to their least upper bound.
     * @group eliminators
     */
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

      override lazy val void: Parser[Unit] =
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




