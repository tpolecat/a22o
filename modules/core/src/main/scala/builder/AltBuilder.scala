// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

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

  def void: Parser[Unit] =
    foldN(_ => (), _ => ()).void

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

  /** @group alternation */
  def |[D](pd: => Parser[D]): AltBuilder4[A, B, C, D] =
    new AltBuilder4(pa, pb, pc, pd)

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

object AltBuilder3 {

  implicit class MergeableAltBuilder3[A](ab: AltBuilder3[A, A, A]) {
    /**
     * Merge alternatives to their least upper bound.
     * @group eliminators
     */
    def merge: Parser[A] =
      ab.foldN(identity, identity, identity)
  }

}

class AltBuilder4[+A, +B, +C, +D](pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D]) {

  /** @group alternation */
  def |[E](pe: => Parser[E]): AltBuilder5[A, B, C, D, E] =
    new AltBuilder5(pa, pb, pc, pd, pe)

  def coproduct: Parser[Either[A, Either[B, Either[C, D]]]] =
    foldN(
      a => Left(a),
      b => Right(Left(b)),
      c => Right(Right(Left(c))),
      d => Right(Right(Right(d)))
    )

  def foldN[E](fa: A => E, fb: B => E, fc: C => E, fd: D => E): Parser[E] =
    new Parser[E] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd

      override lazy val void: Parser[Unit] =
        new AltBuilder4(pa.void, pb.void, pc.void, pd.void).foldN(_ => (), _ => (), _ => (), _ => ())

      def mutParse(mutState: MutState): E = {
        val pos = mutState.getPoint
                              val a = paʹ.mutParse(mutState); if (mutState.isOk) return fa(a)
        mutState.reset(pos);  val b = pbʹ.mutParse(mutState); if (mutState.isOk) return fb(b)
        mutState.reset(pos);  val c = pcʹ.mutParse(mutState); if (mutState.isOk) return fc(c)
        mutState.reset(pos);  val d = pdʹ.mutParse(mutState); if (mutState.isOk) return fd(d)
        dummy
      }

    }

}

object AltBuilder4 {

  implicit class MergeableAltBuilder4[A](ab: AltBuilder4[A, A, A, A]) {
    /**
     * Merge alternatives to their least upper bound.
     * @group eliminators
     */
    def merge: Parser[A] =
      ab.foldN(identity, identity, identity, identity)
  }

}

class AltBuilder5[+A, +B, +C, +D, +E](pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D], pe: => Parser[E]) {

  /** @group alternation */
  def |[F](pf: => Parser[F]): AltBuilder6[A, B, C, D, E, F] =
    new AltBuilder6(pa, pb, pc, pd, pe, pf)

  def coproduct: Parser[Either[A, Either[B, Either[C, Either[D, E]]]]] =
    foldN(
      a => Left(a),
      b => Right(Left(b)),
      c => Right(Right(Left(c))),
      d => Right(Right(Right(Left(d)))),
      e => Right(Right(Right(Right(e))))
    )

  def foldN[F](fa: A => F, fb: B => F, fc: C => F, fd: D => F, fe: E => F): Parser[F] =
    new Parser[F] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      lazy val peʹ = pe

      override lazy val void: Parser[Unit] =
        new AltBuilder5(pa.void, pb.void, pc.void, pd.void, pe.void).foldN(_ => (), _ => (), _ => (), _ => (), _ => ())

      def mutParse(mutState: MutState): F = {
        val pos = mutState.getPoint
                              val a = paʹ.mutParse(mutState); if (mutState.isOk) return fa(a)
        mutState.reset(pos);  val b = pbʹ.mutParse(mutState); if (mutState.isOk) return fb(b)
        mutState.reset(pos);  val c = pcʹ.mutParse(mutState); if (mutState.isOk) return fc(c)
        mutState.reset(pos);  val d = pdʹ.mutParse(mutState); if (mutState.isOk) return fd(d)
        mutState.reset(pos);  val e = peʹ.mutParse(mutState); if (mutState.isOk) return fe(e)
        dummy
      }

    }

}

object AltBuilder5 {

  implicit class MergeableAltBuilder5[A](ab: AltBuilder5[A, A, A, A, A]) {
    /**
     * Merge alternatives to their least upper bound.
     * @group eliminators
     */
    def merge: Parser[A] =
      ab.foldN(identity, identity, identity, identity, identity)
  }

}

class AltBuilder6[+A, +B, +C, +D, +E, +F](pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D], pe: => Parser[E], pf: => Parser[F]) {

  /** @group alternation */
  def |[G](pg: => Parser[G]): AltBuilder7[A, B, C, D, E, F, G] =
    new AltBuilder7(pa, pb, pc, pd, pe, pf, pg)

  def coproduct: Parser[Either[A, Either[B, Either[C, Either[D, Either[E, F]]]]]] =
    foldN(
      a => Left(a),
      b => Right(Left(b)),
      c => Right(Right(Left(c))),
      d => Right(Right(Right(Left(d)))),
      e => Right(Right(Right(Right(Left(e))))),
      f => Right(Right(Right(Right(Right(f)))))
    )

  def foldN[G](fa: A => G, fb: B => G, fc: C => G, fd: D => G, fe: E => G, ff: F => G): Parser[G] =
    new Parser[G] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      lazy val peʹ = pe
      lazy val pfʹ = pf

      override lazy val void: Parser[Unit] =
        new AltBuilder6(pa.void, pb.void, pc.void, pd.void, pe.void, pf.void).foldN(_ => (), _ => (), _ => (), _ => (), _ => (), _ => ())

      def mutParse(mutState: MutState): G = {
        val pos = mutState.getPoint
                              val a = paʹ.mutParse(mutState); if (mutState.isOk) return fa(a)
        mutState.reset(pos);  val b = pbʹ.mutParse(mutState); if (mutState.isOk) return fb(b)
        mutState.reset(pos);  val c = pcʹ.mutParse(mutState); if (mutState.isOk) return fc(c)
        mutState.reset(pos);  val d = pdʹ.mutParse(mutState); if (mutState.isOk) return fd(d)
        mutState.reset(pos);  val e = peʹ.mutParse(mutState); if (mutState.isOk) return fe(e)
        mutState.reset(pos);  val f = pfʹ.mutParse(mutState); if (mutState.isOk) return ff(f)
        dummy
      }

    }

}

object AltBuilder6 {

  implicit class MergeableAltBuilder6[A](ab: AltBuilder6[A, A, A, A, A, A]) {
    /**
     * Merge alternatives to their least upper bound.
     * @group eliminators
     */
    def merge: Parser[A] =
      ab.foldN(identity, identity, identity, identity, identity, identity)
  }

}

class AltBuilder7[+A, +B, +C, +D, +E, +F, +G](pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D], pe: => Parser[E], pf: => Parser[F], pg: => Parser[G]) {

  def coproduct: Parser[Either[A, Either[B, Either[C, Either[D, Either[E, Either[F, G]]]]]]] =
    foldN(
      a => Left(a),
      b => Right(Left(b)),
      c => Right(Right(Left(c))),
      d => Right(Right(Right(Left(d)))),
      e => Right(Right(Right(Right(Left(e))))),
      f => Right(Right(Right(Right(Right(Left(f)))))),
      g => Right(Right(Right(Right(Right(Right(g))))))
    )

  def foldN[H](fa: A => H, fb: B => H, fc: C => H, fd: D => H, fe: E => H, ff: F => H, fg: G => H): Parser[H] =
    new Parser[H] {

           val paʹ = pa
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      lazy val peʹ = pe
      lazy val pfʹ = pf
      lazy val pgʹ = pg

      override lazy val void: Parser[Unit] =
        new AltBuilder7(pa.void, pb.void, pc.void, pd.void, pe.void, pf.void, pg.void).foldN(_ => (), _ => (), _ => (), _ => (), _ => (), _ => (), _ => ())

      def mutParse(mutState: MutState): H = {
        val pos = mutState.getPoint
                              val a = paʹ.mutParse(mutState); if (mutState.isOk) return fa(a)
        mutState.reset(pos);  val b = pbʹ.mutParse(mutState); if (mutState.isOk) return fb(b)
        mutState.reset(pos);  val c = pcʹ.mutParse(mutState); if (mutState.isOk) return fc(c)
        mutState.reset(pos);  val d = pdʹ.mutParse(mutState); if (mutState.isOk) return fd(d)
        mutState.reset(pos);  val e = peʹ.mutParse(mutState); if (mutState.isOk) return fe(e)
        mutState.reset(pos);  val f = pfʹ.mutParse(mutState); if (mutState.isOk) return ff(f)
        mutState.reset(pos);  val g = pgʹ.mutParse(mutState); if (mutState.isOk) return fg(g)
        dummy
      }

    }

}

object AltBuilder7 {

  implicit class MergeableAltBuilder7[A](ab: AltBuilder7[A, A, A, A, A, A, A]) {
    /**
     * Merge alternatives to their least upper bound.
     * @group eliminators
     */
    def merge: Parser[A] =
      ab.foldN(identity, identity, identity, identity, identity, identity, identity)
  }

}
