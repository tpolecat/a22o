// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

import scala.collection.Factory
// import cats.Monoid

final class AccumBuilder[+A, B](
  private val pa:    Parser[A],
  private val min:   Int,
  private val max:   Int,
  private val sepBy: Parser[B]
) { outer =>

  // Good enough.
  require(min >= 0,   s"min ($min) must be non-negative")
  require(max >= 1,   s"max ($max, D must be positive")
  require(max >= min, s"max ($max) must not be less than min ($min)")

  // Let's make this private for now
  private def copy[C, D](
    pa:    Parser[C] = pa,
    min:   Int       = min,
    max:   Int       = max,
    sepBy: Parser[D] = sepBy
  ): AccumBuilder[C, D] =
    new AccumBuilder(pa, min, max, sepBy)

  // Configurators
  def min(n: Int): AccumBuilder[A, B] = copy(min = n)
  def max(n: Int): AccumBuilder[A, B] = copy(max = n)
  def sepBy[D](p: Parser[D]): AccumBuilder[A, D] = copy(sepBy = p)

  // Eliminators
  def as[T](fact: Factory[A, T]): Parser[T] =
    foldLeft(fact.newBuilder)(_ += _).map(_.result)

  def void: Parser[Unit] =
    copy(pa = pa.void).foldLeft(())((_, _) => ())

  def text: Parser[String] =
    void.text

  // // move to cats module
  // def foldMap[B](f: A => B)(implicit ev: Monoid[B]): Parser[B] =
  //   foldLeft(ev.empty)((b, a) => ev.combine(b, f(a)))

  // def fold(implicit ev: Monoid[A]): Parser[A] =
  //   foldMap(identity)


  // fold that lets you look at the delimiter
  def foldSep[T](z: T)(inj: A => T, ind: B => (T, A) => T): Parser[T] =
    new Parser[T] {
      override def void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): T = {

        var accum: T   = z
        var count: Int = 0

        // invariant: point must be after the last matched element

        // first element
        val p = mutState.getPoint
        val a = pa.mutParse(mutState)
        if (mutState.isOk) {
          count = count + 1
          accum = inj(a)
        } else {
          mutState.setPoint(p)
          mutState.setError("no matches")
        }

        // TODO: productivity checks

        // subsequent sequence of delim + elem
        while (count < max && mutState.isOk) {

          val p = mutState.getPoint
          val b = sepBy.mutParse(mutState)
          if (mutState.isErrored) {
            mutState.setPoint(p)
            mutState.setError("expected delimiter")
          } else {
            val p = mutState.getPoint
            val a = pa.mutParse(mutState)
            if (mutState.isOk) {
              count = count + 1
              accum = ind(b)(accum, a)
            } else {
              mutState.setPoint(p)
              mutState.setError("expected element")
            }
          }
        }

        // done
        if (count < min) {
          mutState.setError("not enough elements")
          dummy
        } else {
          mutState.setError(null)
          accum
        }

      }
    }

  def foldSepA[AA >: A](z: AA)(f: B => (AA, A) => AA): Parser[AA] =
    foldSep(z)(identity, f)

  def foldLeft[T](z: T)(f: (T, A) => T): Parser[T] =
    copy(sepBy = sepBy.void).foldSep(z)(f(z, _), _ => f)

}

object AccumBuilder {

  implicit class InvariantOps[A, B](self: AccumBuilder[A, B]) {

    def reduceSepA(f: B => (A, A) => A): Parser[A] = {
      assert(self.min >= 1, s"reduceSepA requires that min (${self.min}) be positive.")
      self.foldSep(null.asInstanceOf[A])(identity, f)
    }

  }

}