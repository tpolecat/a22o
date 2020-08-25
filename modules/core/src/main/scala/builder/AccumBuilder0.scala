// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o


package builder

import scala.annotation.tailrec
import scala.collection.Factory
import scala.annotation.unused

final class AccumBuilder0[+A](
  private val pa:    Parser[A],
  private val min:   Option[Int],
  private val max:   Option[Int],
) { outer =>

  // Good enough.
  require(min.forall(_ >= 0),   s"min ($min) must be non-negative")
  require(max.forall(_ >= 1),   s"max ($max, D must be positive")
  require(max.forall(max => min.forall(max >= _)), s"max ($max) must not be less than min ($min)")

  // Let's make this private for now
  private def copy[C](
    pa:    Parser[C] = pa,
    min:   Option[Int] = min,
    max:   Option[Int] = max,
  ): AccumBuilder0[C] =
    new AccumBuilder0(pa, min, max)

  private val minʹ = min.getOrElse(0)
  private val maxʹ = max.getOrElse(Int.MaxValue) // let's just pretend nobody will ever parse two billion things

  // Configurators
  def min(n: Int): AccumBuilder0[A] = copy(min = Some(n))
  def max(n: Int): AccumBuilder0[A] = copy(max = Some(n))
  def sepBy[D](p: Parser[D]): AccumBuilder[A, D] = new AccumBuilder(pa, min.getOrElse(0), max.getOrElse(Int.MaxValue), p)

  // Eliminators
  def to[T](fact: Factory[A, T]): Parser[T] = {
    // Use the nonstrict foldLeft so we get a new builder every time we parse!
    foldLeftʹ(fact.newBuilder)((b, a) => b += a).map(_.result())
  }

  // this is a common-enough case that it's worth specializing
  def void: Parser[Unit] =
    (min, max) match {

      // 0..
      case (None | Some(0), None) =>
        new Parser[Unit] {
          final val pa = outer.pa.void
          override val void: Parser[Unit] = this
          def mutParse(mutState: MutState): Unit = {
            // This is as minimal as I can get it but it's still much slower than fastparse.
            do pa.mutParse(mutState) while (mutState.isOk)
            mutState.setError(null)
          }
        }

      // 0..1
      case (None | Some(0), Some(1)) =>
        new Parser[Unit] {
          final val pa = outer.pa.void
          override val void: Parser[Unit] = this
          def mutParse(mutState: MutState): Unit = {
            pa.mutParse(mutState)
            mutState.setError(null)
          }
        }

      // 1..
      case (Some(1), None) =>
        new Parser[Unit] {
          final val pa = outer.pa.void
          override val void: Parser[Unit] = this
          def mutParse(mutState: MutState): Unit = {
            pa.mutParse(mutState)
            if (mutState.isOk) {
              do pa.mutParse(mutState) while (mutState.isOk)
              mutState.setError(null)
            }
          }
        }

      // m..n
      case _ =>
        new Parser[Unit] {
          val pa = outer.pa.void
          override val void: Parser[Unit] = this
          def mutParse(mutState: MutState): Unit = {
            @tailrec def go(count: Int): Unit = {
              pa.mutParse(mutState)
              if (mutState.isOk && count != maxʹ) {
                go(count + 1) // keep going
              } else {
                // done, failure depends on count
                if (count >= minʹ) // it's ok, we have enough. stop. point is in the right place
                  mutState.setError(null)
                // else propagate the failure
              }
            }
            go(1)
          }
        }

    }

  def inputText: Parser[String] =
    void.inputText

  def foldLeft[T](z: T)(f: (T, A) => T): Parser[T] =
    foldLeftʹ(z)(f)

  // Ok this one is special because we don't want to evaluate `z` until we get to mutParse. This
  // lets `to` above take a factory and pass `.newBuilder` for `z` here and not end up reusing the
  // same builder over and over. Super sketchy, sorry.
  private def foldLeftʹ[T](z: => T)(f: (T, A) => T): Parser[T] =
    new Parser[T] {
      val atLeastOne = minʹ > 0
      val noMax = max.isEmpty // TODO: special case this for * ? +
      override lazy val void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): T = {

        var accum: T   = z
        var count: Int = 0

        // invariant: point must be after the last matched element

        // first element
        val p = mutState.getPoint
        val a = pa.mutParse(mutState)
        if (mutState.isOk) {
          count = count + 1
          accum = f(accum, a)
        } else {
          mutState.setPoint(p)
          mutState.setError("no matches")
          if (atLeastOne) return dummy // micro-optimization, really helps!
        }

        // TODO: productivity checks

        // subsequent sequence of delim + elem
        while (mutState.isOk && noMax || (count < maxʹ)) {
          val p = mutState.getPoint
          val a = pa.mutParse(mutState)
          if (mutState.isOk) {
            count = count + 1
            accum = f(accum, a)
          } else {
            mutState.setPoint(p)
            mutState.setError("expected element")
          }
        }

        // done
        if (count < minʹ) {
          mutState.setError("not enough elements")
          dummy
        } else {
          mutState.setError(null)
          accum
        }

      }
    }

}

object AccumBuilder0 {

  implicit class InvariantOps[A](self: AccumBuilder0[A]) {

    @inline def reduce(@unused f: (A, A) => A): Parser[A] = {
      assert(self.min.exists(_ >= 1), s"reduce requires that min (${self.min}) be positive.")
      Parser.fail("not implemented")
    }

  }

}