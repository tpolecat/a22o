// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

import a22o.builder._

object BaseParser {

  object Constructors extends Constructors
  trait Constructors {

    /** @group base */
    val unit: Parser[Unit] =
      const(()).named("unit")

    /** @group base */
    def const[A](a: A): Parser[A] =
      new Parser[A](s"const($a)") {
        override lazy val void: Parser[Unit] = unit
        override lazy val peek: Parser[A] = this
        override lazy val consumed: Parser[Int] = const(0)
        def mutParse(mutState: MutState): A = a
      }

    /** @group base */
    def fail[A](message: String): Parser[A] =
      new Parser[A]("fail(...)") {
        override lazy val void: Parser[Unit] = fail(message)
        final override def peek: Parser[A] = this
        def mutParse(mutState: MutState): A = {
          mutState.setError(message)
          dummy
        }
      }

    /** @group text */
    def take(n: Int): Parser[String] =
      new Parser[String](s"take($n)") {
        override lazy val void = skip(n)
        override def consumed: Parser[Int] = void.consumed
        def mutParse(mutState: MutState): String =
          if (n < 0) {
            mutState.setError("take: negative length")
            dummy
          } else if (mutState.remaining >= n) {
            val s = mutState.consume(n)
            s
          } else {
            mutState.setError("take: insufficient input")
            dummy
          }
      }

    /** @group base */
    def skip(n: Int): Parser[Unit] =
      new Parser[Unit](s"skip($n)") {
        override lazy val void = this
        def mutParse(mutState: MutState): Unit =
          if (n < 0) {
            mutState.setError("skip: negative length")
            dummy
          } else if (mutState.remaining >= n) {
            mutState.advance(n)
            ()
          } else {
            mutState.setError("skip: insufficient input")
            dummy
          }
      }

  }

  trait Combinators[+A] { outer: Parser[A] =>

    /**
     * An equivalent parser that discards its result. This is equationally the same as `.map(_ => ())`
     * but is more efficient because implementations can often avoid computing results at all.
     * @group transformation
     */
    lazy val void: Parser[Unit] =
      new Parser[Unit](s"$outer.void") {
        override def consumed: Parser[Int] = outer.consumed
        override lazy val void: Parser[Unit] = this
        def mutParse(mutState: MutState): Unit = {
          outer.mutParse(mutState)
          if (mutState.isOk) ()
          else dummy
        }
      }

    /**
     * An equivalent parser that applies `f` to its computed result. Prefer `.as` for constant
     * functions and `.void` for the unit function as these can often avoid computing the underlying
     * result.
     * @group transformation
     */
    final def map[B](f: A => B): Parser[B] =
      new Parser[B] {
        override lazy val void = outer.void // we can throw away the map
        def mutParse(mutState: MutState): B = {
          val a = outer.mutParse(mutState)
          if (mutState.isOk) f(a)
          else dummy // don't call f if the parser failed
        }
      }

    /**
     * An equivalent parser that applies secondary validation to the computed result, failing if the
     * supplied predicate `f` returns `false`.
     * @group error handling
     */
    final def filter(f: A => Boolean): Parser[A] =
      new Parser[A](s"$this.filter") {
        val err = s"${outer.toString}: filter failed"
        def mutParse(mutState: MutState): A = {
          val o = mutState.getPoint
          val a = outer.mutParse(mutState)
          if (mutState.isOk) {
            f(a) match {
              case true  => a
              case false =>
                mutState.reset(o)
                mutState.setError(err)
                dummy
            }
          } else dummy // don't call f if the parser failed
        }
      }

    /**
     * An equivalent parser that never fails, yielding `z` on error and applying `f` to the computed
     * result on success.
     * @group error handling
     */
    final def fold[B](z: => B)(f: A => B): Parser[B] =
      new Parser[B](s"$outer.fold(...)(...)") {
        lazy val zʹ = z
        override lazy val void: Parser[Unit] = outer.void.fold(())(identity)
        def mutParse(mutState: MutState): B = {
          val a = outer.mutParse(mutState)
          if (mutState.isOk) f(a)
          else {
            mutState.setError(null)
            zʹ
          }
        }
      }

    /**
     * Sequence this parser with another parser, based on a predicate on the computed value.
     * Prefer this over `flatMap` when possible, as `select` incurs no allocation overhead.
     * @group branching
     */
    def select[B](f: A => Boolean, ft: => Parser[B], ff: => Parser[B]): Parser[B] =
      new Parser[B] {
        lazy val ftʹ = ft
        lazy val ffʹ = ff
        override lazy val void: Parser[Unit] = outer.select(f, ft.void, ff.void)
        def mutParse(mutState: MutState): B = {
          val b = outer.mutParse(mutState)
          if (mutState.isOk) {
            if (f(b)) ftʹ.mutParse(mutState)
            else      ffʹ.mutParse(mutState)
          } else dummy
        }
      }

    /**
     * Sequence this parser with another parser, based on the computed value. Care should be taken not
     * to construct parsers in the body of `f` if it can be avoided. Prefer `select` when possible.
     * @group branching
     */
    final def flatMap[B](f: A => Parser[B]): Parser[B] =
      new Parser[B] {
        override lazy val void = outer.flatMap(a => f(a).void)
        def mutParse(mutState: MutState): B = {
          val a = outer.mutParse(mutState)
          if (mutState.isOk) {
            val b = f(a).mutParse(mutState)
            if (mutState.isOk) b
            else dummy
          } else dummy
        }
      }

    /**
     * @group error handling
     */
    def onError[B >: A](a: => B): Parser[B] =
      this.fold(a)(identity)

    /** @group sequencing */
    def <~[B](pb: => Parser[B]): Parser[A] =
      (this ~ pb.void).mapN((a, _) => a).named(s"($this <~ ...)")

    /** @group sequencing */
    def ~>[B](pb: => Parser[B]): Parser[B] =
      (this.void ~ pb).mapN((_, b) => b).named(s"($this ~> ...)")

    /** @group repetition */
    def many: AccumBuilder[A, Unit] =
      new AccumBuilder(this, 0, Int.MaxValue, Constructors.unit)

    /** @group repetition */
    def many1: AccumBuilder[A, Unit] =
      new AccumBuilder(this, 1, Int.MaxValue, Constructors.unit)

    /** @group sequencing */
    def ~[B](pb: => Parser[B]) =
      new ApBuilder2[A, B](this, pb)

    /** @group alternation */
    def |[B](pb: => Parser[B]) =
      new AltBuilder2[A, B](this, pb)

    /**
     * An equivalent parser that never fails, yielding `Some` on success and `None` on failure.
     * @group error handling
     */
    def opt: Parser[Option[A]] =
      this.fold(Option.empty[A])(Some(_)).named(s"$this.opt")

    /**
     * An equivalent parser that replaces its result with the given value. This is equationally the
     * same as `.map(_ => a)` but is more efficient because implementations can often avoid computing
     * results at all.
     * @group transformation
     */
    def as[B](b: B): Parser[B] =
      (this.void ~> Constructors.const(b)).named(s"${this.void}.as(...)")

  }

}
