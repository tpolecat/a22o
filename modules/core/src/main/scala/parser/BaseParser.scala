// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

import a22o.builder._

object BaseParser {

  /** Const consumes no input and never fail. */
  private final class Const[A](val a: A) extends Parser[A](if (a == ()) "unit" else s"const($a)") {
    @inline override def as[B](b: B): Parser[B] = Constructors.const(b)
    @inline override def consumed: Parser[Int] = Constructors.const(0)
    @inline override def flatMap[B](f: A => Parser[B]): Parser[B] = f(a)
    @inline override def inputText: Parser[String] = Constructors.const("")
    @inline override def map[B](f: A => B): Parser[B] = Constructors.const(f(a))
    @inline override def onError[B >: A](b: => B): Parser[B] = this
    @inline override def opt: Parser[Option[A]] = Constructors.const(Some(a))
    @inline override def peek: Parser[A] = this
    @inline override def void: Parser[Unit] = Constructors.unit
    @inline override def mutParse(mutState: MutState): A = a
    override def equivalentTo[B >: A](other: Parser[B]): Boolean =
      other match {
        case p: Const[_] => p.a == a
        case _ => false
      }
  }

  /**
   * Fail consumes no input and always fails. All combinators other than error-handling are no-ops.
   */
  private final class Fail[A](val message: String) extends Parser[A]("fail(...)") {
    @inline def coerce[B] = this.asInstanceOf[Parser[B]] // this is sound because `A` is a phantom!
    @inline override def as[B](b: B): Parser[B] = coerce
    @inline override def consumed: Parser[Int] = coerce
    @inline override def flatMap[B](f: A => Parser[B]): Parser[B] = coerce
    @inline override def inputText: Parser[String] = coerce
    @inline override def map[B](f: A => B): Parser[B] = coerce
    @inline override def onError[B >: A](b: => B): Parser[B] = Constructors.const(b)
    @inline override def opt: Parser[Option[A]] = Constructors.const(None)
    @inline override def peek: Parser[A] = coerce
    @inline override def void: Parser[Unit] = coerce
    @inline override def mutParse(mutState: MutState): A = {
      mutState.setError(message)
      dummy
    }
    override def equivalentTo[B >: A](other: Parser[B]): Boolean =
      other match {
        case p: Fail[_] => p.message == message
        case _ => false
      }
   }


  /**
   * Fail consumes no input and always fails. All combinators other than error-handling are no-ops.
   */
  private final class Skip(val n: Int) extends Parser[Unit](s"skip($n)") {
    @inline override def void = this
    @inline override def inputText: Parser[String] = TextParser.Constructors.take(n)
    @inline override def mutParse(mutState: MutState): Unit =
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
    override def equivalentTo[B >: Unit](other: Parser[B]): Boolean =
      other match {
        case p: Skip => p.n == n
        case _ => false
      }
   }

  object Constructors extends Constructors
  trait Constructors {

    /** @group base */
    val unit: Parser[Unit] =
      new Const(())

    /** @group base */
    def const[A](a: A): Parser[A] =
      new Const[A](a)

    /** @group base */
    def fail[A](message: String): Parser[A] =
      new Fail(message)

    /** @group base */
    def skip(n: Int): Parser[Unit] =
      if (n == 0) unit else new Skip(n)

  }

  trait Combinators[+A] { outer: Parser[A] =>

    /**
     * An equivalent parser with the given name.
     * @group meta
     */
    def named(name: String): Parser[A] =
      new Parser[A](name) {
        @inline override def mutParse(mutState: MutState): A =
          outer.mutParse(mutState)
      }

    /**
     * An equivalent parser that discards its result. This is equationally the same as `.map(_ => ())`
     * but is more efficient because implementations can often avoid computing results at all.
     * @group transformation
     */
    def void: Parser[Unit] =
      new Parser[Unit](s"$outer.void") {
        @inline override def void: Parser[Unit] = this
        @inline override def mutParse(mutState: MutState): Unit = {
          outer.mutParse(mutState)
          if (mutState.isOk) ()
          else dummy
        }
      }

    /**
     * An equivalent parser that discards its result and never fails. Use this combinator to skip
     * optional input text.
     * @group error handling
     */
    def orNot: Parser[Unit] =
      new Parser[Unit](s"$outer.orNot") {
        @inline override def void: Parser[Unit] = this
        @inline override def mutParse(mutState: MutState): Unit = {
          outer.mutParse(mutState)
          if (mutState.isError)
            mutState.setError(null)
        }
      }

    /**
     * An equivalent parser that applies `f` to its computed result. Prefer `.as` for constant
     * functions and `.void` for the unit function as these can often avoid computing the underlying
     * result.
     * @group transformation
     */
    def map[B](f: A => B): Parser[B] =
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
    def filter(f: A => Boolean): Parser[A] =
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
    def fold[B](z: => B)(f: A => B): Parser[B] =
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
    def flatMap[B](f: A => Parser[B]): Parser[B] =
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
     * An equivalent parser that never fails, but instead yields constant value `b` in cases where
     * parsing would have failed.
     * @group error handling
     */
    def onError[B >: A](b: => B): Parser[B] =
      this.fold(b)(identity)

    /** @group sequencing */
    def <~[B](pb: => Parser[B]): Parser[A] =
      (this ~ pb.void).mapN((a, _) => a).named(s"($this <~ ...)")

    /** @group sequencing */
    def ~>[B](pb: => Parser[B]): Parser[B] =
      (this.void ~ pb).mapN((_, b) => b).named(s"($this ~> ...)")

    /** @group repetition */
    def many: AccumBuilder0[A] =
      new AccumBuilder0(this, None, None)

    /** @group repetition */
    def many1: AccumBuilder0[A] =
      new AccumBuilder0(this, Some(1), None)

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
     * Alternative name for `opt`.
     * @group error handling
     */
    def option: Parser[Option[A]] = opt

    /**
     * An equivalent parser that never fails, yielding an error or successful result.
     * @group error handling
     */
    def attempt: Parser[Either[String, A]] =
      new Parser[Either[String, A]](s"$outer.attempt") {
        override lazy val void: Parser[Unit] = outer.void.orNot
        def mutParse(mutState: MutState): Either[String, A] = {
          val a = outer.mutParse(mutState)
          if (mutState.isOk) Right(a)
          else {
            mutState.setError(null)
            Left(mutState.getError)
          }
        }
      }

    /**
     * Alternative name for `attempt`.
     * @group error handling
     */
    def either: Parser[Either[String, A]] = attempt

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
