// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

import a22o.parser.base._
import a22o.parser.meta._
import java.text.ParseException
import a22o.parser.TextParser

object Parser extends TextParser.Constructors

abstract class Parser[+A] private[a22o] (override val toString: String = "<parser>")
  extends TextParser.Combinators[A] { outer =>

  final def accept(input: String): Boolean = {
    val s = new MutState(input)
    mutParse(s)
    s.isOk
  }

  /**
   * Attempt to parse the given input, returning the unconsumed remainder and either an error
   * or the computed result.
   * @group eliminators
   */
  final def parse(input: String): (String, Either[String, A]) = {
    val s = new MutState(input)
    val a = mutParse(s)
    val r = s.remainingInput
    (r, Either.cond(s.isOk, a, s.getError))
  }

  /**
   * Attempt to parse the given input completely, returning either an error or the computed result.
   * @group eliminators
   */
  final def parseAll(input: String): Either[String, A] = {
    val s = new MutState(input)
    val a = (this <~ endOfInput) mutParse(s)
    Either.cond(s.isOk, a, s.getError)
  }

  /**
   * Attempt to parse the given input, returning the unconsumed remainder and the computed result,
   * or raising a `ParseException` on failure.
   * @group eliminators
   */
  final def unsafeParse(input: String): (String, A) = {
    val s = new MutState(input)
    val a = mutParse(s)
    if (s.isOk) (s.remainingInput, a)
    else throw new ParseException(s.getError, s.getPoint)
  }

  /**
   * Attempt to parse the given input completely, returning the computed result, or raising a
   * `ParseException` ib failure.
   * @group eliminators
   */
  final def unsafeParseAll(input: String): A = {
    val s = new MutState(input)
    val a = (this <~ endOfInput) mutParse(s)
    if (s.isOk) a
    else throw new ParseException(s.getError, s.getPoint)
  }

  /**
   * An equivalent parser with the given name (and optionally a given name for its `void`
   * equivalent).
   * @group meta
   */
  final def named(name: String, voidName: Option[String] = None): Parser[A] =
    new Parser[A](name) {
      override lazy val void: Parser[Unit] = voidName.fold(outer.void)(outer.void.named(_))
      def mutParse(mutState: MutState): A = outer.mutParse(mutState)
    }

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
   * {{{
   * int.validate("must be positive")(_ > 0)
   * }}}
   * @group error handling
   */
  final def validate(desc: String)(f: A => Boolean): Parser[A] =
    new Parser[A](s"$this.validate") {
      val err = s"validate: $desc"
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
   * An equivalent parser that discards its result and instead yields the input that was consumed.
   * @group meta
   */
  final def inputText: Parser[String] =
    new Parser[String](s"$outer.text") {
      val paʹ = outer.void
      override lazy val void = paʹ
      def mutParse(mutState: MutState): String = {
        val p0 = mutState.getPoint
        paʹ.mutParse(mutState)
        if (mutState.isOk) {
          val p1 = mutState.getPoint
          mutState.setPoint(p0)
          mutState.consume(p1 - p0)
        } else {
          dummy
        }
      }
    }

  /**
   * An equivalent parser that discards its result and instead yields the number of characters
   * consumed. This is equationally the same as to `.text.map(_.length)` but is more efficient
   * because it avoids a String allocation.
   * @group meta
   */
  def consumed: Parser[Int] =
    new Parser[Int](s"$outer.consumed") {
      val paʹ = outer.void
      override lazy val void = paʹ
      override def consumed: Parser[Int] = this
      def mutParse(mutState: MutState): Int = {
        val p0 = mutState.getPoint
        paʹ.mutParse(mutState)
        if (mutState.isOk) {
          val p1 = mutState.getPoint
          p1 - p0
        } else {
          dummy
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
   * An equivalent parser that consumes no input.
   * @group meta
   */
  def peek: Parser[A] =
    new Parser[A](s"$outer.peek") {
      override def peek: Parser[A] = this
      override lazy val void = outer.void.peek
      def mutParse(mutState: MutState): A = {
        val o = mutState.getPoint
        val a = outer.mutParse(mutState)
        mutState.setPoint(o)
        a
      }
    }

  // grim
  protected final val dummy: A =
    null.asInstanceOf[A]

  // the contract is: on entry isError is false, offset is correct
  // on exit, offset is advanced on success, untouched on failure
  protected[a22o] def mutParse(mutState: MutState): A


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
    new AccumBuilder(this, 0, Int.MaxValue, unit)

  /** @group repetition */
  def many1: AccumBuilder[A, Unit] =
    new AccumBuilder(this, 1, Int.MaxValue, unit)

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
    (this.void ~> const(b)).named(s"${this.void}.as(...)")

}

