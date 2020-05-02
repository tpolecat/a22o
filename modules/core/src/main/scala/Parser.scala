// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

import cats._
import cats.data._
import scala.annotation.tailrec

abstract class Parser[+A] private[a22o] { outer =>

  final def parse(input: String): (String, Either[String, A]) = {
    val s = new MutState(input)
    val a = mutParse(s)
    val r = s.remainingInput
    (r,
      if (s.isErrored) Left(s.getError)
      else Right(a)
    )
  }

  // TODO: make this private to the allocation tester, we would want to use .void for this if it
  // were intended for use by end-users
  final def accept(input: String): Boolean = {
    val s = new MutState(input)
    mutParse(s)
    s.isOk
  }

  // Implementations may wish to override to prevent allocating values which are then discarded.
  def void: Parser[Unit] =
    map(_ => ())

  final def as[B](b: B): Parser[B] =
    void.map(_ => b)

  final def map[B](f: A => B): Parser[B] =
    new Parser[B] {
      override def void = outer.void // throw away the map
      def mutParse(mutState: MutState): B = {
        val a = outer.mutParse(mutState)
        if (mutState.isOk) f(a)
        else dummy // don't call f if the parser failed
      }
    }

  final def emap[B](f: A => Either[String, B]): Parser[B] =
    new Parser[B] {
      def mutParse(mutState: MutState): B = {
        val o = mutState.getPoint
        val a = outer.mutParse(mutState)
        if (mutState.isOk) {
          f(a) match {
            case Right(a) => a
            case Left(e)  =>
              mutState.reset(o)
              mutState.setError(e)
              dummy
          }
        } else dummy // don't call f if the parser failed
      }
    }

  final def flatMap[B](f: A => Parser[B]): Parser[B] =
    new Parser[B] {
      def mutParse(mutState: MutState): B = {
        val a = outer.mutParse(mutState)
        if (mutState.isOk) {
          val b = f(a).mutParse(mutState)
          if (mutState.isOk) b
          else dummy
        } else dummy
      }
    }

  // Syntax delegates
  // N.B. ~ is added via syntax in the package object
  final def length: Parser[Int]                          = parser.text.length(this)
  final def text: Parser[String]                          = parser.text.text(this)
  final def opt: Parser[Option[A]]                        = parser.combinator.opt(this)
  final def <~[B](pb: => Parser[B]): Parser[A]            = parser.combinator.discardRight(this, pb)
  final def ~>[B](pb: => Parser[B]): Parser[B]            = parser.combinator.discardLeft(this, pb)
  final def token: Parser[A]                              = parser.text.token(this)


  final def many: AccumBuilder[A, Unit] =
    new AccumBuilder(this, 0, Int.MaxValue, parser.combinator.void)

  final def many1: AccumBuilder[A, Unit] =
    new AccumBuilder(this, 1, Int.MaxValue, parser.combinator.void)

  // The .text combinator means we don't need this anymore

  final def +(pb: => Parser[String])(implicit ev: A <:< String): Parser[String] =
    parser.text.concat(map(ev), pb)

  // grim
  protected final val dummy: A =
    null.asInstanceOf[A]

  // the contract is: on entry isError is false, offset is correct
  // on exit, offset is advanced on success, untouched on failure
  protected[a22o] def mutParse(mutState: MutState): A

}

object Parser {
  import parser.all.{ ok, fail }

  implicit def monoidParser[A](
    implicit ma: Monoid[A]
  ): Monoid[Parser[A]] =
    new Monoid[Parser[A]] {
      def combine(p0: Parser[A], p1: Parser[A]): Parser[A] = (p0 ~ p1).mapN(ma.combine)
      def empty: Parser[A] = ok(ma.empty)
    }

  implicit val MonoidKParser: MonoidK[Parser] =
    new MonoidK[Parser] {
      def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = (x | y).merge
      def empty[A]: Parser[A] = fail("No match.")
    }

  implicit val MonadErrorParser: MonadError[Parser, String] =
    new MonadError[Parser, String] {

      // todo: override ap2, ap3, ... map2, map3, ... tuple2, tuple3, ... imap2, imap3, ...

      override def map2[A, B, Z](fa: Parser[A], fb: Parser[B])(f: (A, B) => Z): Parser[Z] =
        (fa ~ fb).mapN(f)

      override def void[A](fa: Parser[A]): Parser[Unit] = fa.void
      override def as[A, B](fa: Parser[A], b: B): Parser[B] = fa.as(b)
      override def pure[A](a: A): Parser[A] = ok(a)
      override def raiseError[A](e: String): Parser[A] = fail(e)
      override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = fa.map(f)
      override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = fa.flatMap(f)
      override def handleErrorWith[A](fa: Parser[A])(f: String => Parser[A]): Parser[A] =
        new Parser[A] {
          def mutParse(mutState: MutState): A = {
            val o = mutState.getPoint
            val a = fa.mutParse(mutState)
            if (mutState.isOk) a
            else {
              val pa = f(mutState.getError)
              mutState.reset(o)
              pa.mutParse(mutState)
            }
          }
        }

      override def tailRecM[A, B](a: A)(f: A => Parser[Either[A,B]]): Parser[B] =
        new Parser[B] {
          def mutParse(mutState: MutState): B = {
            @tailrec def go(a: A): B = {
              val pab = f(a)
              val ab = pab.mutParse(mutState)
              if (mutState.isOk) {
                ab match {
                  case Left(a)  => go(a)
                  case Right(b) => b
                }
              } else dummy
            }
            go(a)
          }
        }

    }

}
