// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

import cats._
import cats.data._
import scala.annotation.tailrec

abstract class Parser[+A] private[a22o] { outer =>

  def parse(input: String): (String, Either[String, A]) = {
    val s = new MutState(input)
    val a = mutParse(s)
    val r = s.input.substring(s.offset)
    (r,
      if (s.isErrored) Left(s"Parse error at position ${s.offset}: ${s.error}")
      else Right(a)
    )
  }

  // Implementations may wish to override to prevent allocating values which are then discarded.
  def void: Parser[Unit] =
    map(_ => ())

  def map[B](f: A => B): Parser[B] =
    new Parser[B] {
      override def void = outer.void // throw away the map
      def mutParse(mutState: MutState): B = {
        val a = outer.mutParse(mutState)
        if (mutState.isOk) f(a)
        else dummy // don't call f if the parser failed
      }
    }

  def emap[B](f: A => Either[String, B]): Parser[B] =
    new Parser[B] {
      def mutParse(mutState: MutState): B = {
        val o = mutState.offset
        val a = outer.mutParse(mutState)
        if (mutState.isOk) {
          f(a) match {
            case Right(a) => a
            case Left(e)  =>
              mutState.reset(o)
              mutState.error = e
              dummy
          }
        } else dummy // don't call f if the parser failed
      }
    }

  def flatMap[B](f: A => Parser[B]): Parser[B] =
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
  def ~[B](pb: => Parser[B]): Parser[A ~ B]         = parser.combinator.pair(this, pb)
  def |[B >: A](pb: => Parser[B]): Parser[B]        = parser.combinator.or(this, pb)
  def ||[B](pb: => Parser[B]): Parser[Either[A, B]] = parser.combinator.either(this, pb)
  def opt: Parser[Option[A]]                        = parser.combinator.opt(this)
  def many: Parser[List[A]]                         = parser.combinator.many(this)
  def many1: Parser[NonEmptyList[A]]                = parser.combinator.many1(this)
  def skipMany: Parser[Unit]                        = parser.combinator.skipMany(this)
  def skipMany1: Parser[Unit]                       = parser.combinator.skipMany1(this)
  def <~[B](pb: => Parser[B]): Parser[A]            = parser.combinator.discardRight(this, pb)
  def ~>[B](pb: => Parser[B]): Parser[B]            = parser.combinator.discardLeft(this, pb)
  def token: Parser[A]                              = parser.text.token(this)

  // grim
  protected def dummy: A =
    null.asInstanceOf[A]

  // the contract is: on entry isError is false, offset is correct
  // on exit, offset is advanced on success, untouched on failure
  protected[a22o] def mutParse(mutState: MutState): A

}

object Parser {
  import parser.all.{ ok, fail, zipWith, or }

  implicit def monoidParser[A](
    implicit ma: Monoid[A]
  ): Monoid[Parser[A]] =
    new Monoid[Parser[A]] {
      def combine(p0: Parser[A], p1: Parser[A]): Parser[A] = zipWith(p0, p1)(ma.combine)
      def empty: Parser[A] = ok(ma.empty)
    }

  implicit val MonoidKParser: MonoidK[Parser] =
    new MonoidK[Parser] {
      def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = or(x, y)
      def empty[A]: Parser[A] = fail("No match.")
    }

  implicit val MonadErrorParser: MonadError[Parser, String] =
    new MonadError[Parser, String] {

      override def pure[A](a: A): Parser[A] = ok(a)
      override def raiseError[A](e: String): Parser[A] = fail(e)
      override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = fa.map(f)
      override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = fa.flatMap(f)

      override def handleErrorWith[A](fa: Parser[A])(f: String => Parser[A]): Parser[A] =
        new Parser[A] {
          def mutParse(mutState: MutState): A = {
            val o = mutState.offset
            val a = fa.mutParse(mutState)
            if (mutState.isOk) a
            else {
              val pa = f(mutState.error)
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
