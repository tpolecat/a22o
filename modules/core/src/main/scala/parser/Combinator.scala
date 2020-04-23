// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

import scala.annotation.tailrec
import cats.data.NonEmptyList

trait Combinator {

  def ok[A](a: A): Parser[A] =
    new Parser[A] {
      def mutParse(mutState: MutState): A = a
    }

  def fail(message: String): Parser[Nothing] =
    new Parser[Nothing] {
      def mutParse(mutState: MutState): Nothing = {
        mutState.error = message
        dummy
      }
    }

  def zipWith[A, B, C](pa: Parser[A], pb: => Parser[B], f: (A, B) => C): Parser[C] =
    new Parser[C] {
      def mutParse(mutState: MutState): C = {
        val a = pa.mutParse(mutState)
        if (mutState.isOk) {
          val b = pb.mutParse(mutState)
          if (mutState.isOk) f(a, b)
          else dummy
        } else dummy
      }
    }

  def pair[A, B](pa: Parser[A], pb: => Parser[B]): Parser[A ~ B] =
    zipWith[A, B, A ~ B](pa, pb, (_, _))

  def discardRight[A, B](pa: Parser[A], pb: => Parser[B]): Parser[A] =
      zipWith[A, B, A](pa, pb, (a, _) => a)

  def discardLeft[A, B](pa: Parser[A], pb: => Parser[B]): Parser[B] =
      zipWith[A, B, B](pa, pb, (_, b) => b)

  def or[A](pa: Parser[A], pb: => Parser[A]): Parser[A] =
    new Parser[A] {
      def mutParse(mutState: MutState): A = {
        val o = mutState.offset
        val a = pa.mutParse(mutState)
        if (mutState.isOk) a
        else {
          mutState.reset(o)
          val b = pb.mutParse(mutState)
          if (mutState.isOk) b
          else dummy
        }
      }
    }

  def either[A, B](pa: Parser[A], pb: => Parser[B]): Parser[Either[A, B]] =
    new Parser[Either[A, B]] {
      def mutParse(mutState: MutState): Either[A, B] = {
        val o = mutState.offset
        val a = pa.mutParse(mutState)
        if (mutState.isOk) Left(a)
        else {
          mutState.reset(o)
          val b = pb.mutParse(mutState)
          if (mutState.isOk) Right(b)
          else dummy
        }
      }
    }

  def many[A](pa: Parser[A]): Parser[List[A]] =
    new Parser[List[A]] {
      def mutParse(mutState: MutState): List[A] = {
        val list = List.newBuilder[A]
        @tailrec def go: List[A] = {
          val o = mutState.offset
          val a = pa.mutParse(mutState)
          if (mutState.isOk) {
            list += a
            go
          } else {
            mutState.offset = o
            mutState.error  = null
            list.result
          }
        }
        go
      }
    }

  def skipMany[A](pa: Parser[A]): Parser[Unit] =
    new Parser[Unit] {
      def mutParse(mutState: MutState): Unit = {
        @tailrec def go(): Unit = {
          val o = mutState.offset
          pa.mutParse(mutState)
          if (mutState.isOk) {
            go()
          } else {
            mutState.offset = o
            mutState.error  = null
            ()
          }
        }
        go()
      }
    }

  def many1[A](pa: Parser[A]): Parser[NonEmptyList[A]] =
    new Parser[NonEmptyList[A]] {
      def mutParse(mutState: MutState): NonEmptyList[A] = {
        val head = pa.mutParse(mutState)
        if (mutState.isOk) {
          val tail = List.newBuilder[A]
          @tailrec def go: NonEmptyList[A] = {
            val o = mutState.offset
            val a = pa.mutParse(mutState)
            if (mutState.isOk) {
              tail += a
              go
            } else {
              mutState.offset = o
              mutState.error  = null
              NonEmptyList(head, tail.result)
            }
          }
          go
        } else dummy
      }
    }

  def skipMany1[A](pa: Parser[A]): Parser[Unit] =
    new Parser[Unit] {
      def mutParse(mutState: MutState): Unit = {
        pa.mutParse(mutState)
        if (mutState.isOk) {
          @tailrec def go(): Unit = {
            val o = mutState.offset
            pa.mutParse(mutState)
            if (mutState.isOk) {
              go()
            } else {
              mutState.offset = o
              mutState.error  = null
              ()
            }
          }
          go()
        } else dummy
      }
    }

}
