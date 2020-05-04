// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

trait BaseParsers {

  val unit: Parser[Unit] =
    const(()).named("unit")

  def const[A](a: A): Parser[A] =
    new Parser[A](s"const($a)") {
     override lazy val void: Parser[Unit] = unit
     override def peek: Parser[A] = this
     override def consumed: Parser[Int] = const(0)
     def mutParse(mutState: MutState): A = a
    }

  def fail[A](message: String): Parser[A] =
    new Parser[A]("fail(...)") {
      override lazy val void: Parser[Unit] = fail(message)
      final override def peek: Parser[A] = this
      def mutParse(mutState: MutState): A = {
        mutState.setError(message)
        dummy
      }
    }

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
