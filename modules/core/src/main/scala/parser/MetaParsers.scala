// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

trait MetaParsers {
  import base._

  val endOfInput: Parser[Unit] =
    new Parser[Unit]("endOfInput") {
      override lazy val void: Parser[Unit] = this
      def mutParse(mutState: MutState): Unit =
        if (mutState.remaining == 0) ()
        else {
          mutState.setError("expected end of input")
          dummy
        }
    }

  def remaining: Parser[String] =
    new Parser[String]("remaining") {
      override lazy val void: Parser[Unit] = unit
      override def peek: Parser[String] = this
      def mutParse(mutState: MutState): String = mutState.remainingInput
    }

  def available: Parser[Int] =
    new Parser[Int]("available") {
      override lazy val void: Parser[Unit] = unit
      override def peek: Parser[Int] = this
      def mutParse(mutState: MutState): Int = mutState.remaining
    }

  def ensure(n: Int): Parser[Unit] =
   new Parser[Unit](s"ensure($n)") {
      override lazy val void: Parser[Unit] = this
      override def peek: Parser[Unit] = this
      def mutParse(mutState: MutState): Unit =
        if (mutState.remaining >= n) ()
        else {
          mutState.setError("ensure: insufficient input")
          dummy
        }
   }

}
