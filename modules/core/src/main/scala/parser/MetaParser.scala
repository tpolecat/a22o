// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

object MetaParser {
  import BaseParser.Constructors.unit

  object Constructors extends Constructors
  trait Constructors {

    /** @group meta */
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

    /** @group meta */
    def remaining: Parser[String] =
      new Parser[String]("remaining") {
        override lazy val void: Parser[Unit] = unit
        override def peek: Parser[String] = this
        def mutParse(mutState: MutState): String = mutState.remainingInput
      }

    /** @group meta */
    def available: Parser[Int] =
      new Parser[Int]("available") {
        override lazy val void: Parser[Unit] = unit
        override def peek: Parser[Int] = this
        def mutParse(mutState: MutState): Int = mutState.remaining
      }

    /** @group meta */
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

  trait Combinators[+A] { outer: Parser[A] =>

    /**
     * An equivalent parser that discards its result and instead yields the input that was consumed.
     * @group meta
     */
    def inputText: Parser[String] =
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
     * An equivalent parser that consumes no input.
     * @group meta
     */
    def peek: Parser[A] =
      new Parser[A](s"$outer.peek") {
        override lazy val peek: Parser[A] = this
        override lazy val void = outer.void.peek
        def mutParse(mutState: MutState): A = {
          val o = mutState.getPoint
          val a = outer.mutParse(mutState)
          mutState.setPoint(o)
          a
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

  }

}
