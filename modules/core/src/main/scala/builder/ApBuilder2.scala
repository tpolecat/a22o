// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

/** Applicative builder at arity 2. */
final class ApBuilder2[+A,+B](
  pa: Parser[A], pb: => Parser[B]
) { outer =>

  /**
   * Append another parser to this ApBuilder.
   * @group sequencing
   */
  def ~[C](pc: => Parser[C]): ApBuilder3[A,B,C] =
    new ApBuilder3[A,B,C](pa, pb, pc)

  /**
   * Eliminate this ApBuilder2, yielding a parser that executes the `.void` versions of
   * both component parsers in sequence.
   * @group eliminators
   */
  def void: Parser[Unit] =
    new ApBuilder2(pa.void, pb.void).mapN((_,_) => ())

  /**
   * Eliminate this ApBuilder2, yielding a parser that executes the `.void` versions of
   * both component parsers in sequence and returns the consumed input text. This
   * is equivalent to `.void.inputText`.
   * @group eliminators
   */
  def inputText: Parser[String] =
    void.inputText

  /**
   * Eliminate this ApBuilder2, yielding a parser that returns a Tuple2 of
   * results from both component parsers.
   * @group eliminators
   */
  def tupled: Parser[(A,B)] =
    mapN(Tuple2.apply)

  /**
   * Eliminate this ApBuilder2 by providing a function that combines results from
   * both component parsers, yielding a parser.
   * @group eliminators
   */
  def mapN[C](ap: (A,B) => C): Parser[C] =
    new Parser[C] {
      lazy val pbʹ = pb
      override def void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): C = {
        val a = pa .mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        ap(a, b)
      }
  }

}
