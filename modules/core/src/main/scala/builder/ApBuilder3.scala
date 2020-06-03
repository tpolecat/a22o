// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

/** Applicative builder at arity 3. */
final class ApBuilder3[+A,+B,+C](
  pa: Parser[A], pb: => Parser[B], pc: => Parser[C]
) { outer =>

  /**
   * Append another parser to this ApBuilder.
   * @group sequencing
   */
  def ~[D](pd: => Parser[D]): ApBuilder4[A,B,C,D] =
    new ApBuilder4[A,B,C,D](pa, pb, pc, pd)

  /**
   * Eliminate this ApBuilder3, yielding a parser that executes the `.void` versions of
   * all 3 component parsers in sequence.
   * @group eliminators
   */
  def void: Parser[Unit] =
    new ApBuilder3(pa.void, pb.void, pc.void).mapN((_,_,_) => ())

  /**
   * Eliminate this ApBuilder3, yielding a parser that executes the `.void` versions of
   * all 3 component parsers in sequence and returns the consumed input text. This
   * is equivalent to `.void.inputText`.
   * @group eliminators
   */
  def inputText: Parser[String] =
    void.inputText

  /**
   * Eliminate this ApBuilder3, yielding a parser that returns a Tuple3 of
   * results from all 3 component parsers.
   * @group eliminators
   */
  def tupled: Parser[(A,B,C)] =
    mapN(Tuple3.apply)

  /**
   * Eliminate this ApBuilder3 by providing a function that combines results from
   * all 3 component parsers, yielding a parser.
   * @group eliminators
   */
  def mapN[D](ap: (A,B,C) => D): Parser[D] =
    new Parser[D] {
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      override def void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): D = {
        val a = pa .mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        ap(a, b, c)
      }
  }

}
