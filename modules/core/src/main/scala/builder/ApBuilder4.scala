// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

/** Applicative builder at arity 4. */
final class ApBuilder4[+A,+B,+C,+D](
  pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D]
) { outer =>

  /**
   * Append another parser to this ApBuilder.
   * @group sequencing
   */
  def ~[E](pe: => Parser[E]): ApBuilder5[A,B,C,D,E] =
    new ApBuilder5[A,B,C,D,E](pa, pb, pc, pd, pe)

  /**
   * Eliminate this ApBuilder4, yielding a parser that executes the `.void` versions of
   * all 4 component parsers in sequence.
   * @group eliminators
   */
  def void: Parser[Unit] =
    new ApBuilder4(pa.void, pb.void, pc.void, pd.void).mapN((_,_,_,_) => ())

  /**
   * Eliminate this ApBuilder4, yielding a parser that executes the `.void` versions of
   * all 4 component parsers in sequence and returns the consumed input text. This
   * is equivalent to `.void.inputText`.
   * @group eliminators
   */
  def inputText: Parser[String] =
    void.inputText

  /**
   * Eliminate this ApBuilder4, yielding a parser that returns a Tuple4 of
   * results from all 4 component parsers.
   * @group eliminators
   */
  def tupled: Parser[(A,B,C,D)] =
    mapN(Tuple4.apply)

  /**
   * Eliminate this ApBuilder4 by providing a function that combines results from
   * all 4 component parsers, yielding a parser.
   * @group eliminators
   */
  def mapN[E](ap: (A,B,C,D) => E): Parser[E] =
    new Parser[E] {
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      override def void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): E = {
        val a = pa .mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        val d = pdʹ.mutParse(mutState); if (mutState.isError) return dummy
        ap(a, b, c, d)
      }
  }

}
