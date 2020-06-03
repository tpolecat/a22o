// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

/** Applicative builder at arity 5. */
final class ApBuilder5[+A,+B,+C,+D,+E](
  pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D], pe: => Parser[E]
) { outer =>

  /**
   * Append another parser to this ApBuilder.
   * @group sequencing
   */
  def ~[F](pf: => Parser[F]): ApBuilder6[A,B,C,D,E,F] =
    new ApBuilder6[A,B,C,D,E,F](pa, pb, pc, pd, pe, pf)

  /**
   * Eliminate this ApBuilder5, yielding a parser that executes the `.void` versions of
   * all 5 component parsers in sequence.
   * @group eliminators
   */
  def void: Parser[Unit] =
    new ApBuilder5(pa.void, pb.void, pc.void, pd.void, pe.void).mapN((_,_,_,_,_) => ())

  /**
   * Eliminate this ApBuilder5, yielding a parser that executes the `.void` versions of
   * all 5 component parsers in sequence and returns the consumed input text. This
   * is equivalent to `.void.inputText`.
   * @group eliminators
   */
  def inputText: Parser[String] =
    void.inputText

  /**
   * Eliminate this ApBuilder5, yielding a parser that returns a Tuple5 of
   * results from all 5 component parsers.
   * @group eliminators
   */
  def tupled: Parser[(A,B,C,D,E)] =
    mapN(Tuple5.apply)

  /**
   * Eliminate this ApBuilder5 by providing a function that combines results from
   * all 5 component parsers, yielding a parser.
   * @group eliminators
   */
  def mapN[F](ap: (A,B,C,D,E) => F): Parser[F] =
    new Parser[F] {
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      lazy val peʹ = pe
      override def void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): F = {
        val a = pa .mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        val d = pdʹ.mutParse(mutState); if (mutState.isError) return dummy
        val e = peʹ.mutParse(mutState); if (mutState.isError) return dummy
        ap(a, b, c, d, e)
      }
  }

}
