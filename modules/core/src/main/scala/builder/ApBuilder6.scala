// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

/** Applicative builder at arity 6. */
final class ApBuilder6[+A,+B,+C,+D,+E,+F](
  pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D], pe: => Parser[E], pf: => Parser[F]
) { outer =>

  /**
   * Append another parser to this ApBuilder.
   * @group sequencing
   */
  def ~[G](pg: => Parser[G]): ApBuilder7[A,B,C,D,E,F,G] =
    new ApBuilder7[A,B,C,D,E,F,G](pa, pb, pc, pd, pe, pf, pg)

  /**
   * Eliminate this ApBuilder6, yielding a parser that executes the `.void` versions of
   * all 6 component parsers in sequence.
   * @group eliminators
   */
  def void: Parser[Unit] =
    new ApBuilder6(pa.void, pb.void, pc.void, pd.void, pe.void, pf.void).mapN((_,_,_,_,_,_) => ())

  /**
   * Eliminate this ApBuilder6, yielding a parser that executes the `.void` versions of
   * all 6 component parsers in sequence and returns the consumed input text. This
   * is equivalent to `.void.inputText`.
   * @group eliminators
   */
  def inputText: Parser[String] =
    void.inputText

  /**
   * Eliminate this ApBuilder6, yielding a parser that returns a Tuple6 of
   * results from all 6 component parsers.
   * @group eliminators
   */
  def tupled: Parser[(A,B,C,D,E,F)] =
    mapN(Tuple6.apply)

  /**
   * Eliminate this ApBuilder6 by providing a function that combines results from
   * all 6 component parsers, yielding a parser.
   * @group eliminators
   */
  def mapN[G](ap: (A,B,C,D,E,F) => G): Parser[G] =
    new Parser[G] {
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      lazy val peʹ = pe
      lazy val pfʹ = pf
      override def void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): G = {
        val a = pa .mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        val d = pdʹ.mutParse(mutState); if (mutState.isError) return dummy
        val e = peʹ.mutParse(mutState); if (mutState.isError) return dummy
        val f = pfʹ.mutParse(mutState); if (mutState.isError) return dummy
        ap(a, b, c, d, e, f)
      }
  }

}
