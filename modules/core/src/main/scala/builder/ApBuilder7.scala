// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

/** Applicative builder at arity 7. */
final class ApBuilder7[+A,+B,+C,+D,+E,+F,+G](
  pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D], pe: => Parser[E], pf: => Parser[F], pg: => Parser[G]
) { outer =>

  /**
   * Append another parser to this ApBuilder.
   * @group sequencing
   */
  def ~[H](ph: => Parser[H]): ApBuilder8[A,B,C,D,E,F,G,H] =
    new ApBuilder8[A,B,C,D,E,F,G,H](pa, pb, pc, pd, pe, pf, pg, ph)

  /**
   * Eliminate this ApBuilder7, yielding a parser that executes the `.void` versions of
   * all 7 component parsers in sequence.
   * @group eliminators
   */
  def void: Parser[Unit] =
    new ApBuilder7(pa.void, pb.void, pc.void, pd.void, pe.void, pf.void, pg.void).mapN((_,_,_,_,_,_,_) => ())

  /**
   * Eliminate this ApBuilder7, yielding a parser that executes the `.void` versions of
   * all 7 component parsers in sequence and returns the consumed input text. This
   * is equivalent to `.void.inputText`.
   * @group eliminators
   */
  def inputText: Parser[String] =
    void.inputText

  /**
   * Eliminate this ApBuilder7, yielding a parser that returns a Tuple7 of
   * results from all 7 component parsers.
   * @group eliminators
   */
  def tupled: Parser[(A,B,C,D,E,F,G)] =
    mapN(Tuple7.apply)

  /**
   * Eliminate this ApBuilder7 by providing a function that combines results from
   * all 7 component parsers, yielding a parser.
   * @group eliminators
   */
  def mapN[H](ap: (A,B,C,D,E,F,G) => H): Parser[H] =
    new Parser[H] {
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      lazy val peʹ = pe
      lazy val pfʹ = pf
      lazy val pgʹ = pg
      override def void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): H = {
        val a = pa .mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        val d = pdʹ.mutParse(mutState); if (mutState.isError) return dummy
        val e = peʹ.mutParse(mutState); if (mutState.isError) return dummy
        val f = pfʹ.mutParse(mutState); if (mutState.isError) return dummy
        val g = pgʹ.mutParse(mutState); if (mutState.isError) return dummy
        ap(a, b, c, d, e, f, g)
      }
  }

}
