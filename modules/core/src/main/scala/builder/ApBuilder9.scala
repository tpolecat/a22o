// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

/** Applicative builder at arity 9. */
final class ApBuilder9[+A,+B,+C,+D,+E,+F,+G,+H,+I](
  pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D], pe: => Parser[E], pf: => Parser[F], pg: => Parser[G], ph: => Parser[H], pi: => Parser[I]
) { outer =>

  /**
   * Append another parser to this ApBuilder.
   * @group sequencing
   */
  def ~[J](pj: => Parser[J]): ApBuilder10[A,B,C,D,E,F,G,H,I,J] =
    new ApBuilder10[A,B,C,D,E,F,G,H,I,J](pa, pb, pc, pd, pe, pf, pg, ph, pi, pj)

  /**
   * Eliminate this ApBuilder9, yielding a parser that executes the `.void` versions of
   * all 9 component parsers in sequence.
   * @group eliminators
   */
  def void: Parser[Unit] =
    new ApBuilder9(pa.void, pb.void, pc.void, pd.void, pe.void, pf.void, pg.void, ph.void, pi.void).mapN((_,_,_,_,_,_,_,_,_) => ())

  /**
   * Eliminate this ApBuilder9, yielding a parser that executes the `.void` versions of
   * all 9 component parsers in sequence and returns the consumed input text. This
   * is equivalent to `.void.inputText`.
   * @group eliminators
   */
  def inputText: Parser[String] =
    void.inputText

  /**
   * Eliminate this ApBuilder9, yielding a parser that returns a Tuple9 of
   * results from all 9 component parsers.
   * @group eliminators
   */
  def tupled: Parser[(A,B,C,D,E,F,G,H,I)] =
    mapN(Tuple9.apply)

  /**
   * Eliminate this ApBuilder9 by providing a function that combines results from
   * all 9 component parsers, yielding a parser.
   * @group eliminators
   */
  def mapN[J](ap: (A,B,C,D,E,F,G,H,I) => J): Parser[J] =
    new Parser[J] {
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      lazy val peʹ = pe
      lazy val pfʹ = pf
      lazy val pgʹ = pg
      lazy val phʹ = ph
      lazy val piʹ = pi
      override def void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): J = {
        val a = pa .mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        val d = pdʹ.mutParse(mutState); if (mutState.isError) return dummy
        val e = peʹ.mutParse(mutState); if (mutState.isError) return dummy
        val f = pfʹ.mutParse(mutState); if (mutState.isError) return dummy
        val g = pgʹ.mutParse(mutState); if (mutState.isError) return dummy
        val h = phʹ.mutParse(mutState); if (mutState.isError) return dummy
        val i = piʹ.mutParse(mutState); if (mutState.isError) return dummy
        ap(a, b, c, d, e, f, g, h, i)
      }
  }

}
