// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package builder

/** Applicative builder at arity 22. */
final class ApBuilder22[+A,+B,+C,+D,+E,+F,+G,+H,+I,+J,+K,+L,+M,+N,+O,+P,+Q,+R,+S,+T,+U,+V](
  pa: Parser[A], pb: => Parser[B], pc: => Parser[C], pd: => Parser[D], pe: => Parser[E], pf: => Parser[F], pg: => Parser[G], ph: => Parser[H], pi: => Parser[I], pj: => Parser[J], pk: => Parser[K], pl: => Parser[L], pm: => Parser[M], pn: => Parser[N], po: => Parser[O], pp: => Parser[P], pq: => Parser[Q], pr: => Parser[R], ps: => Parser[S], pt: => Parser[T], pu: => Parser[U], pv: => Parser[V]
) { outer =>

        
  /**
   * Eliminate this ApBuilder22, yielding a parser that executes the `.void` versions of
   * all 22 component parsers in sequence.
   * @group eliminators
   */
  def void: Parser[Unit] =
    new ApBuilder22(pa.void, pb.void, pc.void, pd.void, pe.void, pf.void, pg.void, ph.void, pi.void, pj.void, pk.void, pl.void, pm.void, pn.void, po.void, pp.void, pq.void, pr.void, ps.void, pt.void, pu.void, pv.void).mapN((_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) => ())

  /**
   * Eliminate this ApBuilder22, yielding a parser that executes the `.void` versions of
   * all 22 component parsers in sequence and returns the consumed input text. This
   * is equivalent to `.void.inputText`.
   * @group eliminators
   */
  def inputText: Parser[String] =
    void.inputText

  /**
   * Eliminate this ApBuilder22, yielding a parser that returns a Tuple22 of
   * results from all 22 component parsers.
   * @group eliminators
   */
  def tupled: Parser[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)] =
    mapN(Tuple22.apply)

  /**
   * Eliminate this ApBuilder22 by providing a function that combines results from
   * all 22 component parsers, yielding a parser.
   * @group eliminators
   */
  def mapN[W](ap: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => W): Parser[W] =
    new Parser[W] {
      lazy val pbʹ = pb
      lazy val pcʹ = pc
      lazy val pdʹ = pd
      lazy val peʹ = pe
      lazy val pfʹ = pf
      lazy val pgʹ = pg
      lazy val phʹ = ph
      lazy val piʹ = pi
      lazy val pjʹ = pj
      lazy val pkʹ = pk
      lazy val plʹ = pl
      lazy val pmʹ = pm
      lazy val pnʹ = pn
      lazy val poʹ = po
      lazy val ppʹ = pp
      lazy val pqʹ = pq
      lazy val prʹ = pr
      lazy val psʹ = ps
      lazy val ptʹ = pt
      lazy val puʹ = pu
      lazy val pvʹ = pv
      override def void: Parser[Unit] = outer.void
      def mutParse(mutState: MutState): W = {
        val a = pa .mutParse(mutState); if (mutState.isError) return dummy
        val b = pbʹ.mutParse(mutState); if (mutState.isError) return dummy
        val c = pcʹ.mutParse(mutState); if (mutState.isError) return dummy
        val d = pdʹ.mutParse(mutState); if (mutState.isError) return dummy
        val e = peʹ.mutParse(mutState); if (mutState.isError) return dummy
        val f = pfʹ.mutParse(mutState); if (mutState.isError) return dummy
        val g = pgʹ.mutParse(mutState); if (mutState.isError) return dummy
        val h = phʹ.mutParse(mutState); if (mutState.isError) return dummy
        val i = piʹ.mutParse(mutState); if (mutState.isError) return dummy
        val j = pjʹ.mutParse(mutState); if (mutState.isError) return dummy
        val k = pkʹ.mutParse(mutState); if (mutState.isError) return dummy
        val l = plʹ.mutParse(mutState); if (mutState.isError) return dummy
        val m = pmʹ.mutParse(mutState); if (mutState.isError) return dummy
        val n = pnʹ.mutParse(mutState); if (mutState.isError) return dummy
        val o = poʹ.mutParse(mutState); if (mutState.isError) return dummy
        val p = ppʹ.mutParse(mutState); if (mutState.isError) return dummy
        val q = pqʹ.mutParse(mutState); if (mutState.isError) return dummy
        val r = prʹ.mutParse(mutState); if (mutState.isError) return dummy
        val s = psʹ.mutParse(mutState); if (mutState.isError) return dummy
        val t = ptʹ.mutParse(mutState); if (mutState.isError) return dummy
        val u = puʹ.mutParse(mutState); if (mutState.isError) return dummy
        val v = pvʹ.mutParse(mutState); if (mutState.isError) return dummy
        ap(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
      }
  }

}
