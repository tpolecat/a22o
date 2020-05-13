package a22o

package arb

import org.scalacheck.Gen

/** Signature for modules that export a collection of TParsers. */
trait TParsers {
  def base: List[Gen[TParser]]
  def combinator(maxDepth: Int): List[Gen[TParser]]
}
