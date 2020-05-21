// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

package arb

import org.scalacheck.Gen

/** Signature for modules that export a collection of TParsers. */
trait TParsers {
  def base: List[Gen[TParser]]
  def combinator(maxDepth: Int): List[Gen[TParser]]
}
