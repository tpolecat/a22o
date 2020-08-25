// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

package arb

import cats.data.Ior
import org.scalacheck.Gen

/**
 * A testable parser, which is a parser for some unknown data type, plus generators for valid
 * and/or invalid input.
 */
final case class TParser(
  parser: Parser[_],
  generators: Ior[Gen[String], Gen[String]]
) {

  /** Generator of matching that should succeed. */
  def matching: Option[Gen[String]] = generators.left

  /** Generator of inputs that should fail. */
  def failing:  Option[Gen[String]] = generators.right

  /** Generator of inputs (possibly failing). */
  def input: Gen[String] = generators.fold(identity, identity, Gen.oneOf(_, _))

  /** True if `input` generates any values that should succeed. */
  def canMatch: Boolean = generators.isLeft  || generators.isBoth

  /** True if `input` generates any values that should fail. */
  def canFail:  Boolean = generators.isRight || generators.isBoth

  /** Generator of an input paired with a boolean indicating whether parsing should succeed. */
  def inputWithExpectation: Gen[(String, Boolean)] =
    generators.fold(
      s => s.map((_, true)),
      f => f.map((_, false)),
      (s, f) => Gen.oneOf(s.map((_, true)), f.map((_, false)))
    )

}
