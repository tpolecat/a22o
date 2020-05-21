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

  def matching = generators.left
  def failing  = generators.right
  def input    = generators.fold(identity, identity, Gen.oneOf(_, _))
  def canMatch = generators.isLeft  || generators.isBoth
  def canFail  = generators.isRight || generators.isBoth

  def inputWithExpectation =
    generators.fold(
      s => s.map((_, true)),
      f => f.map((_, false)),
      (s, f) => Gen.oneOf(s.map((_, true)), f.map((_, false)))
    )

}
