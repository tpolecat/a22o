// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

import a22o.parser.meta._
import java.text.ParseException
import a22o.parser._

object Parser
  extends BaseParser.Constructors
     with CharParser.Constructors
     with IntParser.Constructors
     with MetaParser.Constructors
     with TextParser.Constructors

abstract class Parser[@specialized +A] private[a22o] (override val toString: String = "<parser>")
  extends BaseParser.Combinators[A]
     with MetaParser.Combinators[A]
     with TextParser.Combinators[A] { outer =>

  /**
   * Attempt to parse the given input, returning a result containing the unconsumed remainder and
   * either an error or the computed result.
   * @group eliminators
   */
  final def parse(input: String): Result[A] = {
    val s = ParseState(input)
    val a = mutParse(s)
    s.result(a)
  }


  /**
   * Attempt to parse the given input completely, returning either an error or the computed result.
   * @group eliminators
   */
  final def parseAll(input: String): Result[A] = {
    (this <~ endOfInput).parse(input)
  }

  /**
   * Attempt to parse the given input, returning the unconsumed remainder and the computed result,
   * or raising a `ParseException` on failure.
   * @group eliminators
   */
  final def unsafeParse(input: String): A =
    parse(input).fold(
      e => throw new ParseException(e, 0), // TODO: position!
      identity
    )

  /**
   * Attempt to parse the given input completely, returning the computed result, or raising a
   * `ParseException` ib failure.
   * @group eliminators
   */
  final def unsafeParseAll(input: String): A =
    (this <~ endOfInput).unsafeParse(input)

  // grim
  protected[this] final val dummy: A =
    null.asInstanceOf[A]

  // the contract is: on entry isError is false, offset is correct
  // on exit, offset is advanced on success, untouched on failure
  protected[a22o] def mutParse(mutState: MutState): A

}

