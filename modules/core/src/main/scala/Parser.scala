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

abstract class Parser[+A] private[a22o] (override val toString: String = "<parser>")
  extends BaseParser.Combinators[A]
     with MetaParser.Combinators[A]
     with TextParser.Combinators[A] { outer =>

  final def accept(input: String): Boolean = {
    val s = new MutState(input)
    mutParse(s)
    s.isOk
  }

  /**
   * Attempt to parse the given input, returning the unconsumed remainder and either an error
   * or the computed result.
   * @group eliminators
   */
  final def parse(input: String): (String, Either[String, A]) = {
    val s = new MutState(input)
    val a = mutParse(s)
    val r = s.remainingInput
    (r, Either.cond(s.isOk, a, s.getError))
  }

  /**
   * Attempt to parse the given input completely, returning either an error or the computed result.
   * @group eliminators
   */
  final def parseAll(input: String): Either[String, A] = {
    val s = new MutState(input)
    val a = (this <~ endOfInput) mutParse(s)
    Either.cond(s.isOk, a, s.getError)
  }

  /**
   * Attempt to parse the given input, returning the unconsumed remainder and the computed result,
   * or raising a `ParseException` on failure.
   * @group eliminators
   */
  final def unsafeParse(input: String): (String, A) = {
    val s = new MutState(input)
    val a = mutParse(s)
    if (s.isOk) (s.remainingInput, a)
    else throw new ParseException(s.getError, s.getPoint)
  }

  /**
   * Attempt to parse the given input completely, returning the computed result, or raising a
   * `ParseException` ib failure.
   * @group eliminators
   */
  final def unsafeParseAll(input: String): A = {
    val s = new MutState(input)
    val a = (this <~ endOfInput) mutParse(s)
    if (s.isOk) a
    else throw new ParseException(s.getError, s.getPoint)
  }

  /**
   * An equivalent parser with the given name (and optionally a given name for its `void`
   * equivalent).
   * @group meta
   */
  final def named(name: String, voidName: Option[String] = None): Parser[A] =
    new Parser[A](name) {
      override lazy val void: Parser[Unit] = voidName.fold(outer.void)(outer.void.named(_))
      override lazy val peek: Parser[A] = outer.peek
      def mutParse(mutState: MutState): A = outer.mutParse(mutState)
    }

  // grim
  protected final val dummy: A =
    null.asInstanceOf[A]

  // the contract is: on entry isError is false, offset is correct
  // on exit, offset is advanced on success, untouched on failure
  protected[a22o] def mutParse(mutState: MutState): A

}

