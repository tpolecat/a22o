// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

import java.text.ParseException
import a22o.parser._

/** Module of `Parser` constructors. */
object Parser
  extends BaseParser.Constructors
     with CharParser.Constructors
     with IntParser.Constructors
     with MetaParser.Constructors
     with TextParser.Constructors {

  /**
   * A `Char` can be used as a parser that parses itself.
   * @group char
   */
  implicit def charToParser(c: Char): Parser[Char] =
    char(c)

  /**
   * A `String` can be used as a parser that parses itself.
   * @group text
   */
  implicit def stringToParser(s: String): Parser[String] =
    string(s)

}

abstract class Parser[@specialized +A] private[a22o] (val name: String = "<parser>")
  extends BaseParser.Combinators[A]
     with MetaParser.Combinators[A]
     with TextParser.Combinators[A] { outer =>

  override def toString = name

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
    (this <~ Parser.endOfInput).parse(input)
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
    (this <~ Parser.endOfInput).unsafeParse(input)


  /**
   * This parser, as a `PartialFunction`.
   */
  lazy val toPartialFunction: PartialFunction[String, A] =
    new PartialFunction[String, A] {

      def apply(v1: String): A =
        applyOrElse[String, A](v1, _ => throw new MatchError(v1))

      def isDefinedAt(x: String): Boolean =
        as(true).onError(false).unsafeParse(x)

      override def applyOrElse[A1 <: String, B1 >: A](x: A1, default: A1 => B1): B1 =
        onError(default(x)).unsafeParse(x)

    }


  // grim
  protected[this] final val dummy: A =
    null.asInstanceOf[A]

  // the contract is: on entry isError is false, offset is correct
  // on exit, offset is advanced on success, untouched on failure
  protected[a22o] def mutParse(mutState: MutState): A

  /**
   * True if this parser is definitely equivalent to `other` (see `equivalentParser`) *and* has the
   * same name and failure message; i.e., this parser and `other` can be swapped without any
   * observable effect. This is intended for testing.
   */
  final override def equals(any: Any): Boolean =
    any match {
      case other: Parser[_] => equivalentTo(other) && (this.name == other.name)
      case _ => false
    }

  /**
   * True if this parser definitely has the same behavior as `other` up to naming; i.e., this parser
   * and `other` have the same success/failure behavior, disregarding the parser's name and failure
   * message. All instances of `fail(s)` are equivalent, for example. This is undecidable in general,
   * but in some cases it's useful to test static reductions like `const(a).void -> unit`. For const
   * parsers this assumes sane universal equality. This is intended for testing only.
   */
  def equivalentTo[B >: A](other: Parser[B]): Boolean =
    this eq other

}

