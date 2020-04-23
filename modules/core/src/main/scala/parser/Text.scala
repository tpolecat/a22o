// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

trait Text {
  import combinator._

  val anyChar: Parser[Char] =
    accept(_ => true, "any character")

  def accept(p: Char => Boolean, name: String): Parser[Char] =
    new Parser[Char] {
      val predicateError = s"expected $name"
      val eofError = s"expected $name, found end of input"
      def mutParse(mutState: MutState): Char = {
        if (mutState.remaining >= 1) {
          val c = mutState.input(mutState.offset)
          if (p(c)) {
            mutState.offset += 1
            c
          }
          else {
            mutState.error = predicateError
            dummy
          }
        } else {
          mutState.error = eofError
          dummy
        }
      }
    }

  val digit      = accept(_.isDigit, "digit")
  val letter     = accept(_.isLetter, "letter")
  val whitespace = accept(_.isWhitespace, "whitespace")

  def char(c: Char): Parser[Char] =
    accept(_ == c, s"character '$c'")

  def token[A](pa: Parser[A]): Parser[A] =
    discardRight(pa, skipMany(whitespace))

  def bracketed[A](open: Char, pa: Parser[A], close: Char): Parser[A] =
    char(open).token ~> pa.token <~ char(close)

  def parens[A](pa: Parser[A]): Parser[A] =
    bracketed('(', pa, ')')

  def take(n: Int): Parser[String] =
    new Parser[String] {
      def mutParse(mutState: MutState): String =
        if (n < 0) {
          mutState.error = "take: negative length"
          dummy
        } else if (mutState.remaining >= n) {
          val s = mutState.input.substring(mutState.offset, mutState.offset + n)
          mutState.offset += n
          s
        } else {
          mutState.error = "take: insufficient input"
          dummy
        }
    }

  def string(s: String): Parser[String] =
    new Parser[String] {
      def mutParse(mutState: MutState): String =
        if (mutState.input.startsWith(s, mutState.offset)) {
          mutState.offset += s.length
          s
        } else {
          mutState.error = "string: no match"
          dummy
        }
    }

  /** Consumes characters while `p` holds true. */
  def stringOf(p: Char => Boolean): Parser[String] =
    new Parser[String] {
      def mutParse(mutState: MutState): String = {
        var i = mutState.offset
        while (i < mutState.input.length && p(mutState.input(i)))
          i += 1
        val s = mutState.input.substring(mutState.offset, i)
        mutState.offset = i
        s
      }
    }

  def stringOf1(p: Char => Boolean): Parser[String] =
    new Parser[String] {
      def mutParse(mutState: MutState): String = {
        var i = mutState.offset
        while (i < mutState.input.length && p(mutState.input(i)))
          i += 1
        if (i == mutState.offset) {
          mutState.error = "stringOf1: no match"
          dummy
        } else {
          val s = mutState.input.substring(mutState.offset, i)
          mutState.offset = i
          s
        }
      }
    }

}