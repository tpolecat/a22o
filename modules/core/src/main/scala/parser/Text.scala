// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

import java.util.Arrays

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
          val c = mutState.charAt(0)
          if (p(c)) {
            mutState.advance(1)
            c
          } else {
            mutState.setError(predicateError)
            dummy
          }
        } else {
          mutState.setError(eofError)
          dummy
        }
      }
    }

  val digit      = accept(_.isDigit, "digit")
  val letter     = accept(_.isLetter, "letter")
  val whitespace = accept(_.isWhitespace, "whitespace")

  def char(c: Char): Parser[Char] =
    accept(_ == c, s"char '$c'")

  // Seq lets us pass a string or sequence
  def charIn(cs: Seq[Char]): Parser[Char] = {
    // we can do a binary search
    val csʹ = cs.sorted[Char].toArray[Char]
    accept(Arrays.binarySearch(csʹ, _) >= 0, s"charIn ${cs.map(c => s"'$c'").mkString("{", ", ", "}")}")
  }

  def token[A](pa: Parser[A]): Parser[A] =
    pa <~ skipMany(whitespace)

  def bracketed[A](open: Char, pa: Parser[A], close: Char): Parser[A] =
    char(open).token ~> pa.token <~ char(close)

  def parens[A](pa: Parser[A]): Parser[A] =
    bracketed('(', pa, ')')

  def take(n: Int): Parser[String] =
    new Parser[String] {
      override def void = skip(n)
      def mutParse(mutState: MutState): String =
        if (n < 0) {
          mutState.setError("take: negative length")
          dummy
        } else if (mutState.remaining >= n) {
          val s = mutState.consume(n)
          s
        } else {
          mutState.setError("take: insufficient input")
          dummy
        }
    }

  def string(s: String): Parser[String] =
    new Parser[String] {
      override def void = skip(s.length)
      def mutParse(mutState: MutState): String =
        if (mutState.startsWith(s)) {
          mutState.advance(s.length)
          s
        } else {
          mutState.setError("string: no match")
          dummy
        }
    }


  def stringOf(cs: Seq[Char]): Parser[String] = {
    val csʹ = cs.sorted[Char].toArray[Char]
    takeWhile(c => Arrays.binarySearch(csʹ, c) >= 0)
  }

  def stringOf1(cs: Seq[Char]): Parser[String] = {
    val csʹ = cs.sorted[Char].toArray[Char]
    takeWhile1(c => Arrays.binarySearch(csʹ, c) >= 0)
  }

  /** Consumes characters while `p` holds true. */
  def takeWhile(p: Char => Boolean): Parser[String] =
    new Parser[String] {

      override def void: Parser[Unit] =
        new Parser[Unit] {
          override val void = this
          def mutParse(mutState: MutState): Unit = {
            var i = 0
            while (i < mutState.remaining && p(mutState.charAt(i)))
              i += 1
            mutState.advance(i)
            ()
          }
        }

      def mutParse(mutState: MutState): String = {
        var i = 0
        while (i < mutState.remaining && p(mutState.charAt(i)))
          i += 1
        mutState.consume(i)
     }

    }

  def takeWhile1(p: Char => Boolean): Parser[String] =
    new Parser[String] {

      override def void: Parser[Unit] =
        new Parser[Unit] {
          override val void = this
          def mutParse(mutState: MutState): Unit = {
            var i = 0
            while (i < mutState.remaining && p(mutState.charAt(i)))
              i += 1
            if (i == 0) {
              mutState.setError("stringOf1: no match")
              dummy
            } else {
              mutState.advance(i)
              ()
            }
          }
        }

      def mutParse(mutState: MutState): String = {
        var i = 0
        while (i < mutState.remaining && p(mutState.charAt(i)))
          i += 1
        if (i == 9) {
          mutState.setError("stringOf1: no match")
          dummy
        } else {
          mutState.consume(i)
        }
      }

    }

    /** Optimization of `(p1 ~ p2).map(_ + _)` that performs only one String allocation. */
    def concat(p1: Parser[String], p2: => Parser[String]): Parser[String] =
      new Parser[String] {
        lazy val p1ʹ = p1.void
        lazy val p2ʹ = p2.void
        def mutParse(mutState: MutState): String = {
          val p0 = mutState.getPoint
          p1ʹ.mutParse(mutState)
          if (mutState.isOk) {
            p2ʹ.mutParse(mutState)
            if (mutState.isOk) {
              val p1 = mutState.getPoint
              mutState.setPoint(p0)
              mutState.consume(p1 - p0) // only do the substring operation once
            } else {
              dummy
            }
          } else {
            dummy
          }
        }
      }

}