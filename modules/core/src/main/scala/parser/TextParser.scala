// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

import java.util.Arrays

object TextParser {
  import base._, char._

  trait Constructors {

    def charsInPredicate(cs: Seq[Char]): Char => Boolean =
       cs.length match {
        case 0 =>
          ??? // TODO
        case 1 =>
          val c0 = cs(0)
          c => c == c0
        case 2 =>
          val c0 = cs(0)
          val c1 = cs(1)
          c => c == c0 || c == c1
        case 3 =>
          val c0 = cs(0)
          val c1 = cs(1)
          val c2 = cs(2)
          c => c == c0 || c == c1 || c == c2
        case _ =>
          val csʹ = cs.sorted[Char].toArray[Char]

          val x = csʹ.mkString
          c => x.indexOf(c.toInt) >= 0

          // c => Arrays.binarySearch(csʹ, c) >= 0
      }

    /** @group text */
    def take(n: Int): Parser[String] =
      new Parser[String](s"take($n)") {
        @inline override def void = skip(n)
        @inline override def mutParse(mutState: MutState): String =
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

    /**
     * @group text
     */
    def string(s: String): Parser[String] =
      new Parser[String](s"string($s)") {
        override lazy val void = base.skip(s.length)
        def mutParse(mutState: MutState): String =
          if (mutState.startsWith(s)) {
            mutState.advance(s.length)
            s
          } else {
            mutState.setError("string: no match")
            dummy
          }
      }


    /**
     * @group text
     */
    def stringOf(cs: Seq[Char]): Parser[String] =
      takeWhile(charsInPredicate(cs))

    /**
     * @group text
     */
    def stringOf1(cs: Seq[Char]): Parser[String] = {
      val csʹ = cs.sorted[Char].toArray[Char]
      takeWhile1(c => Arrays.binarySearch(csʹ, c) >= 0)
    }

    /**
     * @group text
     */
    def takeWhile(p: Char => Boolean): Parser[String] =
      new Parser[String] {

        override lazy val void: Parser[Unit] =
          new Parser[Unit] {
            override lazy val void = this
            def mutParse(mutState: MutState): Unit = {
              val rem = mutState.remaining
              var i = 0
              while (i < rem && p(mutState.charAt(i)))
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

    /**
     * @group text
     */
    def takeWhile1(p: Char => Boolean): Parser[String] =
      new Parser[String] {

        override lazy val void: Parser[Unit] =
          new Parser[Unit] {
            override lazy val void = this
            def mutParse(mutState: MutState): Unit = {
              val rem = mutState.remaining
              var i = 0
              while (i < rem && p(mutState.charAt(i)))
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
          val rem = mutState.remaining
          var i = 0
          while (i < rem && p(mutState.charAt(i)))
            i += 1
          if (i == 0) {
            mutState.setError("stringOf1: no match")
            dummy
          } else {
            mutState.consume(i)
          }
        }

      }

  }

  trait Combinators[+A] { self: Parser[A] =>

    /**
     * @group text
     */
    def token: Parser[A] =
      this <~ whitespace.many.void

    /**
     * @group text
     */
    def bracketed(open: Char, close: Char): Parser[A] =
      char(open) ~> this.token <~ char(close)

    /**
     * @group text
     */
    def parens:   Parser[A] = bracketed('(', ')')

    /**
     * @group text
     */
    def braces:   Parser[A] = bracketed('{', '}')

    /**
     * @group text
     */
    def brackets: Parser[A] = bracketed('[', ']')

    /**
     * @group sequencing
     */
    def +(p2: => Parser[String])(
      implicit ev: A <:< String
    ): Parser[String] =
      new Parser[String] {
        lazy val p1ʹ = self.void
        lazy val p2ʹ = p2.void
        override lazy val void: Parser[Unit] = p1ʹ ~> p2ʹ
        def mutParse(mutState: MutState): String = {
          val pos0 = mutState.getPoint
          p1ʹ.mutParse(mutState)
          if (mutState.isOk) {
            p2ʹ.mutParse(mutState)
            if (mutState.isOk) {
              val pos1 = mutState.getPoint
              mutState.setPoint(pos0)
              mutState.consume(pos1 - pos0) // only do the substring operation once
            } else {
              dummy
            }
          } else {
            dummy
          }
        }
      }

  }

}

