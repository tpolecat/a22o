// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

import java.util.Arrays

object TextParser {
  import char._

  trait Constructors {

    /**
     * @group text
     */
    def string(s: String): Parser[String] =
      new Parser[String] {
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
    def stringOf(cs: Seq[Char]): Parser[String] = {
      val csʹ = cs.sorted[Char].toArray[Char]
      takeWhile(c => Arrays.binarySearch(csʹ, c) >= 0)
    }

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

    /**
     * @group text
     */
    def takeWhile1(p: Char => Boolean): Parser[String] =
      new Parser[String] {

        override lazy val void: Parser[Unit] =
          new Parser[Unit] {
            override lazy val void = this
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



// trait TextParsers { module =>
//   import base._
//   import char._

//   def token[A](pa: Parser[A]): Parser[A] =
//     pa <~ whitespace.many.void

//   def bracketed[A](open: Char, pa: Parser[A], close: Char): Parser[A] =
//     token(char(open)) ~> token(pa) <~ char(close)

//   def parens[A](pa: Parser[A]): Parser[A] =
//     bracketed('(', pa, ')')

//   def braces[A](pa: Parser[A]): Parser[A] =
//     bracketed('{', pa, '}')

//   def brackets[A](pa: Parser[A]): Parser[A] =
//     bracketed('[', pa, ']')

//   def string(s: String): Parser[String] =
//     new Parser[String] {
//       override lazy val void = skip(s.length)
//       def mutParse(mutState: MutState): String =
//         if (mutState.startsWith(s)) {
//           mutState.advance(s.length)
//           s
//         } else {
//           mutState.setError("string: no match")
//           dummy
//         }
//     }


//   def stringOf(cs: Seq[Char]): Parser[String] = {
//     val csʹ = cs.sorted[Char].toArray[Char]
//     takeWhile(c => Arrays.binarySearch(csʹ, c) >= 0)
//   }

//   def stringOf1(cs: Seq[Char]): Parser[String] = {
//     val csʹ = cs.sorted[Char].toArray[Char]
//     takeWhile1(c => Arrays.binarySearch(csʹ, c) >= 0)
//   }

//   /** Consumes characters while `p` holds true. */
//   def takeWhile(p: Char => Boolean): Parser[String] =
//     new Parser[String] {

//       override lazy val void: Parser[Unit] =
//         new Parser[Unit] {
//           override lazy val void = this
//           def mutParse(mutState: MutState): Unit = {
//             var i = 0
//             while (i < mutState.remaining && p(mutState.charAt(i)))
//               i += 1
//             mutState.advance(i)
//             ()
//           }
//         }

//       def mutParse(mutState: MutState): String = {
//         var i = 0
//         while (i < mutState.remaining && p(mutState.charAt(i)))
//           i += 1
//         mutState.consume(i)
//      }

//     }

//   def takeWhile1(p: Char => Boolean): Parser[String] =
//     new Parser[String] {

//       override lazy val void: Parser[Unit] =
//         new Parser[Unit] {
//           override lazy val void = this
//           def mutParse(mutState: MutState): Unit = {
//             var i = 0
//             while (i < mutState.remaining && p(mutState.charAt(i)))
//               i += 1
//             if (i == 0) {
//               mutState.setError("stringOf1: no match")
//               dummy
//             } else {
//               mutState.advance(i)
//               ()
//             }
//           }
//         }

//       def mutParse(mutState: MutState): String = {
//         var i = 0
//         while (i < mutState.remaining && p(mutState.charAt(i)))
//           i += 1
//         if (i == 9) {
//           mutState.setError("stringOf1: no match")
//           dummy
//         } else {
//           mutState.consume(i)
//         }
//       }

//     }

//   /** Optimization of `(p1 ~ p2).map(_ + _)` that performs only one String allocation. */
//   def concat(p1: Parser[String], p2: => Parser[String]): Parser[String] =
//     new Parser[String] {
//       lazy val p1ʹ = p1.void
//       lazy val p2ʹ = p2.void
//       override lazy val void: Parser[Unit] = p1ʹ ~> p2ʹ
//       def mutParse(mutState: MutState): String = {
//         val p0 = mutState.getPoint
//         p1ʹ.mutParse(mutState)
//         if (mutState.isOk) {
//           p2ʹ.mutParse(mutState)
//           if (mutState.isOk) {
//             val p1 = mutState.getPoint
//             mutState.setPoint(p0)
//             mutState.consume(p1 - p0) // only do the substring operation once
//           } else {
//             dummy
//           }
//         } else {
//           dummy
//         }
//       }
//     }

// }


// class TextCombinators[+A] { this: Parser[A] =>

//   /** @group text */
//   def token: Parser[A] = text.token(this)

//   /** @group text */
//   def bracketed(open: Char, close: Char): Parser[A] = text.bracketed(open, this, close)

//   /** @group text */
//   def parens: Parser[A] = text.parens(this)

//   /** @group text */
//   def braces: Parser[A] = text.braces(this)

//   /** @group text */
//   def brackets: Parser[A] = text.brackets(this)

// }
