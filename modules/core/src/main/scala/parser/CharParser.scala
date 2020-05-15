// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

import java.util.Arrays

object CharParser {

  object Constructors extends Constructors
  trait Constructors {

    /** @group char */
    def accept(p: Char => Boolean): Parser[Char] =
      new Parser[Char]("accept(...)") {
        val predicateError = s"$this: predicate failed"
        val eofError = s"$this: end of input"
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

    /** @group char */
    val digit      = accept(_.isDigit).named("digit")
    /** @group char */
    val letter     = accept(_.isLetter).named("letter")
    /** @group char */
    val whitespace = accept(_.isWhitespace).named("whitespace")

    /** @group char */
    def char(c: Char): Parser[Char] =
      accept(_ == c).named(s"char($c)")

    /** @group char */
    def anyChar: Parser[Char] =
      new Parser[Char]("anyChar") {
        val delegate = Constructors.accept(_ => true).named("anyChar")
        override def peek: Parser[Char] = delegate.peek
        override lazy val void: Parser[Unit] = BaseParser.Constructors.skip(1)
        def mutParse(mutState: MutState): Char = delegate.mutParse(mutState)
      }

    // Seq lets us pass a string or sequence
    /** @group char */
    def charIn(cs: Seq[Char]): Parser[Char] = {
      // we can do a binary search
      val csʹ = cs.distinct.sorted[Char].toArray[Char]
      val name = s"charIn(${csʹ.mkString})"
      accept(Arrays.binarySearch(csʹ, _) >= 0).named(name)
    }

  }

}