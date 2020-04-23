// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

import scala.annotation.switch

trait Numeric {

  val int: Parser[Int] =
    int(10)

  // Hey this doesn't work reliably for non-decimal radices, why? private for now
  private def int(radix: Int): Parser[Int] =
    new Parser[Int] {
      def mutParse(mutState: MutState): Int = {

        // One strategy would be to just look for leading +/- and a string of digits and then
        // delegate to String#toInt so this is a super-optimization that may not end up being any
        // faster at all in real life, but it does avoid a String allocation because we don't need
        // to call `.substring`. Need to benchmark these approaches because the code below
        // is really error-prone.

        // Ok so we accumulate by counting down from zero, which lets us accommodate Int.MinValue
        // which would overflow if we counted up. This leads to more complication at the bottom but
        // I think it makes sense.
        var index = mutState.offset
        var accum = 0
        var signed = false // can we rely on untupling?

        // If the first char is '-' then it's negative. '+' is also allowed.
        val neg: Boolean =
          (index < mutState.input.length) && {
            (mutState.input(index) : @switch) match {
              case '+' => index += 1; signed = true; false
              case '-' => index += 1; signed = true; true
              case _   => false
            }
          }

        // There must be at least one digit
        if (index < mutState.input.length) {
          val c = mutState.input(index)
          val n = Character.digit(c, radix)
          if (n < 0) {
            mutState.error = if (signed) "Expected digit." else "Expected sign or digit."
            return dummy // !!!
          } else {
            accum *= radix
            accum -= n
            index += 1
          }
        } else {
          mutState.error = if (signed) "Expected digit." else "Expected sign or digit."
          return dummy // !!!
        }

        // And there can be many remaining digits
        var done = false
        while (index < mutState.input.length && !done) {
          val c = mutState.input(index)
          val n = Character.digit(c, radix)
          if (n < 0) {
            done = true
          } else {
            val temp = accum * radix - n
            // Since we're at most multiplying by ten we can catch over/underflow here, but it
            // still leaves the case of Int.MinValue which can't be negated, so we have to check
            // one last time below to be sure the sign and `neg` agree.
            if (temp > 0) {
              mutState.error = "Integer over/underflow."
              return dummy // !!!
            }
            accum  = temp
            index += 1
          }
        }

        // Done
        val ret = if (neg) accum else -accum
        // This catches the case where we parse 2147483648 which is valid if there's a leading '-'
        // but overflows otherwise. This is kind of janky, maybe we can fold it into the logic
        // above somehow.
        if (ret == 0 || (neg && ret < 0) || ret > 0) {
          mutState.offset = index
          ret
        } else {
          mutState.error = "Integer over/underflow."
          dummy
        }

    }
  }

}

