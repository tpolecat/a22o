// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package object a22o {
  val A22o = a22o.parser.all

  implicit def charToParser(c: Char): Parser[Char] = Parser.char(c)
  implicit def stringToParser(s: String): Parser[String] = Parser.string(s)

}