// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

package object parser {

  // We declare these as members of the package object rather than direct package members to avoid
  // classfile names that differ only in case, which doesn't work on some filesystems.
  object base extends BaseParsers
  object meta extends MetaParsers
  object int  extends IntParsers
  object char extends CharParsers
  object text extends TextParser.Constructors

  object all extends BaseParsers
                with CharParsers
                with MetaParsers
                with IntParsers
                with TextParsers

}
