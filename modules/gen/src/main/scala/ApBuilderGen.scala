// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

import java.io.File
import java.nio.file.Files

abstract class BuilderGen(val baseName: String) {

  /** The Nth type variable. */
  def tvar(n: Int): String =
    ('A' + (n - 1)).toChar.toString

  /** The Nth covariant type parameter. */
  def tparam(n: Int, variance: String = ""): String =
    s"$variance${tvar(n)}"

  /** The Nth variable. */
  def `var`(n: Int): String =
    ('a' + (n - 1)).toChar.toString

  /** The Nth primed variable. */
  def varʹ(n: Int): String =
    ('a' + (n - 1)).toChar.toString

  /** The Nth parser. */
  def parser(n: Int): String =
    "p" + `var`(n)

  /** The Nth primed parser. */
  def parserʹ(n: Int): String =
    s"${parser(n)}ʹ"

  /** Comma-separated list at the given arity. */
  def tvars(arity: Int): String =
    (1 to arity).map(tvar).mkString(",")

  /** Comma-separated list at the given arity. */
  def tparams(arity: Int, variance: String = ""): String =
    (1 to arity).map(tparam(_, variance)).mkString(",")

  /** Comma-separated list at the given arity. */
  def parsers(arity: Int): String =
    (1 to arity).map(parser).mkString(", ")

  /** Comma-separated list at the given arity. */
  def vars(arity: Int): String =
    (1 to arity).map(`var`).mkString(", ")

  /** Type name at the given arity, with type parameters. */
  def typeDecl(arity: Int): String =
    s"$baseName$arity[${tparams(arity, "+")}]"

  /** Type name at the given arity, with type parameters. */
  def ctor(arity: Int): String =
     s"$baseName$arity"

  /** Type name at the given arity, with type parameters. */
  def typeName(arity: Int): String =
    s"$baseName$arity[${tparams(arity)}]"

  def indices(arity: Int): List[Int] =
    (1 to arity).toList

  def all(arity: Int): String =
    if (arity == 2) "both" else s"all $arity"

  def atArity(arity: Int): String

  def main(args: Array[String]): Unit = {
    val dest = new File("modules/core/src/main/scala/builder")
    (2 to 22).foreach { n =>
      val text = atArity(n)
      val file = new File(dest, ctor(n) + ".scala")
      Files.write(file.toPath(), text.getBytes("UTF-8"))
    }
  }

}

object ApBuilderGen extends BuilderGen("ApBuilder") {

  def atArity(arity: Int): String = {
    assert(arity > 1 && arity <= 22, "Arity must be in [2..22]")
    val next = arity + 1
    s"""|package a22o
        |package builder
        |
        |/** Applicative builder at arity $arity. */
        |final class ${typeDecl(arity)}(
        |  pa: Parser[A], ${indices(arity).drop(1).map(n => s"${parser(n)}: => Parser[${tvar(n)}]").mkString(", ")}
        |) { outer =>
        |
        ${if (arity == 22) "" else
          s"""|  /**
              |   * Append another parser to this $baseName.
              |   * @group sequencing
              |   */
              |  def ~[${tvar(next)}](${parser(next)}: => Parser[${tvar(next)}]): ${typeName(next)} =
              |    new ${typeName(next)}(${parsers(next)})
              |"""
        }
        |  /**
        |   * Eliminate this ${ctor(arity)}, yielding a parser that executes the `.void` versions of
        |   * ${all(arity)} component parsers in sequence.
        |   * @group eliminators
        |   */
        |  def void: Parser[Unit] =
        |    new ${ctor(arity)}(${indices(arity).map(n => s"${parser(n)}.void").mkString(", ")}).mapN((${List.fill(arity)("_").mkString(",")}) => ())
        |
        |  /**
        |   * Eliminate this ${ctor(arity)}, yielding a parser that executes the `.void` versions of
        |   * ${all(arity)} component parsers in sequence and returns the consumed input text. This
        |   * is equivalent to `.void.inputText`.
        |   * @group eliminators
        |   */
        |  def inputText: Parser[String] =
        |    void.inputText
        |
        |  /**
        |   * Eliminate this ${ctor(arity)}, yielding a parser that returns a Tuple$arity of
        |   * results from ${all(arity)} component parsers.
        |   * @group eliminators
        |   */
        |  def tupled: Parser[(${tvars(arity)})] =
        |    mapN(Tuple${arity}.apply)
        |
        |  /**
        |   * Eliminate this ${ctor(arity)} by providing a function that combines results from
        |   * ${all(arity)} component parsers, yielding a parser.
        |   * @group eliminators
        |   */
        |  def mapN[${tvar(arity + 1)}](ap: (${tvars(arity)}) => ${tvar(next)}): Parser[${tvar(next)}] =
        |    new Parser[${tvar(next)}] {
        |      ${indices(arity).drop(1).map(n => s"lazy val ${parserʹ(n)} = ${parser(n)}").mkString("\n|      ")}
        |      override def void: Parser[Unit] = outer.void
        |      def mutParse(mutState: MutState): ${tvar(next)} = {
        |        val a = pa .mutParse(mutState); if (mutState.isError) return dummy
        |        ${indices(arity).drop(1).map(n => s"val ${`var`(n)} = ${parserʹ(n)}.mutParse(mutState); if (mutState.isError) return dummy").mkString("\n|        ")}
        |        ap(${vars(arity)})
        |      }
        |  }
        |
        |}
        |""".stripMargin
  }

}



