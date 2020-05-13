package a22o
package arb

import org.scalacheck.Gen
import cats.data.Ior


object BaseTestableParser extends TParsers {

  lazy val base = List(
    const,
    fail,
    skip,
    unit,
    char,
  )

  def combinator(maxDepth: Int) = List(
    token(maxDepth),
    void(maxDepth),
  )

  // Constructors

  lazy val const: Gen[TParser] =
    Gen.posNum[Int].flatMap { n =>
      TParser(Parser.const(n), Ior.left(Gen.const("")))
    }

  lazy val fail: Gen[TParser] =
    Gen.const(TParser(Parser.fail("always fails"), Ior.right(Gen.const(""))))

  lazy val skip: Gen[TParser] =
    Gen.choose(1, 10).flatMap { n =>
      TParser(
        Parser.skip(n),
        Ior.both(
          Gen.listOfN(n, Gen.alphaNumChar).map(_.mkString),
          Gen.const("")
        )
      )
    }

  lazy val unit: Gen[TParser] =
    Gen.const(TParser(Parser.unit, Ior.left(Gen.const(""))))

  lazy val char: Gen[TParser] =
    Gen.alphaNumChar.map { c =>
      TParser(
        Parser.char(c),
        Ior.both(
          Gen.const(c.toString),
          Gen.alphaStr.filterNot(_.startsWith(c.toString))
        )
      )
    }

  // Combinators

  def token(maxDepth: Int): Gen[TParser] =
    tParser(maxDepth - 1).map { tp =>
      TParser(
        tp.parser.token,
        tp.generators.leftMap(_.flatMap(s => whitespaceStr.map(s + _)))
      )
    }

  def void(maxDepth: Int): Gen[TParser] =
    tParser(maxDepth - 1).map { tp =>
      TParser(tp.parser.void, tp.generators)
    }


}


