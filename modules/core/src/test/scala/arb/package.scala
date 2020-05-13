package a22o

import org.scalacheck.Gen

package object arb {

  private[this] val MaxDepth = 5

  /** Registry of all TParsers. */
  private[this] lazy val all: List[TParsers] =
    List(
      BaseTestableParser,
    )

  private[this] lazy val allBase: List[Gen[TParser]] =
    all.flatMap(_.base)

  private[this] def allCombinator(maxDepth: Int): List[Gen[TParser]] =
    all.flatMap(_.combinator(maxDepth))

  assert(allBase.length > 1)
  assert(allCombinator(1).length > 1)

  private[arb] def tParser(maxDepth: Int): Gen[TParser] = {
    val gs = if (maxDepth < 1) allBase else allBase ++ allCombinator(maxDepth)
    Gen.oneOf(gs(0), gs(1), gs.drop(2): _*)
  // } .map { tp =>
  //   println(s"tParser($maxDepth) => ${tp.parser}")
  //   tp
  }

  lazy val parserWithInputAndExpectation: Gen[(Parser[_], String, Boolean)] =
    tParser(MaxDepth).flatMap(tp => tp.inputWithExpectation.map { case (s, b) => (tp.parser, s, b) })

  lazy val parserWithInput: Gen[(Parser[_], String)] =
    tParser(MaxDepth).flatMap(tp => tp.input.map(s => (tp.parser, s)))

  lazy val parser: Gen[Parser[_]] =
    tParser(MaxDepth).map(_.parser)

  lazy val whitespace: Gen[Char] =
    Gen.oneOf("\t\r\n\f ": Seq[Char])

  lazy val whitespaceStr =
    Gen.choose(0, 10).flatMap(Gen.listOfN(_, whitespace).map(_.mkString))

}
