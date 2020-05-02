// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o
package parser

trait Combinator {

  def void: Parser[Unit] =
    ok(())

  def ok[A](a: A): Parser[A] =
    new Parser[A] {
      def mutParse(mutState: MutState): A = a
    }

  def fail[A](message: String): Parser[A] =
    new Parser[A] {
      def mutParse(mutState: MutState): A = {
        mutState.setError(message)
        dummy
      }
    }

  def ensure(n: Int): Parser[Unit] =
   new Parser[Unit] {
     def mutParse(mutState: MutState): Unit =
      if (mutState.remaining >= n) ()
      else {
        mutState.setError("ensure: insufficient input")
        dummy
      }
   }

  def discardRight[A, B](pa: Parser[A], pb: => Parser[B]): Parser[A] =
    (pa ~ pb.void).mapN((a, _) => a)

  def discardLeft[A, B](pa: Parser[A], pb: => Parser[B]): Parser[B] =
    (pa.void ~ pb).mapN((_, b) => b)

  // def or[A](pa: Parser[A], pb: => Parser[A]): Parser[A] =
  //   new Parser[A] {
  //     lazy val pbʹ = pb
  //     override def void = or(pa.void, pb.void)
  //     def mutParse(mutState: MutState): A = {
  //       val o = mutState.getPoint
  //       val a = pa.mutParse(mutState)
  //       if (mutState.isOk) a
  //       else {
  //         mutState.reset(o)
  //         val b = pbʹ.mutParse(mutState)
  //         if (mutState.isOk) b
  //         else dummy
  //       }
  //     }
  //   }

  // def either[A, B](pa: Parser[A], pb: => Parser[B]): Parser[Either[A, B]] =
  //   new Parser[Either[A, B]] {
  //     lazy val pbʹ = pb
  //     override def void = or(pa.void, pb.void)
  //     def mutParse(mutState: MutState): Either[A, B] = {
  //       val o = mutState.getPoint
  //       val a = pa.mutParse(mutState)
  //       if (mutState.isOk) Left(a)
  //       else {
  //         mutState.reset(o)
  //         val b = pbʹ.mutParse(mutState)
  //         if (mutState.isOk) Right(b)
  //         else dummy
  //       }
  //     }
  //   }


  // def many[A](pa: Parser[A]): Parser[List[A]] =
  //   new Parser[List[A]] {
  //     override def void = pa.skipMany
  //     def mutParse(mutState: MutState): List[A] = {
  //       val list = List.newBuilder[A]
  //       @tailrec def go: List[A] = {
  //         val o = mutState.getPoint
  //         val a = pa.mutParse(mutState)
  //         if (mutState.isOk) {
  //           if (mutState.getPoint == o) {
  //             mutState.setError("many: nontermination detected.")
  //             dummy
  //           } else {
  //           list += a
  //           go
  //           }
  //         } else {
  //           mutState.reset(o)
  //           list.result
  //         }
  //       }
  //       go
  //     }
  //   }

  // def skipMany[A](pa: Parser[A]): Parser[Unit] =
  //   new Parser[Unit] {
  //     val paʹ = pa.void
  //     override val void = this
  //     def mutParse(mutState: MutState): Unit = {
  //       @tailrec def go(): Unit = {
  //         val o = mutState.getPoint
  //         paʹ.mutParse(mutState)
  //         if (mutState.isOk) {
  //           if (mutState.getPoint == o) {
  //             mutState.setError("skipMany: nontermination detected.")
  //             dummy
  //           } else go()
  //         } else {
  //           mutState.reset(o)
  //           ()
  //         }
  //       }
  //       go()
  //     }
  //   }

  // def many1[A](pa: Parser[A]): Parser[NonEmptyList[A]] =
  //   new Parser[NonEmptyList[A]] {
  //     override def void = pa.skipMany1
  //     def mutParse(mutState: MutState): NonEmptyList[A] = {
  //       val head = pa.mutParse(mutState)
  //       if (mutState.isOk) {
  //         val tail = List.newBuilder[A]
  //         @tailrec def go: NonEmptyList[A] = {
  //           val o = mutState.getPoint
  //           val a = pa.mutParse(mutState)
  //           if (mutState.isOk) {
  //             if (mutState.getPoint == o) {
  //               mutState.setError("many1: nontermination detected.")
  //               dummy
  //             } else {
  //             tail += a
  //             go
  //             }
  //           } else {
  //             mutState.reset(o)
  //             NonEmptyList(head, tail.result)
  //           }
  //         }
  //         go
  //       } else dummy
  //     }
  //   }

  // def sepBy[A](pa: Parser[A], sep: Parser[_]): Parser[List[A]] =
  //   new Parser[List[A]] {
  //     val sepʹ = sep.void
  //     override def void = pa.skipMany1
  //     def mutParse(mutState: MutState): List[A] = {
  //       val head = pa.mutParse(mutState)
  //       val o = mutState.getPoint
  //       if (mutState.isOk) {
  //         val tail = List.newBuilder[A]
  //         @tailrec def go: List[A] = {
  //           val o = mutState.getPoint
  //           sepʹ.mutParse(mutState)
  //           if (mutState.isErrored) {
  //             mutState.reset(o)
  //             head :: tail.result
  //           } else {
  //             val a = pa.mutParse(mutState)
  //             if (mutState.isOk) {
  //               if (mutState.getPoint == o) {
  //                 mutState.setError("many1: nontermination detected.")
  //                 dummy
  //               } else {
  //               tail += a
  //               go
  //               }
  //             } else {
  //               mutState.reset(o)
  //               head :: tail.result
  //             }
  //           }
  //         }
  //         go
  //       } else {
  //         mutState.reset(o)
  //         Nil
  //       }
  //     }
  //   }

  // def skipMany1[A](pa: Parser[A]): Parser[Unit] =
  //   new Parser[Unit] {
  //     val paʹ = pa.void
  //     override val void = this
  //     def mutParse(mutState: MutState): Unit = {
  //       pa.mutParse(mutState)
  //       if (mutState.isOk) {
  //         @tailrec def go(): Unit = {
  //           val o = mutState.getPoint
  //           paʹ.mutParse(mutState)
  //           if (mutState.isOk) {
  //             if (mutState.getPoint == o) {
  //               mutState.setError("skipMany1: nontermination detected.")
  //               dummy
  //             } else go()
  //           } else {
  //             mutState.reset(o)
  //             ()
  //           }
  //         }
  //         go()
  //       } else dummy
  //     }
  //   }

  def skip(n: Int): Parser[Unit] =
    new Parser[Unit] {
      override val void = this
      def mutParse(mutState: MutState): Unit =
        if (n < 0) {
          mutState.setError("skip: negative length")
          dummy
        } else if (mutState.remaining >= n) {
          mutState.advance(n)
          ()
        } else {
          mutState.setError("skip: insufficient input")
          dummy
        }
    }

  /** Parser that yields the same result as `p`, but consumes no input. */
  def peek[A](p: Parser[A]): Parser[A] =
    new Parser[A] {
      override def void = peek(p.void)
      def mutParse(mutState: MutState): A = {
        val o = mutState.getPoint
        val a = p.mutParse(mutState)
        mutState.setPoint(o)
        a
      }
    }

  def opt[A](pa: Parser[A]): Parser[Option[A]] =
    new Parser[Option[A]] {
      override def void: Parser[Unit] =
        new Parser[Unit] {
          val paʹ = pa.void
          override def void = this
          def mutParse(mutState: MutState): Unit = {
            paʹ.mutParse(mutState)
            mutState.setError(null)
            ()
          }
        }
      def mutParse(mutState: MutState): Option[A] = {
        val a = pa.mutParse(mutState)
        if (mutState.isOk) Some(a)
        else {
          mutState.setError(null)
          None
        }
      }
    }

}
