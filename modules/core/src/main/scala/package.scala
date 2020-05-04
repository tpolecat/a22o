// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package object a22o {

  val A22o = a22o.parser.all

  // implicit class ParserOps[A](val self: Parser[A]) extends AnyVal {

  //   final def ~[B](pb: => Parser[B]): ApBuilder2[A, B] =
  //     new ApBuilder2(self, pb)

  //   final def |[B](pb: => Parser[B]): AltBuilder2[A, B] =
  //     new AltBuilder2(self, pb)

  // }

  // type ~[+A, +B] = (A, B)
  // object ~ {
  //   def unapply[A, B](t: A ~ B): Some[A ~ B] = Some(t)
  // }

  // // TODO: generate this for all 22 cases

  // implicit class Tuple2ParseOps[A, B](tup: (Parser[A], Parser[B])) {
  //   def zipN[C](f: (A, B) => C): Parser[C] =
  //     new Parser[C] {

  //       override def void: Parser[Unit] =
  //         tup._1.void ~>
  //         tup._2.void

  //       def mutParse(mutState: MutState): C = {
  //         val v1 = tup._1.mutParse(mutState); if (mutState.isErrored) return dummy
  //         val v2 = tup._2.mutParse(mutState); if (mutState.isErrored) return dummy
  //         f(v1, v2)
  //       }

  //     }
  // }

  // implicit class Tuple3ParseOps[A, B, C](tup: (Parser[A], Parser[B], Parser[C])) {
  //   def zipN[D](f: (A, B, C) => D): Parser[D] =
  //     new Parser[D] {

  //       override def void: Parser[Unit] =
  //         tup._1.void ~>
  //         tup._2.void ~>
  //         tup._3.void

  //       def mutParse(mutState: MutState): D = {
  //         val v1 = tup._1.mutParse(mutState); if (mutState.isErrored) return dummy
  //         val v2 = tup._2.mutParse(mutState); if (mutState.isErrored) return dummy
  //         val v3 = tup._3.mutParse(mutState); if (mutState.isErrored) return dummy
  //         f(v1, v2, v3)
  //       }

  //     }
  // }

  // implicit class Tuple4ParseOps[A, B, C, D](tup: (Parser[A], Parser[B], Parser[C], Parser[D])) {
  //   def zipN[E](f: (A, B, C, D) => E): Parser[E] =
  //     new Parser[E] {

  //       override def void: Parser[Unit] =
  //         tup._1.void ~>
  //         tup._2.void ~>
  //         tup._3.void ~>
  //         tup._4.void

  //       def mutParse(mutState: MutState): E = {
  //         val v1 = tup._1.mutParse(mutState); if (mutState.isErrored) return dummy
  //         val v2 = tup._2.mutParse(mutState); if (mutState.isErrored) return dummy
  //         val v3 = tup._3.mutParse(mutState); if (mutState.isErrored) return dummy
  //         val v4 = tup._4.mutParse(mutState); if (mutState.isErrored) return dummy
  //         f(v1, v2, v3, v4)
  //       }

  //     }
  // }

}