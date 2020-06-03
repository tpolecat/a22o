// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

// By encoding both the mutable parse state and the result as a single value we only end up doing
// one allocation when we parse (which is mostly a way to juice benchmarks).

/**
 * Parse result, either an error or a result, plus any unconsumed input.
 */
sealed trait Result[+A] {

  /**
   * The unconsumed input. Note that this value is not allocated until requested.
   * @group properties
   */
  def remainingInput: String

  /**
   * True if parsing was successful, false otherwise.
   * @group properties
   */
  def isOk: Boolean

  /**
   * True if parsing failed, false otherwise.
   * @group properties
   */
  def isError: Boolean

  /**
   * Eliminate this `Result` by folding the error and success cases into some type `B`.
   * @group eliminators
   */
  def fold[B](f: String => B, g: A => B): B

  /**
   * Eliminate this `Result` by yielding an `Either` containing the error or the successful result.
   * @group eliminators
   */
  final def toEither: Either[String, A] =
    fold(Left(_), Right(_))

  /**
   * Eliminate this `Result` by yielding the remaining input, as well as an `Either` containing
   * the error or the successful result.
   * @group eliminators
   */
  final def toRemainingAndEither: (String, Either[String, A]) =
    (remainingInput, toEither)

  /**
   * Eliminate this `Result` by yielding the successful result, if any.
   * @group eliminators
   */
  final def toOption: Option[A] =
    fold(_ => None, Some(_))

  final override def toString: String =
    fold(
      e => s"Result.Error($e)",
      a => s"Result.Ok($a)"
    )

}

/** Module of `Result` unapplies. */
object Result {

  /** Module providing an unapply for successful parse results. */
  object Ok {
    def unapply[A](ra: Result[A]): Option[A] = ra.toOption
  }

  /** Module providing an unapply for failed parse results. */
  object Error {
    def unapply[A](ra: Result[A]): Option[String] = ra.fold(Some(_), _ => None)
  }

}

private[a22o] sealed trait ParseState {
  def getPoint: Int
  def setPoint(point: Int): Unit
  def setError(message: String): Unit
  def getError: String
  def isError: Boolean
  def isOk: Boolean
  def remaining: Int
  def reset(point: Int): Unit
  def charAt(n: Int): Char
  def advance(n: Int): Unit
  def consume(n: Int): String
  def remainingInput: String
  def startsWith(s: String): Boolean
  def result[A](t: A): Result[A]
}

private[a22o] object ParseState {

  def apply(inputString: String): ParseState =
    new ParseStateImpl[Nothing](inputString)

  private final class ParseStateImpl[A](private val input: String) extends ParseState with Result[A] {

    var pos: Int = 0
    var error: String = null
    var value: A = null.asInstanceOf[A]

    @inline def getPoint: Int = pos
    @inline def setPoint(point: Int): Unit = pos = point
    @inline def setError(message: String): Unit = error = message
    @inline def getError: String = error
    @inline def isError: Boolean = error != null
    @inline def isOk: Boolean = error == null
    @inline def remaining: Int = input.length - pos
    @inline def reset(point: Int) = { pos = point; error = null }
    @inline def charAt(n: Int): Char = input.charAt(pos + n)
    @inline def advance(n: Int) = pos += n
    @inline def consume(n: Int): String = { val s = input.substring(pos, pos + n); pos += n; s}
    @inline def remainingInput = input.substring(pos)
    @inline def startsWith(s: String): Boolean = input.startsWith(s, pos)

    @inline def fold[B](f: String => B, g: A => B): B =
      if (isError) f(error) else g(value)

    @inline def result[T](t: T): Result[T] = {
      val r = this.asInstanceOf[ParseStateImpl[T]]
      r.value = t
      r
    }

    final override def equals(a: Any): Boolean =
      a match {
        case r: ParseStateImpl[_] => r.input == input && r.pos == pos && r.remaining == remaining && r.value == value
        case _ => false
      }


  }

}

