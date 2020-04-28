// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

private[a22o] final class MutState(input: String) {

  type Point = Int

  private var pos: Int = 0
  private var error: String = null

  @inline def getPoint: Point = pos
  @inline def setPoint(point: Point): Unit = pos = point
  @inline def setError(message: String): Unit = error = message
  @inline def getError: String = error
  @inline def isErrored: Boolean = error != null
  @inline def isOk: Boolean = error == null
  @inline def remaining: Int = input.length - pos
  @inline def reset(point: Point) = { setPoint(point); setError(null)  }
  @inline def charAt(n: Int): Char = input.charAt(pos + n)
  @inline def advance(n: Int) = pos += n
  @inline def consume(n: Int): String = { val s = input.substring(pos, pos + n); pos += n; s}
  @inline def remainingInput = input.substring(pos)
  @inline def startsWith(s: String): Boolean = input.startsWith(s, pos)

}
