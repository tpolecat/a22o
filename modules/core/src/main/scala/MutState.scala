// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package a22o

private[a22o] final class MutState(val input: String) {
  var offset: Int = 0
  var error: String = null
  def isErrored = error != null
  def isOk = error == null
  def remaining: Int = input.length - offset

  def reset(newOffset: Int) = {
    offset = newOffset
    error  = null
  }

  override def toString =
    s"MutState(${input.take(offset)}‸${input.drop(offset)})"

}
