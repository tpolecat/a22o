// Copyright (c) 2020 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package object a22o {

  type ~[+A, +B] = (A, B)
  object ~ {
    def unapply[A, B](t: A ~ B): Some[A ~ B] = Some(t)
  }

}