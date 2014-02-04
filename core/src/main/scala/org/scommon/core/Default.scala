package org.scommon.core

/**
 * Setup for emulating C#'s default keyword. Please see:
 *   http://missingfaktor.blogspot.com/2011/08/emulating-cs-default-keyword-in-scala.html
 */
trait Default[A] {
  def value: A
}

trait LowPriorityImplicitsForDefault { this: Default.type =>
  implicit def forAnyRef[A](implicit ev: Null <:< A) = Default withValue ev(null)
}

object Default extends LowPriorityImplicitsForDefault {
  def withValue[A](a: A) = new Default[A] {
    def value = a
  }

  implicit val forBoolean = Default withValue false
  implicit val forChar = Default withValue ' '
  implicit def forNumeric[A](implicit n: Numeric[A]) = Default withValue n.zero
  implicit val forString = Default withValue ""
  implicit def forOption[A] = Default withValue (None: Option[A])
}
