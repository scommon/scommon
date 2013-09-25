package org.scommon

import java.io.{OutputStream, File, InputStream}

import org.joda.time.{ReadableDateTime, ReadableInstant}
import org.joda.time.{DateTime => JodaDateTime}

import scala.language.reflectiveCalls
import scala.language.implicitConversions

/**
 */
package object core {
  import ZeroOrOneCustomizableComprehension._

  val readableInstantOrdering = implicitly[Ordering[ReadableInstant]]

  implicit val ReadableDateTimeOrdering = new Ordering[ReadableDateTime] {
    def compare(a:ReadableDateTime, b:ReadableDateTime) = a.compareTo(b)
  }

  implicit val DateTimeOrdering = new Ordering[JodaDateTime] {
    def compare(a:JodaDateTime, b:JodaDateTime) = a.compareTo(b)
  }

  /**
   * Provides functionality similar to C#'s default keyword.
   * However, default is now pimped -- you can do much more with it
   * than you can C#'s.
   */
  def default[A: Default] = implicitly[Default[A]].value

  /**
   * Provides functionality similar to C#'s default keyword.
   * Use when default[A] doesn't work.
   *
   * Alternative is to use "null.asInstanceOf[A]" which will
   * accomplish the same task.
   */
  def defaultValue[A] = {
    class Temp {
      var default_value: A = _
    }
    (new Temp).default_value
  }

  /**
   * Structural type that says "anything with a close method that returns Unit".
   */
  type CloseableType = { def close():Unit }

  /**
   * Type alias that describes what's required for the using method to work.
   * This follows the magnet pattern.
   *
   * @tparam A Type of resource that will be provided to the body of the using
   *           method.
   */
  type UsingMagnet[A <: java.io.Closeable] = ZeroOrOneCustomizableComprehension[A]

  /**
   * Accepts [[org.scommon.core.UsingMagnet]] instances and provides automatic
   * resource cleanup after the evaluation of the provided body.
   *
   * Instances of [[org.scommon.core.UsingMagnet]] are typically derived via
   * implicit conversion from a [[org.scommon.core.CloseableType]] or an instance
   * of [[org.scommon.core.Closeable]] or [[java.io.Closeable]].
   *
   * @param closeable Instance of a [[org.scommon.core.UsingMagnet]].
   * @param body Body that will execute with the provided resource and automatically
   *             cleaned up afterwards.
   * @tparam A Type of resource to provide to the body.
   * @return Return nothing.
   */
  def using[A <: java.io.Closeable, B](closeable: UsingMagnet[A])(body: A => B):B =
    if (closeable.isDefined)
      closeable.process(body)
    else
      throw new IllegalArgumentException(s"Closeable must be defined")

  /**
   * Accepts [[org.scommon.core.UsingMagnet]] instances and provides automatic
   * resource cleanup after the evaluation of the provided body.
   *
   * Instances of [[org.scommon.core.UsingMagnet]] are typically derived via
   * implicit conversion from a [[org.scommon.core.CloseableType]] or an instance
   * of [[org.scommon.core.Closeable]] or [[java.io.Closeable]].
   *
   * @param closeable Instance of a [[org.scommon.core.UsingMagnet]].
   * @param body Body that will execute and be automatically cleaned up afterwards.
   * @tparam A Type of resource provided.
   * @return Return nothing.
   */
  def usingUnit[A <: java.io.Closeable](closeable: UsingMagnet[A])(body: => Unit):Unit =
    closeable.foreach(c => body)

  //The following allows us to use anything with a .close() method in a for comprehension w/
  //automatic cleanup.
  private[this] class ResourceWithAutomaticCleanup[A](resource:A, fnOnFinally:A => Unit) extends ForEachProcessor {
    def foreach[T, U](value:T, fn: (T) => U):U = {
      try {
        fn(value)
      } finally {
        fnOnFinally(resource)
      }
    }
  }

  /**
   * Transforms something that's structurally a [[org.scommon.core.CloseableType]] into a concrete
   * [[org.scommon.core.Closeable]].
   *
   * @param resource The resource to convert to a [[org.scommon.core.Closeable]].
   * @tparam T Refers to the original type that is structurally equivalent to [[org.scommon.core.CloseableType]].
   * @return New instance of [[org.scommon.core.Closeable]] that encapsulates the resource.
   */
  @inline implicit def closeableType2Closeable[T <: CloseableType](resource: T): Closeable = new Closeable {
    def close() = resource.close()
  }

  /**
   * Implicitly converts something that's Closeable into a
   * [[org.scommon.core.ZeroOrOneCustomizableComprehension]]. This should be picked up for use in for comprehensions and has no
   * real utility elsewhere.
   *
   * @param closeable An instance of a class that defines a .close() method as described by Closeable.
   * @tparam T Actual type of the Closeable.
   * @return A new instance of [[org.scommon.core.ZeroOrOneCustomizableComprehension]].
   */
  @inline implicit def closeable2ZeroOrOneCustomizableComprehension[T <: java.io.Closeable](closeable: T): ZeroOrOneCustomizableComprehension[T] =
    CustomizedComprehension(
      closeable,
      new ResourceWithAutomaticCleanup[T](closeable, {_.close()})
    )

  /**
   * Implicitly converts something that's Closeable into a
   * [[org.scommon.core.ZeroOrOneCustomizableComprehension]]. This should be picked up for use in for comprehensions and has no
   * real utility elsewhere.
   *
   * @param closeable An instance of a class that defines a .close() method as described by Closeable.
   * @tparam T Actual type of the Closeable.
   * @return A new instance of [[org.scommon.core.ZeroOrOneCustomizableComprehension]].
   */
  @inline implicit def closeableType2ZeroOrOneCustomizableComprehension[T <: CloseableType](closeable: T): ZeroOrOneCustomizableComprehension[T] =
    CustomizedComprehension(
      closeable,
      new ResourceWithAutomaticCleanup[T](closeable, {_.close()})
    )

  @inline implicit class OptionStringExtensions(s: Option[String]) {
    /** @see [[org.scommon.core.StringUtil#isNoneOrEmpty(Option[String]))]] */
    @inline def isNoneOrEmpty: Boolean = StringUtil.isNoneOrEmpty(s)

    /** @see [[[org.scommon.core.StringUtil#isNonEmpty(Option[String]))]]] */
    @inline def isNonEmpty: Boolean = StringUtil.isNonEmpty(s)

    /** @see [[org.scommon.core.StringUtil#checked(Option[String]))]] */
    @inline def checked: String = StringUtil.checked(s)
  }

  @inline implicit class StringExtensions(s: String) {
    /** @see [[org.scommon.core.StringUtil#isNullOrEmpty(String)]] */
    @inline def isNullOrEmpty: Boolean = StringUtil.isNullOrEmpty(s)

    /** @see [[org.scommon.core.StringUtil#isNonEmpty(String)]] */
    @inline def isNonEmpty: Boolean = StringUtil.isNonEmpty(s)

    /** @see [[org.scommon.core.StringUtil#checked(String)]] */
    @inline def checked: String = StringUtil.checked(s)

    /** @see [[org.scommon.core.StringUtil#toValidIdentifier(String)]] */
    @inline def toValidIdentifier: String = StringUtil.toValidIdentifier(s)
  }
}