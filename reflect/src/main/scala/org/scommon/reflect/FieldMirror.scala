package org.scommon.reflect

import org.scommon.core._

import scala.reflect.runtime.universe._

/**
 * Provides access to a list of fields that describe the object.
 */
trait FieldMirror[+T >: FieldDescriptor[Any]] {
  def fields: Iterable[T]
}


/**
 * Provides a way to convert between representations of data.
 *
 * @tparam TFrom Type of data to accept for the conversion.
 * @tparam TTo Type of data that the converter will emit.
 */
trait FieldConverter[-TFrom, +TTo] extends (TFrom => TTo) {
  def apply(from: TFrom): TTo
}

/**
 * Describes a default value to use for a given type.
 * @tparam T Data type whose default value is requested.
 */
trait FieldDefault[+T] {
  def apply(): T
}

/**
 * Describes a field.
 */
trait FieldDescriptor[+T] {
  def name: String
  def title: String
  def description: String

  def default: FieldDefault[T]
  def dataType: TypeTag[_]

  def toStringConverter[U >: T](value: U)(fnConvert: U => String): String = fnConvert(value)
  def toStringConverter[U >: T](value: U)(implicit converter: FieldConverter[U, String]): String = converter(value)

  def fromStringConverter[U >: T](value: String)(fnConvert: String => U): U = fnConvert(value)
  def fromStringConverter[U >: T](value: String)(implicit converter: FieldConverter[String, U]): U = converter(value)

  override def toString = if (title.isNonEmpty) title else name
}

case class Default[T](value: T) extends FieldDefault[T] {
  def apply() = value
}

case class Converter[-TFrom, +TTo](fn: (TFrom => TTo)) extends FieldConverter[TFrom, TTo] {
  def apply(value: TFrom): TTo = fn(value)
}

object Field {
  def apply[T](name: String, title: String = "", description: String = "")(implicit default: FieldDefault[T], dataType: TypeTag[T]): FieldDescriptor[T] =
    Field[T](name, title, description, default, dataType)

  def withDefault[T](name: String, title: String = "", description: String = "", default: T)(implicit dataType: TypeTag[T]): FieldDescriptor[T] =
    Field[T](name, title, description, Default[T](default), dataType)
}

case class Field[T](name: String, title: String, description: String, default: FieldDefault[T], dataType: TypeTag[T]) extends FieldDescriptor[T] {
  require(name.isNonEmpty, "Missing the field name")
  require(default ne null, "default cannot be null")
  require(dataType ne null, "dataType cannot be null")
}