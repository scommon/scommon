package org.scommon

package object reflect {
  import scala.reflect.runtime.universe
  import scala.reflect.runtime.universe._

  /**
   * Warning: use of these extensions are reliable only when you have the type available at the usage site.
   *
   * e.g., if I have:
   *
   * <code>
   *   val x = "foo".typeOf
   * </code>
   *
   * This is alright because the type of "foo" is known. x will be equivalent to typeOf[String]. However, if you have
   * the following:
   *
   * <code>
   *   val x = "foo"
   *   val y = x.asInstanceOf[Any]
   *   val z = y.typeOf
   * </code>
   *
   * z will be equivalent to typeOf[Any] and not typeOf[String]. This is because the compiler will extract the
   * type tag at the declaration site and is not discovered dynamically at runtime.
   */
  implicit class ReflectExtensions[U: TypeTag](that: U) {
    @inline def typeOf =
      universe.typeOf[U]

    @inline def typeSymbol =
      universe.typeOf[U].typeSymbol

    @inline def <:<(tpe: Type): Boolean =
      universe.typeOf[U] <:< tpe

    @inline def =:=(tpe: Type): Boolean =
      universe.typeOf[U] =:= tpe

    @inline def <:<[T: TypeTag]: Boolean =
      universe.typeOf[U] <:< universe.typeOf[T]

    @inline def =:=[T: TypeTag]: Boolean =
      universe.typeOf[U] =:= universe.typeOf[T]
  }

  type StandardFieldMirror = reflect.FieldMirror[FieldDescriptor[Any]]

  def toConverter[TFrom, TTo](fn: (TFrom => TTo)) = Converter(fn)

  implicit val StringToStringConverter:  FieldConverter[String, String]  = toConverter(x => x)
  implicit val BooleanToStringConverter: FieldConverter[Boolean, String] = toConverter(x => java.lang.Boolean.toString(x))
  implicit val StringToBooleanConverter: FieldConverter[String, Boolean] = toConverter(x => java.lang.Boolean.parseBoolean(x))
  implicit val ByteToStringConverter:    FieldConverter[Byte, String]    = toConverter(x => java.lang.Byte.toString(x))
  implicit val StringToByteConverter:    FieldConverter[String, Byte]    = toConverter(x => java.lang.Byte.parseByte(x))
  implicit val ShortToStringConverter:   FieldConverter[Short, String]   = toConverter(x => java.lang.Short.toString(x))
  implicit val StringToShortConverter:   FieldConverter[String, Short]   = toConverter(x => java.lang.Short.parseShort(x))
  implicit val IntToStringConverter:     FieldConverter[Int, String]     = toConverter(x => java.lang.Integer.toString(x))
  implicit val StringToIntConverter:     FieldConverter[String, Int]     = toConverter(x => java.lang.Integer.parseInt(x))
  implicit val LongToStringConverter:    FieldConverter[Long, String]    = toConverter(x => java.lang.Long.toString(x))
  implicit val StringToLongConverter:    FieldConverter[String, Long]    = toConverter(x => java.lang.Long.parseLong(x))
  implicit val FloatToStringConverter:   FieldConverter[Float, String]   = toConverter(x => java.lang.Float.toString(x))
  implicit val StringToFloatConverter:   FieldConverter[String, Float]   = toConverter(x => java.lang.Float.parseFloat(x))
  implicit val DoubleToStringConverter:  FieldConverter[Double, String]  = toConverter(x => java.lang.Double.toString(x))
  implicit val StringToDoubleConverter:  FieldConverter[String, Double]  = toConverter(x => java.lang.Double.parseDouble(x))
  implicit val AnyToStringConverter:     FieldConverter[Any, String]     = toConverter(x => x.toString)

  implicit def NumericToIntConverter[T](implicit n: Numeric[T]): FieldConverter[T, Double]    = toConverter(x => n.toInt(x))
  implicit def NumericToLongConverter[T](implicit n: Numeric[T]): FieldConverter[T, Double]   = toConverter(x => n.toLong(x))
  implicit def NumericToFloatConverter[T](implicit n: Numeric[T]): FieldConverter[T, Double]  = toConverter(x => n.toFloat(x))
  implicit def NumericToDoubleConverter[T](implicit n: Numeric[T]): FieldConverter[T, Double] = toConverter(x => n.toDouble(x))

  implicit val StringDefault    = Default("")
  implicit val ByteDefault      = Default(0.toByte)
  implicit val ShortDefault     = Default(0.toShort)
  implicit val IntDefault       = Default(0)
  implicit val LongDefault      = Default(0L)
  implicit val FloatDefault     = Default(0.0f)
  implicit val DoubleDefault    = Default(0.0d)
  implicit val AnyRefDefault    = Default[AnyRef](null)
  implicit def OptionDefault[T] = Default[Option[T]](None)
}
