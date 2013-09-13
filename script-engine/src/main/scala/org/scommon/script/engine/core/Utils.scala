package org.scommon.script.engine.core

object Utils {
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
}
