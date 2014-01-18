package org.scommon.script.engine.core

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

object CompilerTypeFilter {
  def apply[T : TypeTag] = new CompilerTypeFilter {
    val tag     = implicitly[TypeTag[T]]
    val tagType = tag.tpe
    val name    = tagType.toString
  }

  def unapply(f: CompilerTypeFilter): Option[(String, TypeTag[_], universe.type#Type)] =
    if (f ne null)
      Some((f.name, f.tag, f.tagType))
    else
      None
}

/**
 * Describes a type that will be looked for during compilation. The results are provided by the
 * [[org.scommon.script.engine.core.CompileResult]] instance given in the
 * [[org.scommon.script.engine.core.CompileEventHandlers#sourceCompiled]] event.
 */
sealed trait CompilerTypeFilter {
  def name: String
  def tag: TypeTag[_]
  def tagType: universe.type#Type
  override def toString = s"CompilerTypeFilter($name)"
}


