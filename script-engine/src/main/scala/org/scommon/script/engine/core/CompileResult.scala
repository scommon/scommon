package org.scommon.script.engine.core

import scala.collection._

import scala.reflect.runtime.universe._

import org.scommon.security.SecurityContext
import org.scommon.reflect.{ClassDescription, ClassRegistry, Mirror}

sealed case class CompileResult ( //NOT serializable
    entryPoints  : Mirror.SerializableDiscoveredEntryPoints
  , filterTypes  : Mirror.SerializableDiscoveredTypeMap
  , classRegistry: ClassRegistry
) {
  def toClassLoader(parent: ClassLoader = Thread.currentThread().getContextClassLoader)(implicit context: SecurityContext) =
    classRegistry.toClassLoader(parent)(context)

  def discoverMainMethods(parent: ClassLoader = Thread.currentThread().getContextClassLoader)(implicit context: SecurityContext): Iterable[MethodMirror] =
    Mirror.discoverMainMethods(entryPoints, toClassLoader(parent)(context))

  def discovered[T : TypeTag]: Iterable[ClassDescription] =
    discovered(CompilerTypeFilter[T])

  def discovered(typeFilter: CompilerTypeFilter): Iterable[ClassDescription] =
    filterTypes.getOrElse(typeFilter.name, Set())

  def discovered(typeName: String): Iterable[ClassDescription] =
    filterTypes.getOrElse(typeName, Set())
}
