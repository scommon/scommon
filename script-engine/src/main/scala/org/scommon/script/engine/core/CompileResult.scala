package org.scommon.script.engine.core

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

import org.scommon.security.SecurityContext

sealed case class CompileResult ( //NOT serializable
    entryPoints  : CompileResult.SerializableDiscoveredEntryPoints
  , filterTypes  : CompileResult.SerializableDiscoveredTypeMap
  , classRegistry: ClassRegistry
) {
  def toClassLoader(parent: ClassLoader = Thread.currentThread().getContextClassLoader)(implicit context: SecurityContext) =
    classRegistry.toClassLoader(parent)(context)

  def discoverMainMethods(parent: ClassLoader = Thread.currentThread().getContextClassLoader)(implicit context: SecurityContext): Iterable[MethodMirror] =
    CompileResult.discoverMainMethods(entryPoints, toClassLoader(parent)(context))
}

object CompileResult {
  type SerializableDiscoveredTypeMap = Map[String, Iterable[ClassDescription]]
  type SerializableDiscoveredEntryPoints = Iterable[ClassDescription]

  def discoverMainMethods(entryPoints: SerializableDiscoveredEntryPoints, classLoader: ClassLoader): Iterable[MethodMirror] = {
      val runtime_mirror = universe.runtimeMirror(classLoader)

      for {
        entry_point <- entryPoints
        //cls = Class.forName(entry_point.javaClassName, false, cl)
        module    = runtime_mirror.staticModule(entry_point.scalaClassName)
        obj       = runtime_mirror.reflectModule(module)
        reflected = runtime_mirror.reflect(obj.instance)
        method    = obj.symbol.typeSignature.member(newTermName("main")).asMethod
        main      = reflected.reflectMethod(method)
      } yield main
    }
}
