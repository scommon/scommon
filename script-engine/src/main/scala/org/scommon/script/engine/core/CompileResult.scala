package org.scommon.script.engine.core

object CompileResult {
  type SerializableDiscoveredTypeMap = Map[String, Iterable[ClassDescription]]
  type SerializableDiscoveredEntryPoints = Iterable[ClassDescription]

  def apply(entryPoints: SerializableDiscoveredEntryPoints, filterTypes: SerializableDiscoveredTypeMap, classRegistry: ClassRegistry) =
    StandardCompileResult(entryPoints, filterTypes, classRegistry)
}

import CompileResult._

trait CompileResult { //NOT serializable
  def entryPoints: SerializableDiscoveredEntryPoints
  def filterTypes: SerializableDiscoveredTypeMap
  def classRegistry: ClassRegistry
}

private[core] sealed case class StandardCompileResult(
    entryPoints  : SerializableDiscoveredEntryPoints
  , filterTypes  : SerializableDiscoveredTypeMap
  , classRegistry: ClassRegistry
) extends CompileResult