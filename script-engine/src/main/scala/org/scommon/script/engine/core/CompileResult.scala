package org.scommon.script.engine.core

object CompileResult {
  type DiscoveredTypeMap = Map[CompilerTypeFilter, Iterable[String]]

  def apply(typesDiscovered: DiscoveredTypeMap = Map() withDefaultValue Iterable()) =
    StandardCompileResult(typesDiscovered)
}

import CompileResult._

trait CompileResult {
  def typesDiscovered: DiscoveredTypeMap
}

sealed case class StandardCompileResult(
  typesDiscovered: DiscoveredTypeMap
) extends CompileResult