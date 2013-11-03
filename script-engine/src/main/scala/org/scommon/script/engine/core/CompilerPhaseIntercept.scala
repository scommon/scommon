package org.scommon.script.engine.core

trait CompilerPhaseIntercept {
  def name: String
  def runsAfterPhases: Iterable[CompilerPhase.EnumVal] = Iterable.empty
  def runsRightAfterPhase: Option[CompilerPhase.EnumVal] = None
  def runsBeforePhases: Iterable[CompilerPhase.EnumVal] = Iterable.empty
}

object CompilerPhaseIntercept {

}