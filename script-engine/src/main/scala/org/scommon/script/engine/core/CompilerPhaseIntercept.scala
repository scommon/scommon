package org.scommon.script.engine.core

trait CompilerPhaseIntercept {
  def name: String
  def runsAfterPhases: Iterable[CompilerPhase.EnumVal] = Iterable.empty
  def runsRightAfterPhase: Option[CompilerPhase.EnumVal] = None
  def runsBeforePhases: Iterable[CompilerPhase.EnumVal] = Iterable.empty
}

object CompilerPhaseIntercept {
  def unapply(i: CompilerPhaseIntercept): Option[(String, Iterable[CompilerPhase.EnumVal], Option[CompilerPhase.EnumVal], Iterable[CompilerPhase.EnumVal])] =
    if (i ne null)
      Some(i.name, i.runsAfterPhases, i.runsRightAfterPhase, i.runsBeforePhases)
    else
      None
}
