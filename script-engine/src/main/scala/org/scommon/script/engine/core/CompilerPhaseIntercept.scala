package org.scommon.script.engine.core

trait CompilerPhaseIntercept {
  def name: String
  def runsBeforePhases: Iterable[CompilerPhase.EnumVal] = Iterable.empty
  def runsAfterPhases: Iterable[CompilerPhase.EnumVal] = Iterable.empty
  def runsRightAfterPhase: Option[CompilerPhase.EnumVal] = None
  //def process(context: TContext)(unit: context..CompilationUnit): Unit
  //def process(global: nsc.Global)(unit: global.CompilationUnit): Unit
}

object CompilerPhaseIntercept {

}