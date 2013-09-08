package org.scommon.script.engine.core

case class CompilerProgress(phase: CompilerPhase.EnumVal, phaseIndex: Int, totalPhaseCount: Int, progress: Double)

trait CompilerProgressListener {
  def progressUpdate(update: CompilerProgress)
}
