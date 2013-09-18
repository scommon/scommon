package org.scommon.script.engine.core

trait CompilerProgress {
  def phase: CompilerPhase.EnumVal
  def phaseIndex: Int
  def totalPhaseCount: Int
  def progress: Double
}

case class StandardCompilerProgress(phase: CompilerPhase.EnumVal, phaseIndex: Int, totalPhaseCount: Int, progress: Double) extends CompilerProgress

trait CompilerProgressListener {
  def progressUpdate(update: CompilerProgress)
}
