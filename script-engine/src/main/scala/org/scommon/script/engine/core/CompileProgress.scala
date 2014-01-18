package org.scommon.script.engine.core

sealed case class CompileProgress(
    phase          : CompilerPhase.EnumVal
  , phaseIndex     : Int
  , totalPhaseCount: Int
  , progress       : Double
)

trait CompilerProgressListener {
  def progressUpdate(update: CompileProgress)
}
