package org.scommon.script.engine.core

trait CompileUpdate {
  def completed: Boolean
  def progress : Option[CompilerProgress]
  def message  : Option[CompilerMessage]
  def result   : Option[CompileResult]
}

sealed case class StandardCompileUpdate(
    completed: Boolean
  , progress : Option[CompilerProgress] = None
  , message  : Option[CompilerMessage]  = None
  , result   : Option[CompileResult]    = None
) extends CompileUpdate

trait CompileListener {
  def onUpdate(update: CompileUpdate): Unit
  def onClose()                      : Unit
}

sealed case class StandardCompileListener(
    fnUpdate: CompileUpdate => Unit = {_=>}
  , fnClose : () => Unit            = {()=>}
) extends CompileListener {
  def onUpdate(update: CompileUpdate) = fnUpdate(update)
  def onClose(): Unit                 = fnClose()
}
