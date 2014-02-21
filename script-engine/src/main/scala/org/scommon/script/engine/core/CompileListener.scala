package org.scommon.script.engine.core

sealed trait CompileUpdate
sealed case class CompileProgressUpdate(progress: CompileProgress) extends CompileUpdate
sealed case class CompileMessageReceived(message: CompileMessage) extends CompileUpdate
sealed case class CompileCompleted(result: CompileResult) extends CompileUpdate
sealed case class CompileFatalError(error: Throwable) extends CompileUpdate

trait CompileListener {
  def onUpdate(update: CompileUpdate): Unit
  def onClose()                      : Unit
}

object CompileListener {
  private[this] case class InnerCompileListener(
      fnUpdate: CompileUpdate => Unit
    , fnClose : () => Unit
  ) extends CompileListener {
    def onUpdate(update: CompileUpdate) = fnUpdate(update)
    def onClose(): Unit                 = fnClose()
    override def toString = s"CompileListener(onUpdate = <fn>, onClose = <fn>)"
  }

  def apply(  fnUpdate: CompileUpdate => Unit = {_=>}
            , fnClose : () => Unit            = {()=>}): CompileListener =
    InnerCompileListener(fnUpdate, fnClose)

  def unapply(l: CompileListener) = l match {
    case i: InnerCompileListener => InnerCompileListener.unapply(i)
    case _ => None
  }
}


