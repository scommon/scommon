package org.scommon.script.engine.core

sealed case class CompileUpdate(
    completed: Boolean
  , progress : Option[CompileProgress] = None
  , message  : Option[CompileMessage]  = None
  , result   : Option[CompileResult]   = None
) {
  override def toString =
    s"CompileUpdate(completed = $completed" +
       (if (progress.isDefined) s", ${progress.get}" else s"") +
       (if (message.isDefined)  s", ${message.get}"  else s"") +
       (if (result.isDefined)   s", ${result.get}"   else s"") +
    s")"
}

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


