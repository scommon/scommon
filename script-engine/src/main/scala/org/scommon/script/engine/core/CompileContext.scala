package org.scommon.script.engine.core

/**
 * Context for use in [[org.scommon.reactive.Generator]]s.
 */
trait CompileContext {
  def handlers: CompileEventHandlers
}

object CompileContext {
  private[this] case class MutableCompileContext(
      var handlers: CompileEventHandlers
  ) extends CompileContext {
    override def toString = s"CompileContext($handlers)"
  }

  def apply(handlers: CompileEventHandlers = CompileEventHandlers()): CompileContext =
    MutableCompileContext(handlers)

  def unapply(c: CompileContext) = c match {
    case m: MutableCompileContext => MutableCompileContext.unapply(m)
    case _ => None
  }
}

