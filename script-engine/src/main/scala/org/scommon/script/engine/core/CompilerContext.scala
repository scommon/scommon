package org.scommon.script.engine.core

/**
 * Context for use in [[org.scommon.reactive.Generator]]s.
 */
trait CompilerContext {
  def handlers: CompilerEventHandlers
}
