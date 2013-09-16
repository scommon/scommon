package org.scommon.script.engine.core

object CompilerError {
  def apply(message: String): CompilerError = CompilerError(message, null)
  def apply(cause: Throwable): CompilerError = CompilerError(cause.getMessage, cause)
}

case class CompilerError(message: String, cause: Throwable) extends RuntimeException(message, cause)
