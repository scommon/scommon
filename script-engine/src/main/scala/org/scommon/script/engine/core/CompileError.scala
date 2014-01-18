package org.scommon.script.engine.core

object CompileError {
  def apply(message: String): CompileError = CompileError(message, null)
  def apply(cause: Throwable): CompileError = CompileError(cause.getMessage, cause)
}

sealed case class CompileError(
    message: String
  , cause: Throwable
) extends RuntimeException(message, cause)
