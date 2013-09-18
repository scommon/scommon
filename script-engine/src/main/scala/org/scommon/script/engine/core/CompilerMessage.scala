package org.scommon.script.engine.core

/**
 * A message from the compiler.
 */
trait CompilerMessage {
  def severity: CompilerMessageSeverity.EnumVal
  def message: String
  def position: Position

  override def toString = message
}

object StandardCompilerMessage {
  def apply(severity: CompilerMessageSeverity.EnumVal, message: String, line: Int, column: Int): CompilerMessage =
    StandardCompilerMessage(severity, message, StandardPosition(line, column))
}

case class StandardCompilerMessage(severity: CompilerMessageSeverity.EnumVal, message: String, position: Position) extends CompilerMessage