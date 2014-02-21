package org.scommon.script.engine.core

/**
 * A message generated during compilation of source.
 */
sealed case class CompileMessage(
    severity: CompileMessageSeverity.EnumVal
  , message : String
  , position: Position
) {
  override def toString = message
}

object CompileMessage {
  def apply(severity: CompileMessageSeverity.EnumVal, message: String): CompileMessage =
    CompileMessage(severity, message, UnknownPosition)

  def apply(severity: CompileMessageSeverity.EnumVal, message: String, line: Int, column: Int): CompileMessage =
    CompileMessage(severity, message, Position(line, column))
}

