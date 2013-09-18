package org.scommon.script.engine.core

import org.scommon.core.Enum

object CompilerMessageSeverity extends Enum {
  sealed case class EnumVal private[CompilerMessageSeverity](id: String, title: String) extends Value {
    def isKnown = CompilerMessageSeverity.isKnown(this)
    def isUnknown = CompilerMessageSeverity.isUnknown(this)
  }

  val Unknown     = EnumVal("unknown", "Unknown")

  val Information = EnumVal("info",    "Information")
  val Warning     = EnumVal("warning", "Warning")
  val Error       = EnumVal("error",   "Error")

  def isUnknown(severity: CompilerMessageSeverity.EnumVal): Boolean =
    Unknown == severity

  def isKnown(severity: CompilerMessageSeverity.EnumVal): Boolean =
    !isUnknown(severity)
}