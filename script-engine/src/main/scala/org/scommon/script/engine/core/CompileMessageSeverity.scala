package org.scommon.script.engine.core

import org.scommon.core.Enum

object CompileMessageSeverity extends Enum {
  sealed case class EnumVal private[CompileMessageSeverity](id: String, title: String) extends Value {
    def isKnown = CompileMessageSeverity.isKnown(this)
    def isUnknown = CompileMessageSeverity.isUnknown(this)
  }

  val Unknown     = EnumVal("unknown", "Unknown")

  val Information = EnumVal("info",    "Information")
  val Warning     = EnumVal("warning", "Warning")
  val Error       = EnumVal("error",   "Error")

  def isUnknown(severity: CompileMessageSeverity.EnumVal): Boolean =
    Unknown == severity

  def isKnown(severity: CompileMessageSeverity.EnumVal): Boolean =
    !isUnknown(severity)
}