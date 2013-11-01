package org.scommon.security2

object Sandbox {
  implicit def defaultContext = CONTEXT_DEFAULT

  def securityManager: SecurityManager.type =
    SecurityManager

  def run[T](callback: => T)(implicit context: Context): T =
    runSecurely[T](callback)(context)

  def runUnsecurely[T](callback: => T): T =
    SecurityManager.disabled(callback)

  def runSecurely[T](callback: => T)(implicit context: Context): T =
    SecurityManager.enabled(callback)(context)
}
