package org.scommon.security2

object Sandbox {
  def securityManager: SecurityManager.type =
    SecurityManager

  def run[T](callback: => T)(implicit context: SecurityContext): T =
    runPrivileged[T](callback)(context)

  def runUnprivileged[T](callback: => T): T =
    SecurityManager.unprivileged(callback)

  def runPrivileged[T](callback: => T)(implicit context: SecurityContext): T =
    SecurityManager.privileged(callback)(context)
}
