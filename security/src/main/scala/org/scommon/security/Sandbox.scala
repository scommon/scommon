package org.scommon.security

object Sandbox {
  def securityManager: SecurityManager.type =
    SecurityManager

  def run[T](callback: => T)(implicit context: SecurityContext): T =
    runPrivileged[T](callback)(context)

  def run[T](classLoader: ClassLoader)(callback: => T)(implicit context: SecurityContext): T =
    runPrivileged[T](classLoader)(callback)(context)

  def runUnprivileged[T](callback: => T): T =
    SecurityManager.unprivileged(callback)

  def runPrivilegedWithoutContext[T](callback: => T): T =
    SecurityManager.privilegedWithoutContext(callback)

  def runPrivileged[T](callback: => T)(implicit context: SecurityContext): T =
    SecurityManager.privileged(callback)(context)

  def runPrivileged[T](classLoader: ClassLoader)(callback: => T)(implicit context: SecurityContext): T =
    SecurityManager.privileged(classLoader)(callback)(context)
}
