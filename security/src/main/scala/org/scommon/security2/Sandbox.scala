package org.scommon.security2

object Sandbox {

  def securityManager: SecurityManager.type =
    SecurityManager

  def run(callback: => Unit)(implicit context: Context): Unit =
    runSecurely(callback)(context)

  def runUnsecurely(callback: => Unit): Unit =
    SecurityManager.disabled(callback)

  def runSecurely(callback: => Unit)(implicit context: Context): Unit =
    SecurityManager.enabled(callback)(context)
}

class Sandbox {
  import Sandbox._
}
