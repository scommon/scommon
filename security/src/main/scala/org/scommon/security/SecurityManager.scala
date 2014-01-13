package org.scommon.security

import java.security.{AccessController, PrivilegedAction}
import java.util.concurrent.atomic.AtomicReference

object SecurityManager {
  /**
   * Removes any existing [[java.lang.SecurityManager]] using [[java.lang.System#setSecurityManager]].
   *
   * @return the previous [[java.lang.SecurityManager]] if one was installed
   */
  def uninstall(): Option[java.lang.SecurityManager] =
    install(null: java.lang.SecurityManager)

  def installDefault(): Option[java.lang.SecurityManager] =
    install(new java.lang.SecurityManager())

  def install(securityManager: Option[java.lang.SecurityManager]): Option[java.lang.SecurityManager] =
    if (securityManager.isDefined)
      install(securityManager.get)
    else
      uninstall()

  /**
   * Installs a new [[java.lang.SecurityManager]] using [[java.lang.System#setSecurityManager]].
   *
   * @param securityManager instance of a [[java.lang.SecurityManager]] to install
   * @return the previous [[java.lang.SecurityManager]] if one was installed
   */
  def install(securityManager: java.lang.SecurityManager): Option[java.lang.SecurityManager] = synchronized {
    val previous = current
    System.setSecurityManager(securityManager)
    previous
  }

  def current: Option[java.lang.SecurityManager] =
    Option(System.getSecurityManager)

  def isAnyInstalled: Boolean =
    current.isDefined

  def unprivileged[T](callback: => T): T =
    callback

  def privilegedWithoutContext[T](callback: => T): T = {
    AccessController.doPrivileged[T](new PrivilegedAction[T] {
      def run(): T =
        callback
    }, CONTEXT_PERMIT_ALL.toAccessControlContext(Thread.currentThread().getContextClassLoader))
  }

  def privileged[T](callback: => T)(implicit context: SecurityContext): T =
    privileged[T](null: ClassLoader)(callback)(context)

  def privileged[T](classLoader: ClassLoader)(callback: => T)(implicit context: SecurityContext): T = {
    AccessController.doPrivileged[T](new PrivilegedAction[T] {
      def run(): T =
        callback
    }, context.toAccessControlContext(if (classLoader eq null) Thread.currentThread().getContextClassLoader else classLoader))
  }

  def privilegedContextClassLoader: ClassLoader =
    AccessController.doPrivileged[ClassLoader](toPrivilegedExceptionAction({
        Thread.currentThread().getContextClassLoader
    }))
}
