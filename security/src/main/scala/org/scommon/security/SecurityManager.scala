package org.scommon.security

import java.security.{AccessController, PrivilegedAction}

object SecurityManager {
  def isAnyInstalled: Boolean =
    System.getSecurityManager ne null

  def unprivileged[T](callback: => T): T = {
    callback
  }

  def privilegedWithoutContext[T](callback: => T): T = {
    AccessController.doPrivileged[T](new PrivilegedAction[T] {
      def run(): T = {
        callback
      }
    }, CONTEXT_PERMIT_ALL.toAccessControlContext(Thread.currentThread().getContextClassLoader))
  }

  def privileged[T](callback: => T)(implicit context: SecurityContext): T =
    privileged[T](null.asInstanceOf[ClassLoader])(callback)(context)

  def privileged[T](classLoader: ClassLoader)(callback: => T)(implicit context: SecurityContext): T = {
    AccessController.doPrivileged[T](new PrivilegedAction[T] {
      def run(): T = {
        callback
      }
    }, context.toAccessControlContext(if (classLoader eq null) Thread.currentThread().getContextClassLoader else classLoader))
  }

  def privilegedContextClassLoader: ClassLoader =
    AccessController.doPrivileged[ClassLoader](toPrivilegedExceptionAction({
        Thread.currentThread().getContextClassLoader
    }))
}
