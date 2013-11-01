package org.scommon.security2

import java.security.{AccessController, PrivilegedAction}

object SecurityManager {
  def isAnyInstalled: Boolean =
    System.getSecurityManager ne null

  def unprivileged[T](callback: => T): T = {
    callback
  }

  def privileged[T](callback: => T)(implicit context: SecurityContext): T = {
    AccessController.doPrivileged[T](new PrivilegedAction[T] {
      def run(): T = {
        callback
      }
    }, context.toAccessControlContext())
  }

  def privilegedContextClassLoader: ClassLoader =
    AccessController.doPrivileged[ClassLoader](toPrivilegedExceptionAction({
        Thread.currentThread().getContextClassLoader
    }))
}
