package org.scommon.security2

import java.security._
import java.util.concurrent.locks.ReentrantLock
import org.scommon.security2.wildfly.WildFlySecurityManager

object SecurityManager {
  def isAnyInstalled: Boolean =
    System.getSecurityManager ne null

  def disabled[T](callback: => T): T = {
    callback
  }

  def enabled[T](callback: => T)(implicit context: Context): T = {
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
