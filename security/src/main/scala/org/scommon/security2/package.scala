package org.scommon

import scala.language.implicitConversions

package object security2 {
  val REQUIRED_PERMISSIONS = Permissions(
    //  new java.security.SecurityPermission("createAccessControlContext")
    //, new java.security.SecurityPermission("getProperty.package.access")
    //, new java.util.logging.LoggingPermission("control", null)
  )

  def DEFAULT_PERMISSIONS = REQUIRED_PERMISSIONS ++ Permissions()

  def PERMISSIVE_PERMISSIONS = DEFAULT_PERMISSIONS ++ Permissions()

  val CONTEXT_DEFAULT: Context = Context(grants = REQUIRED_PERMISSIONS)
  def CONTEXT_PERMISSIVE: Context = Context(grants = PERMISSIVE_PERMISSIONS)

  implicit def toPrivilegedExceptionAction[T](fn: => T): java.security.PrivilegedExceptionAction[T] =
    new java.security.PrivilegedExceptionAction[T] {
      def run(): T = fn
    }
}
