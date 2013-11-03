package org.scommon

import scala.language.implicitConversions

package object security {
  //Normally CONTEXT_DEFAULT and CONTEXT_BASIC are the same instance but could be different
  //if an application has modified the security configuration.
  implicit def CONTEXT_DEFAULT: SecurityContext = SecurityProfile.default
  def CONTEXT_BASIC = SecurityProfile("basic")
  val CONTEXT_PERMIT_ALL = SecurityContext(permitAll = true)

  implicit def toPrivilegedExceptionAction[T](fn: => T): java.security.PrivilegedExceptionAction[T] =
    new java.security.PrivilegedExceptionAction[T] {
      def run(): T = fn
    }
}
