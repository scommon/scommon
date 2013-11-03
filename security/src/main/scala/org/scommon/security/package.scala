package org.scommon

import scala.language.implicitConversions

package object security {
  implicit def CONTEXT_DEFAULT: SecurityContext = SecurityProfile.default

  implicit def toPrivilegedExceptionAction[T](fn: => T): java.security.PrivilegedExceptionAction[T] =
    new java.security.PrivilegedExceptionAction[T] {
      def run(): T = fn
    }
}
