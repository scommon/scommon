package org.scommon

import scala.language.implicitConversions

package object security {
  val SERIALIZABLE_UNIT = new Object with Serializable

  implicit def function2SandboxCallable[T <: java.io.Serializable](fn: Function0[T]) = new SandboxCallable[T] {
    def run(): T =
      fn()
  }

  implicit def callByName2SandboxCallable[T <: java.io.Serializable](fn: => T) = new SandboxCallable[T] {
    def run(): T =
      fn
  }

  implicit def function2SandboxCallableWithUnit(fn: Function0[Unit]) = new SandboxCallable[Serializable] {
    def run(): Serializable = {
      fn()
      SERIALIZABLE_UNIT
    }
  }

  implicit def callByName2SandboxCallableWithUnit(fn: => Unit) = new SandboxCallable[Serializable] {
    def run(): Serializable = {
      fn
      SERIALIZABLE_UNIT
    }
  }
}
