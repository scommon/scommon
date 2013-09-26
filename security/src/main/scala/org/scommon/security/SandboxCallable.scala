package org.scommon.security

trait SandboxCallable[+T <: java.io.Serializable] extends java.io.Serializable {
  def run(): T
}
