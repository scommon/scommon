package org.scommon.security

import net.datenwerke.sandbox.SandboxContext

trait SandboxProfile {
  def name: String
  def context: SandboxContext
  def copyProfile(): SandboxProfile
}

case class StandardSandboxProfile(name: String, context: SandboxContext) extends SandboxProfile {
  def copyProfile(): SandboxProfile =
    copy()
}