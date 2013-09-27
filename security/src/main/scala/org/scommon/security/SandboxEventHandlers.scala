package org.scommon.security

object SandboxEventHandlers {
  type ExceptionReceived = (Sandbox, Throwable) => Unit

  val DEFAULT_EXCEPTION_RECEIVED: ExceptionReceived = (_, t) => {
    t.printStackTrace(Console.err)
  }
}

import SandboxEventHandlers._

trait SandboxEventHandlers {
  def exceptionReceived: ExceptionReceived
}

sealed case class MutableSandboxEventHandlers(
    var exceptionReceived: ExceptionReceived = DEFAULT_EXCEPTION_RECEIVED
) extends SandboxEventHandlers
