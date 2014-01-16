package org.scommon.script.engine.core

object CompilerEventHandlers {
  type MessageReceived = (Engine[CompilerSpecificSettings, Any], CompilerMessage)  => Unit
  type ProgressUpdate  = (Engine[CompilerSpecificSettings, Any], CompilerProgress) => Unit
  type SourceCompiled  = (Engine[CompilerSpecificSettings, Any], CompileResult)    => Unit

  val DEFAULT_MESSAGE_RECEIVED: MessageReceived = (_, msg) => {
    msg.severity match {
      case CompilerMessageSeverity.Error => Console.err.println(msg.message)
      case _ => Console.out.println(msg.message)
    }
  }

  val DEFAULT_PROGRESS_UPDATE: ProgressUpdate = (_, progress) => {
    //Do nothing
  }

  val DEFAULT_SOURCE_COMPILED: SourceCompiled = (_, result) => {
    //Do nothing
  }
}

import CompilerEventHandlers._

/**
 * A single home to register listeners for events that can be emitted during compilation.
 */
trait CompilerEventHandlers {
  def messageReceived: MessageReceived
  def progressUpdate: ProgressUpdate
  def sourceCompiled: SourceCompiled
}

sealed case class StandardCompilerEventHandlers(
    var messageReceived: MessageReceived = DEFAULT_MESSAGE_RECEIVED
  , var progressUpdate: ProgressUpdate   = DEFAULT_PROGRESS_UPDATE
  , var sourceCompiled: SourceCompiled   = DEFAULT_SOURCE_COMPILED
) extends CompilerEventHandlers
