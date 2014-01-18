package org.scommon.script.engine.core

/**
 * A single home to register listeners for events that can be emitted during compilation.
 */
sealed case class CompileEventHandlers(
    var messageReceived: CompileEventHandlers.MessageReceived = CompileEventHandlers.DEFAULT_MESSAGE_RECEIVED
  , var progressUpdate : CompileEventHandlers.ProgressUpdate  = CompileEventHandlers.DEFAULT_PROGRESS_UPDATE
  , var sourceCompiled : CompileEventHandlers.SourceCompiled  = CompileEventHandlers.DEFAULT_SOURCE_COMPILED
)

object CompileEventHandlers {
  type MessageReceived = (Engine[CompilerSpecificSettings, Any], CompileMessage)  => Unit
  type ProgressUpdate  = (Engine[CompilerSpecificSettings, Any], CompileProgress) => Unit
  type SourceCompiled  = (Engine[CompilerSpecificSettings, Any], CompileResult)   => Unit

  val DEFAULT_MESSAGE_RECEIVED: MessageReceived = (_, msg) => {
    msg.severity match {
      case CompileMessageSeverity.Error => Console.err.println(msg.message)
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
