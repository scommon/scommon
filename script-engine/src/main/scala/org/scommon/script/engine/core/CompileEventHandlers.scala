package org.scommon.script.engine.core

/**
 * A single home to register listeners for events that can be emitted during compilation.
 */
sealed case class CompileEventHandlers(
    var messageReceived : CompileEventHandlers.MessageReceived  = CompileEventHandlers.DEFAULT_MESSAGE_RECEIVED
  , var progressUpdate  : CompileEventHandlers.ProgressUpdate   = CompileEventHandlers.DEFAULT_PROGRESS_UPDATE
  , var compileCompleted: CompileEventHandlers.CompileCompleted = CompileEventHandlers.DEFAULT_COMPILE_COMPLETED
  , var fatalError      : CompileEventHandlers.FatalError       = CompileEventHandlers.DEFAULT_FATAL_ERROR
)

object CompileEventHandlers {
  type MessageReceived  = (Engine[CompilerSpecificSettings, Any], CompileMessage)  => Unit
  type ProgressUpdate   = (Engine[CompilerSpecificSettings, Any], CompileProgress) => Unit
  type CompileCompleted = (Engine[CompilerSpecificSettings, Any], CompileResult)   => Unit
  type FatalError       = (Engine[CompilerSpecificSettings, Any], Throwable)       => Unit

  val DEFAULT_MESSAGE_RECEIVED: MessageReceived = (_, msg) => {
    msg.severity match {
      case CompileMessageSeverity.Error =>
        Console.err.println(msg.message)
      case _ =>
        Console.out.println(msg.message)
    }
  }

  val DEFAULT_FATAL_ERROR: FatalError = (_, error) => {
    Console.err.println(s"[FATAL COMPILE ERROR] $error")
  }

  val DEFAULT_PROGRESS_UPDATE: ProgressUpdate = (_, progress) => {
    //Do nothing
  }

  val DEFAULT_COMPILE_COMPLETED: CompileCompleted = (_, result) => {
    //Do nothing
  }
}
