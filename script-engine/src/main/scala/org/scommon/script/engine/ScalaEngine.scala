package org.scommon.script.engine

import org.scommon.script.engine.core._
import org.scommon.core.Version
import org.scommon.reactive.Generator

object ScalaEngine extends EngineFactory {
  import ScalaSettings._

  val instance = this

  lazy val details = new EngineDetails {
    val name: String = "scala-engine"
    val title: String = "Scala Engine"
    val version: Version = ScalaCompiler.version

    val defaultSettings: CompilerSettings = null
  }

  def newEngine[T](settings: CompilerSettings, generator: Generator[CompilerSource[T]]): Engine =
    new ScalaEngine(settings, generator)
}

class ScalaEngine[T](val settings: ScalaSettings, generator: Generator[CompilerSource[T]]) extends Engine {

}
