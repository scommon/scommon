package org.scommon.script.engine

import org.scommon.script.engine.core._
import org.scommon.core.Version
import org.scommon.reactive.Generator

import scala.language.implicitConversions

object ScalaEngine extends EngineFactory[Scala] {
  val instance = this

  lazy val details = new EngineDetails[Scala] {
    val name: String     = "scala-engine"
    val title: String    = "Scala Engine"
    val version: Version = ScalaCompiler.version

    val defaultSettings = new CompilerSettings[Scala] {
      def compiler: Scala =
        Scala.defaults
    }
  }

  def newEngine[U >: Scala, T](settings: CompilerSettings[U], generator: Generator[CompilerSource[T]]): Engine[U] =
    new ScalaEngine[U, T](details, settings, generator)
}

class ScalaEngine[U >: Scala, T](
  val details: EngineDetails[U],
  val settings: CompilerSettings[U],
  generator: Generator[CompilerSource[T]])
extends Engine[U] {

}
