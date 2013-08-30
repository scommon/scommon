package org.scommon.script.engine.core

import scala.collection._

import scala.language.implicitConversions

trait SourceListener {
  def newSourceAvailable(generator: SourceGenerator): Unit
}

trait SourceGenerator { self =>
  private[this] val listeners = mutable.HashSet[SourceListener]()

  def addListener(listener: SourceListener): Unit = synchronized {
    listeners += listener
  }

  def removeListener(listener: SourceListener): Unit = synchronized {
    listeners -= listener
  }

  //protected def notifyListeners = new java.io.FileInputStream().to
}

object SourceGenerator {
  type SourceGeneratedEvent = (SourceGenerator) => Unit

  implicit def sourceGeneratedEvent2SourceListener(callback: SourceGeneratedEvent) =
    new SourceListener {
      def newSourceAvailable(generator: SourceGenerator): Unit = callback(generator)
    }
}


