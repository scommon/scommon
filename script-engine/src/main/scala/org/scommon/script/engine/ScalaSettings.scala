package org.scommon.script.engine

import org.scommon.script.engine.core.CompilerSettings

object ScalaSettings {
  implicit def compilerSettings2ScalaSettings(settings: CompilerSettings): ScalaSettings = new ScalaSettings()
}

class ScalaSettings() extends CompilerSettings {

}
