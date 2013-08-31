package test

import org.scommon.script.engine.TestClassTrait

class TryPackage extends TestClassTrait {
	def result = {
		classOf[javax.swing.Icon].getName
	}
}
