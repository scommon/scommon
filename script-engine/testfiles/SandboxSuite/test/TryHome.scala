package test

import org.scommon.script.engine.TestClassTrait
import java.io.File

class TryHome extends TestClassTrait {
	def result = {
		val f=new File("/home")
		if(f.isDirectory) "directory" else "file"
	}
}
