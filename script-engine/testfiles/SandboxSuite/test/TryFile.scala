package test

import org.scommon.script.engine.TestClassTrait
import java.io.File

class TryFile extends TestClassTrait {
	def result = {
		val a=A("hi")
		println(a.name)
		val f=new File("/tmp")
		if(f.isDirectory) "directory" else "file"
	}
}
