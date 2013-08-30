package test
import org.scommon.script.engine.TestClassTrait

/**
 * @author kostantinos.kougios
 *
 * 2 Jan 2012
 */
class FromClasspathFirst(v: Int) extends TestClassTrait {
	def this() = this(5)
	def result = "fcf:%d".format(v)
}