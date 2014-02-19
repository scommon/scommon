package whatever.blah

trait Bar

//isAbstractType: false
//isAbstractClass: false
//isTrait: false
//isClass: true
//isCaseClass: false
//isModule: false
//isModuleClass: false
class NormalClass(arg1: String) extends org.scommon.script.engine.MyTestTrait

//isAbstractType: false
//isAbstractClass: true
//isTrait: true
//isClass: true
//isCaseClass: false
//isModule: false
//isModuleClass: false
trait ATrait extends org.scommon.script.engine.MyTestTrait

//isAbstractType: false
//isAbstractClass: true
//isTrait: false
//isClass: true
//isCaseClass: false
//isModule: false
//isModuleClass: false
abstract class AbstractClass extends org.scommon.script.engine.MyTestTrait

//isAbstractType: false
//isAbstractClass: false
//isTrait: false
//isClass: true
//isCaseClass: false
//isModule: false
//isModuleClass: false
case object ACaseObject extends org.scommon.script.engine.MyTestTrait

//isAbstractType: false
//isAbstractClass: false
//isTrait: false
//isClass: true
//isCaseClass: true
//isModule: false
//isModuleClass: false
case class SomeCaseClass(var wha: String, var dude: Int) extends org.scommon.script.engine.MyTestTrait

case class SomeCaseClassWithMultipleParameterLists(var wha: String)(var dude: Int) extends Bar with org.scommon.script.engine.MyTestTrait {
  override def toString = s"SomeCaseClassWithMultipleParameterLists($wha)($dude)"
}

object A {
  object Baz extends org.scommon.script.engine.MyTestTrait {
    class Foo extends Bar with org.scommon.script.engine.MyTestTrait
    def main(args: Array[String]) {
      println("Hello, world! " + args.toList)
      //println(System.getProperty("path.separator"))
      //new java.io.File("${PathUtil.queryApplicationDirectory.replaceAllLiterally("\\", "\\\\")}").listFiles.foreach(println)
      //new java.io.File(".").listFiles.foreach(println)
      import scala.sys.process._
      Seq("echo", "Hi there!").!
    }
  }
}