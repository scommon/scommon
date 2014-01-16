package whatever.blah

trait Bar

case class SomeCaseClass() extends org.scommon.script.engine.MyTestTrait

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