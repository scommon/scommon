package org.scommon.script.engine

import _root_.scala.Some
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import java.util.UUID

import org.scommon.io._
import Utils._
import _root_.scala.tools.nsc.{SubComponent, Phase, Settings, Global}
import _root_.scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import _root_.scala.tools.nsc.io.VirtualDirectory
import _root_.scala.tools.nsc.io.AbstractFile
import java.net.{URLClassLoader, URI}
import java.io._
import _root_.scala.reflect.io.{Streamable, NoAbstractFile}
import _root_.scala.collection._
import _root_.scala.tools.util.PathResolver
import java.nio.file.{Paths, LinkOption}
import java.nio.charset.{Charset, CharsetDecoder}
import java.nio.ByteBuffer
import _root_.scala.tools.nsc.plugins.{Plugin, PluginComponent}
import _root_.scala.tools.nsc

import org.scommon.core._
import org.scommon.script.engine.core._

import org.scommon.reactive._
import org.scommon.security
import org.scommon.security.Sandbox


@RunWith(classOf[JUnitRunner])
class ScratchTest extends FunSuite
                     with ShouldMatchers
                     with SeveredStackTraces
                     with BeforeAndAfterAll {

  val PARENT_WORKING_DIR = Path(s"scratch-test").toUserTemp

  override protected def beforeAll() {
    privileged {
      PARENT_WORKING_DIR.mkdirs()
    }
  }

  override protected def afterAll() {
    privileged {
      PARENT_WORKING_DIR.deleteAll
    }
  }

  def privileged(fn: => Unit): Unit = {
    org.scommon.security.SecurityManager.privilegedWithoutContext(fn)
  }

  def privilegedWithSecurityManager(fn: => Unit): Unit = {
    org.scommon.security.SecurityManager.privilegedWithoutContext({
      val old = System.getSecurityManager
      System.setSecurityManager(new java.lang.SecurityManager())
      try {
        fn
      } finally {
        System.setSecurityManager(old)
      }
    })
  }

  test("Scratch") {
    val working = Path(PARENT_WORKING_DIR, s"${UUID.randomUUID().toString}")
    working.mkdirs()

    privilegedWithSecurityManager {
      try {
        val workingDirectory: String = "."
        val customClassPath: Iterable[String] = Seq("a.jar", "b.jar", "c.jar")
        val working_directory: java.nio.file.Path = java.nio.file.Paths.get(workingDirectory).toRealPath(LinkOption.NOFOLLOW_LINKS).toAbsolutePath

        val my = ScalaCompilerSettings()
        //my.inMemory = false
        //my.outputDirectory = Paths.get(working.getAbsolutePath, s"${UUID.randomUUID().toString}")
        //my.outputDirectory.toFile.mkdirs()

        my.handlers.progressUpdate = (_, progress) => {
          println(s"Progress: $progress")
        }

        my.handlers.sourceCompiled = (_, result) => {
          import scala.reflect.runtime.universe
          import scala.reflect.runtime.universe._

          {
            //Execute outside the sandbox
            val cl = result.classRegistry.toClassLoader()
            val runtime_mirror = universe.runtimeMirror(cl)

            for {
              entry_point <- result.entryPoints
              //cls = Class.forName(entry_point.javaClassName, false, cl)
              module    = runtime_mirror.staticModule(entry_point.scalaClassName)
              obj       = runtime_mirror.reflectModule(module)
              reflected = runtime_mirror.reflect(obj.instance)
              method    = obj.symbol.typeSignature.member(newTermName("main")).asMethod
              main      = reflected.reflectMethod(method)
            } {
              //println(cls)
              Sandbox.run(cl) {
                main(Array[String]())
              }(org.scommon.security.CONTEXT_DEFAULT)
            }
          }


  //        result.unitRunInSandbox(_.context.setDebug(false)) { data =>
  //          import org.scommon.core.ThreadUtil._
  //          val thread = new Thread(new Runnable {
  //            def run(): Unit = {
  //              //Load entry points and execute them.
  //
  //              val runtime_mirror = universe.runtimeMirror(data.classLoader)
  //
  //              for {
  //                entry_point <- data.entryPoints
  //                //cls = Class.forName(entry_point.javaClassName, false, Thread.currentThread().getContextClassLoader())
  //                module    = runtime_mirror.staticModule(entry_point.scalaClassName)
  //                obj       = runtime_mirror.reflectModule(module)
  //                reflected = runtime_mirror.reflect(obj.instance)
  //                method    = obj.symbol.typeSignature.member(newTermName("main")).asMethod
  //                main      = reflected.reflectMethod(method)
  //              } {
  //                //println(cls)
  //                main(Array[String]())
  //              }
  //            }
  //          })
  //
  //          thread.start()
  //          thread.join()
  //
  //        }
        }

        my.customClassPath = customClassPath
        my.relativeDirectory = working_directory
        my.withTypeFilter[MyTestTrait]

        //my.options = Seq("-Xprint-types", "-Xshow-phases", "-Ydebug")

        //TODO: Add error/warning listener.
        val engine = Engine.newEngine[Scala](my,
          s"""
            |package whatever.blah
            |trait Bar
            |case class SomeCaseClass() extends org.scommon.script.engine.MyTestTrait
            |object A {
            |  object Baz extends org.scommon.script.engine.MyTestTrait {
            |    class Foo extends Bar with org.scommon.script.engine.MyTestTrait
            |    def main(args: Array[String]) {
            |      println("Hello, world! " + args.toList)
            |      //println(System.getProperty("path.separator"))
            |      //new java.io.File("${PathUtil.queryApplicationDirectory.replaceAllLiterally("\\", "\\\\")}").listFiles.foreach(println)
            |      //new java.io.File(".").listFiles.foreach(println)
            |      //import scala.sys.process._
            |      //Seq("echo", "Hi!").!
            |    }
            |  }
            |}
          """.stripMargin,
          """
            |package abc
            |object Qux {
            |    def main(args: Array[String]) {
            |      println("Hello, world! Qux")
            |    }
            |}
          """.stripMargin
        )

        usingUnit(engine) {

          val separate_context = StandardCompilerContext()
          separate_context.handlers.messageReceived = (e, m) => {
            println(s"OTHER CONTEXT: ${m.message}")
          }

          //Pushing with a different context invokes its handlers.
          engine.push(separate_context, CompilerSource.fromString(
            """
              |package second
              |trait Foo
              |objectz Bar
            """.stripMargin
          ))
        }
      } finally {
        working.deleteAll should be (true)
        working.exists should be (false)
      }
    }
  }
}

trait MyTestTrait

