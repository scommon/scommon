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
import java.nio.file.LinkOption
import java.nio.charset.{Charset, CharsetDecoder}
import java.nio.ByteBuffer
import _root_.scala.tools.nsc.plugins.{Plugin, PluginComponent}
import _root_.scala.tools.nsc

import org.scommon.core._
import org.scommon.script.engine.core._

import org.scommon.reactive._


@RunWith(classOf[JUnitRunner])
class ScratchTest extends FunSuite
                     with ShouldMatchers
                     with SeveredStackTraces
                     with BeforeAndAfterAll {

  val PARENT_WORKING_DIR = Path(s"scratch-test").toUserTemp

  override protected def beforeAll() {
    PARENT_WORKING_DIR.mkdirs()
  }

  override protected def afterAll() {
    PARENT_WORKING_DIR.deleteAll
  }

  test("Scratch") {
    val working = Path(PARENT_WORKING_DIR, s"${UUID.randomUUID().toString}")
    working.mkdirs()

    try {
      val workingDirectory: String = "."
      val customClassPath: Iterable[String] = Seq("a.jar", "b.jar", "c.jar")
      val working_directory: java.nio.file.Path = java.nio.file.Paths.get(workingDirectory).toRealPath(LinkOption.NOFOLLOW_LINKS).toAbsolutePath

      val my = ScalaCompilerSettings()
      my.handlers.progressUpdate = (_, progress) => {
        println(s"Progress: $progress")
      }
      my.customClassPath = customClassPath
      my.relativeDirectory = working_directory
      //my.options = Seq("-Xprint-types", "-Xshow-phases", "-Ydebug")

      //TODO: Add error/warning listener.
      val engine = Engine.newEngine[Scala](my,
        """
          |package whatever
          |trait Bar
          |case class SomeCaseClass() extends org.scommon.script.engine.MyTestTrait
          |object A {
          |  object Baz {
          |    class Foo extends Bar with org.scommon.script.engine.MyTestTrait
          |  }
          |}
        """.stripMargin
      )

      usingUnit(engine) {
        val context = Some {
          val ctx = StandardCompilerContext()
          ctx.handlers.messageReceived = (e, m) => {
            println(s"OTHER CONTEXT: ${m.message}")
          }
          ctx
        }

        //Pushing with a different context invokes its handlers.
        engine.generator.push(context, CompilerSource.fromString(
          """
            |package second
            |trait Foo
            |objectz Bar
          """.stripMargin
        ))
      }

      def error(message:String): Unit = println(s"$message")
    } finally {
      working.deleteAll should be (true)
      working.exists should be (false)
    }
  }
}

trait MyTestTrait

