package org.scommon.script.engine

import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import java.util.UUID

import org.scommon.io._
import Utils._
import scala.tools.nsc.{SubComponent, Phase, Settings, Global}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.io.AbstractFile
import java.net.{URLClassLoader, URI}
import java.io._
import scala.reflect.io.{Streamable, NoAbstractFile}
import scala.collection._
import scala.tools.util.PathResolver
import java.nio.file.LinkOption
import java.nio.charset.{Charset, CharsetDecoder}
import java.nio.ByteBuffer
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc


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

      def error(message:String): Unit = println(s"$message")

      val output = new VirtualDirectory("mem/output", None)
      val settings:Settings = new Settings(error)
      settings.outdir.value = working.getAbsolutePath

      // save class files to a virtual directory in memory
      settings.outputDirs.setSingleOutput(output)

      settings.deprecation.value = true
      settings.unchecked.value = true
      settings.feature.value = true
      settings.elidebelow.value = 900
      settings.printtypes.value = true

      settings.classpath.value = s"${PathResolver.Environment.javaUserClassPath}"
      settings.bootclasspath.value = s"${PathResolver.Environment.javaUserClassPath}"

      //Resolve elements in the custom class path relative to the specified working
      //directory. We'll later prepend in reverse order in order for them to appear
      //on the class path in the correct order.
      val resolved = (
        for {
          custom <- customClassPath
          absolute_path = working_directory.resolve(custom).toFile.getAbsolutePath
        } yield absolute_path
      ).toSeq

      for(element <- resolved.reverse)
        settings.classpath.prepend(element)

      val reporter:Reporter = new ConsoleReporter(settings)
      val compiler = new Global(settings, reporter) {
        override protected def computeInternalPhases () {
          super.computeInternalPhases
          val phase = new Foo(this)
          addToPhasesSet(phase, phase.phaseName)
        }
      }

      val r = new compiler.Run {
        override def progress(current: Int, total: Int) {
          super.progress(current, total)
          println(s"curr: $current, total: $total")
        }
      }

      val is = new ByteArrayInputStream("package whatever; trait Bar; object A { object Baz { class Foo extends Bar with org.scommon.script.engine.MyTestTrait } } \n".getBytes("UTF-8"))
      val sf = new ScalaSourceStream(is)

      r.compileFiles(List(sf))


      //val vd: VirtualDirectory = new VirtualDirectory("d")
      //r.compileFiles(List(sf))
    } finally {
      working.deleteAll should be (true)
      working.exists should be (false)
    }
  }
}

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

trait MyTestTrait

class Foo(val global: Global) extends SubComponent {
  import global._

  val phaseName = "foo"
  val name = phaseName
  val runsAfter = List("typer")
  val runsRightAfter = Some("pickler")
  override val runsBefore = List("erasure")

  /** Called when a class is found. */
  private[this] def `class`(s: ClassSymbol) = {
    process(s.typeOfThis)
  }

  /** Called when a module is found. */
  private[this] def `module`(s: ModuleSymbol) = {
    process(s.typeOfThis)
  }

  private[this] def process(t: Type) = {
    println(s"*********************** FOUND: ${t.toLongString} ${t.baseClasses} ${t <:< typeOf[MyTestTrait]}")
  }

  def newPhase(prev: Phase) = new Phase(prev) {
    def name = phaseName
    def run(): Unit = {
      //Only look for units that are compiling Scala.
      for {
        unit <- currentRun.units
        if !unit.isJava
      } processScalaUnit(unit)
    }

    private[this] def processScalaUnit(unit: CompilationUnit) = {
      val source = unit.source.file.name
      println(s"traversing $source")

      val traverse = new Traverser {
        import scala.reflect.internal._

        def isTopLevel(sym: Symbol): Boolean =
          (sym ne null) &&
          (sym != NoSymbol) &&
          !sym.isImplClass &&
          !sym.isNestedClass &&
          sym.isStatic &&
          !sym.hasFlag(Flags.SYNTHETIC) &&
          !sym.hasFlag(Flags.JAVA)

        def isDefined(sym: Symbol): Boolean =
          (sym ne null) &&
          (sym != NoSymbol)

        def isClass(sym: Symbol): Boolean =
          sym.isClass

        def isModule(sym: Symbol): Boolean =
          sym.isModule

        override def traverse(tree: Tree) = {
          tree match {
            case _: ClassDef | _ : ModuleDef
              if isDefined(tree.symbol) =>
              if (isClass(tree.symbol))
                `class`(tree.symbol.asClass)
              else if (isModule(tree.symbol))
                `module`(tree.symbol.asModule)
            case _ =>
          }
          super.traverse(tree)
        }
      }
      traverse.apply(unit.body)
    }
  }
}
//
//  class DivByZero(val global: Global) extends Plugin {
//    import global._
//
//    val name = "divbyzero"
//    val description = "checks for division by zero"
//    val components = List[PluginComponent](Component)
//
//    private object Component extends PluginComponent {
//      val global: DivByZero.this.global.type = DivByZero.this.global
//
//      val runsAfter = "refchecks"
//
//      // Using the Scala Compiler 2.8.x the runsAfter should be written as below
//      // val runsAfter = List[String]("refchecks");
//      val phaseName = DivByZero.this.name
//      def newPhase(_prev: Phase) = new DivByZeroPhase(_prev)
//
//      class DivByZeroPhase(prev: Phase) extends StdPhase(prev) {
//        override def name = DivByZero.this.name
//        def apply(unit: CompilationUnit) {
//          for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
//                if rcvr.tpe <:< definitions.IntClass.tpe)
//          {
//            unit.error(tree.pos, "definitely division by zero")
//          }
//        }
//      }
//    }
//  }

class ScalaSourceStream(uri: URI, stream: InputStream, charset: Charset = org.scommon.io.DEFAULT_CHARSET) extends AbstractFile {
  def this(file: File) = this(file.toURI, file.openForRead)
  def this(stream: InputStream) = this(URI.create(s"mem:///stream/${UUID.randomUUID().toString}.scala"), stream)

  /** Returns contents of file (if applicable) in a Char array.
    *  warning: use <code>Global.getSourceFile()</code> to use the proper
    *  encoding when converting to the char array.
    */
  @throws(classOf[IOException])
  override def toCharArray = new String(toByteArray, charset).toCharArray

  /** Returns contents of file (if applicable) in a byte array.
    */
  @throws(classOf[IOException])
  override def toByteArray: Array[Byte] = Streamable.bytes(input)

  def input = stream

  val path = Option(uri.getPath) getOrElse ""

  val name = {
    val p = path
    val idx = p.lastIndexOf('/')
    if (idx >= 0) p.substring(idx + 1) else ""
  }

  override def hashCode() = uri.hashCode()
  override def equals(obj: Any) = uri.equals(obj)

  private[this] var last_modified = 0L
  def lastModified: Long = last_modified
  def lastModified_=(x: Long) = last_modified = x

  def absolute = this
  def container = NoAbstractFile
  def create() = unsupported()
  val file = null
  val isDirectory = false
  def delete() = unsupported()
  def output = unsupported()
  def iterator = Iterator.empty
  def lookupName(name: String, directory: Boolean) = null
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported()
}