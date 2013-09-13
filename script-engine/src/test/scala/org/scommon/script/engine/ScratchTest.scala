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

      //TODO: Add error/warning listener.
      val default = Engine.newEngine(
        """
          |package whatever
          |trait Bar
          |object A {
          |  object Baz {
          |    class Foo extends Bar with org.scommon.script.engine.MyTestTrait
          |  }
          |}
        """.stripMargin
      )

      def error(message:String): Unit = println(s"$message")

      val output = new VirtualDirectory("mem:///stream", None)
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

      val inter = new Foo2()
      val compiler = new ScalaCompiler(settings, reporter, Seq(inter), Some(new CompilerProgressListener {
        def progressUpdate(update: CompilerProgress):Unit = println(update)
      }))

      import Generator._

      val wait_for_all_generators = waitForAll

      //Extracts the CompilerSource instances from the junction data (which is what's processing the generator's
      //generated values). The reactive library gathers
      def lift_source: PartialFunction[Any, Any] = {
        case Seq(Some(t @ Seq(_*)), _*) => t
      }

      def process_source: PartialFunction[Any, Any] = { case (sources: Seq[CompilerSource[URI]] @unchecked) =>
        sources.foreach(x => println(s"Compiling ${x.source}"))
        compiler.compile(sources)

        println("Compilation complete")

        val s = mutable.Stack[AbstractFile]()
        s.push(output)

        while(s.nonEmpty) {
          val next = s.pop()

          for (candidate <- next.iterator) {
            if (candidate.isDirectory) {
              s.push(candidate)
            } else {
              val array = candidate.toByteArray
              println(s"compiled: ${candidate.canonicalPath}\nbytes: ${array.length}")
            }
          }
        }
      }

      val generator = ((CompilerSourceGenerator.fromStrings(
        """
          |package whatever
          |trait Bar
          |object A {
          |  object Baz {
          |    class Foo extends Bar with org.scommon.script.engine.MyTestTrait
          |  }
          |}
        """.stripMargin
        )
        |>> wait_for_all_generators)
        >> lift_source
        >> process_source
      ).begin

//      val is = new ByteArrayInputStream("package whatever; trait Bar; object A { object Baz { class Foo extends Bar with org.scommon.script.engine.MyTestTrait } } \n".getBytes("UTF-8"))
//      val sf = new ScalaSourceStream(is)
//
//      compiler.compile(sf)


      //val vd: VirtualDirectory = new VirtualDirectory("d")
      //r.compileFiles(List(sf))
    } finally {
      working.deleteAll should be (true)
      working.exists should be (false)
    }
  }
}

import _root_.scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

trait MyTestTrait

class Foo2 extends ScalaPhaseIntercept {
  val name = "foo2"
  override val runsBeforePhases    = List(CompilerPhase.Erasure)
  override val runsAfterPhases     = List(CompilerPhase.Typer)
  override val runsRightAfterPhase = Some(CompilerPhase.Pickler)

  /** Called when a class or module is found. */
  private[this] def callback(global: Global)(t: global.Type) = {
    import global._

    println(s"*********************** FOUND: ${t.toLongString} ${t.baseClasses} ${t <:< typeOf[MyTestTrait]}")
  }

  def intercept(global: Global)(unit: global.CompilationUnit): Unit = {
    import global._

    val source = unit.source.file.name
    println(s"traversing $source")

    val traverse = new global.Traverser {
      import _root_.scala.reflect.internal._

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
          case _: ClassDef | _: ModuleDef
            if isDefined(tree.symbol) && (isClass(tree.symbol) || isModule(tree.symbol)) =>
            callback(global)(tree.symbol.typeOfThis)
          case _ =>
        }
        super.traverse(tree)
      }
    }
    traverse.apply(unit.body)
  }
}

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