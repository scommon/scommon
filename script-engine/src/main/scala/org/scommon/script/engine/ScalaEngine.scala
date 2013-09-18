package org.scommon.script.engine

import _root_.scala.tools.nsc
import _root_.scala.collection._
import scala.reflect.io.{Directory, PlainDirectory}
import scala.reflect.internal.util.{NoPosition => SNoPosition, Position => SPosition}

import java.net.URI

import org.scommon.core._
import org.scommon.reactive._
import org.scommon.script.engine.core._

import scala.language.implicitConversions
import java.nio.file.LinkOption


object ScalaEngine extends EngineFactory[Scala] {
  val instance = this

  lazy val details = new EngineDetails[Scala] {
    val name: String     = "scala-engine"
    val title: String    = "Scala Engine"
    val version: Version = ScalaCompiler.version

    val defaultSettings = ScalaCompilerSettings()
  }

  def newEngine[U >: Scala <: CompilerSpecificSettings, T](settings: CompilerSettings[U], generator: Generator[CompilerSource[T]]): Engine[Scala] =
    new ScalaEngine[T](details, settings.asInstanceOf[CompilerSettings[Scala]], generator)
}


class ScalaEngine[T](
  val details: EngineDetails[Scala],
  val settings: CompilerSettings[Scala],
  generator: Generator[CompilerSource[T]])
extends Engine[Scala] {
  import Generator._

  private[this] val output_dir: nsc.io.AbstractFile = {
    if (settings.inMemory) {
      //Save class files to a virtual directory in memory
      new nsc.io.VirtualDirectory("mem:///stream", None)
    } else {
      //Save them to the file system.
      val absolute_path = settings.outputDirectory.toRealPath(LinkOption.NOFOLLOW_LINKS).toFile
      new PlainDirectory(new Directory(absolute_path))
    }
  }

  private[this] val compiler_settings = {
    val s = ScalaCompilerSettings.toNscSettings(settings)
    s.outputDirs.setSingleOutput(output_dir)
    s
  }

  private[this] val reporter: nsc.reporters.Reporter = new nsc.reporters.AbstractReporter() {
    val settings = compiler_settings

    def displayPrompt(): Unit = {}

    def display(pos: SPosition, msg: String, severity: Severity) {
      val m: String = SPosition.formatMessage(pos, msg, true)

      val s: CompilerMessageSeverity.EnumVal =
        if (severity == ERROR)
          CompilerMessageSeverity.Error
        else if (severity == WARNING)
          CompilerMessageSeverity.Warning
        else if (severity == INFO)
          CompilerMessageSeverity.Information
        else
          CompilerMessageSeverity.Unknown

      val p: Position =
        if (pos eq null)
          UnknownPosition
        else {
          val pos_in =
            if (pos.isDefined)
              pos.inUltimateSource(pos.source)
            else
              pos
          StandardPosition(pos_in.line, pos_in.column)
        }

      val standard_msg = StandardCompilerMessage(s, m, p)
      ScalaEngine.this.settings.handlers.messageReceived(ScalaEngine.this, standard_msg)
    }
  }

  private[this] val compiler = new ScalaCompiler(compiler_settings, reporter, Seq(new FilterForClassesImplementingTrait()) ++ settings.specific.phaseInterceptors, Some(new CompilerProgressListener {
    def progressUpdate(update: CompilerProgress):Unit =
      settings.handlers.progressUpdate(ScalaEngine.this, update)
  }))

  private[this] val pipeline = ((
    generator
    |>> waitForAll)
    >> lift_source
    >> process_source
  ).begin

  def close(): Unit =
    pipeline.cancel()

  //Extracts the CompilerSource instances from the junction data (which is what's processing the generator's
  //generated values). The reactive library gathers
  private[this] def lift_source: PartialFunction[Any, Any] = {
    case Seq(Some(t @ Seq(_*)), _*) => t
  }

  private[this] def process_source: PartialFunction[Any, Any] = { case (sources: Seq[CompilerSource[URI]] @unchecked) =>
    sources.foreach(x => println(s"Compiling ${x.source}"))
    compiler.compile(sources)

    println("Compilation complete")

    val s = mutable.Stack[nsc.io.AbstractFile]()
    s.push(output_dir)

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

  private[this] class FilterForClassesImplementingTrait extends ScalaPhaseIntercept {
    val name = "filter-classes-implementing-trait"
    override val runsBeforePhases    = List(CompilerPhase.Erasure)
    override val runsAfterPhases     = List(CompilerPhase.Typer)
    override val runsRightAfterPhase = Some(CompilerPhase.Pickler)

    /** Called when a class or module is found. */
    private[this] def callback(global: nsc.Global)(t: global.Type) = {
      import global._

      println(s"*********************** FOUND: ${t.toLongString} ${t.baseClasses}") //${t <:< typeOf[MyTestTrait]}
    }

    def intercept(global: nsc.Global)(unit: global.CompilationUnit): Unit = {
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
}
