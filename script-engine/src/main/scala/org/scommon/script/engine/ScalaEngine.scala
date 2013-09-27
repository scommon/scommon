package org.scommon.script.engine

import _root_.scala.tools.nsc
import _root_.scala.collection._
import scala.reflect.io.{Directory, PlainDirectory}
import scala.reflect.internal.util.{NoPosition => SNoPosition, Position => SPosition}

import java.nio.file.LinkOption

import org.scommon.core._
import org.scommon.reactive._
import org.scommon.script.engine.core._

import scala.language.implicitConversions


private[engine] object ScalaEngine extends EngineFactory[Scala] {
  val instance = this

  lazy val details = new EngineDetails[Scala] {
    val name: String     = "scala-engine"
    val title: String    = "Scala Engine"
    val version: Version = ScalaCompiler.version

    val defaultSettings = ScalaCompilerSettings()
  }

  def newEngine[U >: Scala <: CompilerSpecificSettings, T](settings: CompilerSettings[U], generator: Generator[CompilerSource[T], CompilerContext]): Engine[Scala, T] =
    new ScalaEngine[T](details, settings.asInstanceOf[CompilerSettings[Scala]], generator)
}


private[engine] class ScalaEngine[T](
  val details: EngineDetails[Scala],
  val settings: CompilerSettings[Scala],
  val generator: Generator[CompilerSource[T], CompilerContext])
extends Engine[Scala, T] {
  import Generator._

  private[this] val translated_settings =
    ScalaCompilerSettings.toNscSettings(settings)

  private[this] val pipeline = ((
    generator
    |>> waitForAllWithContext[CompilerContext])
    >> lift_source
    >> process_source
  ).begin

  def close(): Unit =
    pipeline.cancel()

  //Extracts the CompilerSource instances from the junction data (which is what's processing the generator's
  //generated values). We gather all data from all generators and when they all arrive, the gate opens and
  //data begins flowing to junctions.
  private[this] def lift_source: PartialFunction[(Option[CompilerContext], Any), (CompilerContext, Seq[Any])] = {
    case ((Some(c: CompilerContext), Seq(Some(t @ Seq(_*)), _*))) => (c, t)
  }

  private[this] def process_source: PartialFunction[Any, Unit] = { case ((context: CompilerContext, (sources: Seq[CompilerSource[T]] @unchecked))) =>
    sources.foreach(x => println(s"Compiling ${x.source}"))

    val output_dir: nsc.io.AbstractFile = {
      if (settings.inMemory) {
        //Save class files to a virtual directory in memory
        new nsc.io.VirtualDirectory("mem:///stream", None)
      } else {
        //Save them to the file system.
        val absolute_path = settings.outputDirectory.toRealPath(LinkOption.NOFOLLOW_LINKS).toFile
        new PlainDirectory(new Directory(absolute_path))
      }
    }

    val compiler_settings = {
      val s = translated_settings.copy()
      s.outputDirs.setSingleOutput(output_dir)
      s
    }

    val scala_type_filter =
      new ScalaTypeFilter(settings.typeFilters)

    val compiler = ScalaCompiler(compiler_settings, Seq(scala_type_filter) ++ settings.specific.phaseInterceptors) { msg =>
      context.handlers.messageReceived(this, msg)
    } { progress =>
      context.handlers.progressUpdate(this, progress)
    }

    compiler.compile(sources)

    val discovered_types: CompileResult.DiscoveredTypeMap = {
      for {
        (name, types) <- scala_type_filter.discovered
        type_filter <- scala_type_filter.typeFilters

        if type_filter.name == name
      } yield (type_filter, types.toIterable)
    }.toMap withDefaultValue Iterable()

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

    //Notify everyone that we've completed compilation.
    settings.handlers.sourceCompiled(this, CompileResult(discovered_types))
  }

  private[this] class ScalaTypeFilter(val typeFilters: Iterable[CompilerTypeFilter]) extends ScalaPhaseIntercept {
    require(typeFilters.isTraversableAgain, "typeFilters must be iterable more than once")

    val name = "filter-for-types-implementing-trait"
    override val runsBeforePhases    = List(CompilerPhase.Erasure)
    override val runsAfterPhases     = List(CompilerPhase.Typer)
    override val runsRightAfterPhase = Some(CompilerPhase.Pickler)

    val discovered = mutable.Map[String, mutable.LinkedHashSet[String]]() withDefaultValue mutable.LinkedHashSet()

    /** Called when a class or module is found. */
    def typeDiscovered(global: nsc.Global)(s: global.Symbol, t: global.Type) = {
      for {
        filter <- typeFilters
        filter_type_in_global_universe = filter.tag.in(global.rootMirror).tpe

        is_t_subtype_wrt_filter = t <:< filter_type_in_global_universe
        if is_t_subtype_wrt_filter
      }
        discovered(filter.name) = (discovered(filter.name) += t.toLongString)
    }

    def intercept(global: nsc.Global)(unit: global.CompilationUnit): Unit = {
      import global._

      //val source = unit.source.file.name

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
              typeDiscovered(global)(tree.symbol, tree.symbol.typeOfThis)
            case _ =>
          }
          super.traverse(tree)
        }
      }
      traverse.apply(unit.body)
    }
  }
}
