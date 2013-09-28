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

    val discovered_types: CompileResult.SerializableDiscoveredTypeMap = {
      for ((name, descriptions) <- scala_type_filter.discovered)
        yield (name, descriptions.toIterable)
    }.toMap withDefaultValue Iterable()

    val discovered_entry_points =
      scala_type_filter.discoveredEntryPoints

    val descriptions =
      scala_type_filter.descriptions

    val entries = mutable.Stack[ClassEntry]()
    val s = mutable.Stack[nsc.io.AbstractFile]()
    s.push(output_dir)

    while(s.nonEmpty) {
      val next = s.pop()

      for (candidate <- next.iterator) {
        if (candidate.isDirectory) {
          s.push(candidate)
        } else {
          val output_path = output_dir.canonicalPath
          val candidate_path = candidate.canonicalPath
          val path = if (candidate_path.startsWith(output_path)) candidate_path.substring(output_path.length()) else candidate_path
          val path_ex = if (path.startsWith("/")) path.substring(1) else path

          //Lookup the corresponding description.
          descriptions.get(path_ex) match {
            case Some(description) =>
              entries push ClassEntry(description, ClassContents(candidate.toByteArray))
            case _ =>
              throw new IllegalStateException(s"Scala scripting engine was unable to locate the associated class description for $path_ex")
          }

          println(s"compiled: ${candidate.canonicalPath}\nbytes: ${candidate.toByteArray.length}")
        }
      }
    }
    s.clear()

    //Notify everyone that we've completed compilation.
    settings.handlers.sourceCompiled(this, CompileResult(ClassRegistry(entries), discovered_entry_points, discovered_types))
  }

  private[this] class ScalaTypeFilter(val typeFilters: Iterable[CompilerTypeFilter]) extends ScalaPhaseIntercept {
    require(typeFilters.isTraversableAgain, "typeFilters must be iterable more than once")

    val name = "filter-for-types-implementing-trait"
    override val runsBeforePhases    = List(CompilerPhase.Erasure)
    override val runsAfterPhases     = List(CompilerPhase.Typer)
    override val runsRightAfterPhase = Some(CompilerPhase.Pickler)

    val descriptions = mutable.HashMap[String, ClassDescription]()
    val discovered = mutable.HashMap[String, mutable.LinkedHashSet[ClassDescription]]() withDefaultValue mutable.LinkedHashSet()
    val discoveredEntryPoints = mutable.LinkedHashSet[ClassDescription]()

    /** Called when a class or module is found. */
    def typeDiscovered(global: nsc.Global)(s: global.Symbol, t: global.Type) = {
      import global._

      def addDescription(sym: global.Symbol): ClassDescription = {
        val true_java_class_name =
          ScalaCompilerUtils.trueJavaClassName(global)(sym)

        val description = ClassDescription(
            scalaClassName         = sym.fullNameString //sym.fullNameString //sym.typeOfThis.toLongString
          , javaClassName          = true_java_class_name
          , javaClassFileName      = true_java_class_name.replaceAllLiterally(".", "/") + ".class"
          , purportedJavaClassName = (if (sym.isClass || (sym.isModule && !sym.isMethod)) sym.javaBinaryName else sym.javaSimpleName).toString
        )

        //Hold on to the list of discovered classes.
        descriptions(description.javaClassFileName) = description

        description
      }

      //Create and register a class description for this symbol.
      val description =
        addDescription(s)

      //Modules also have an associated module class that should also be added.
      val module_class_description =
        if (s.isModule)
          addDescription(s.moduleClass)
        else
          null

      //Examine each incoming type and see if there's a corresponding type filter
      //defined whose tag is a supertype of the provided discovered type.
      for {
        filter <- typeFilters
        filter_type_in_global_universe = filter.tag.in(global.rootMirror).tpe

        if t <:< filter_type_in_global_universe
      }
        discovered(filter.name) = (discovered(filter.name) += description)

      //Look for a main method. Search parameters are defined as:
      //  - A module
      //  - Method named "main"
      //  - Method has exactly 1 parameter
      //  - Parameter type must be an array of strings
      if (s.isModule) {
        val type_of_array_string = typeOf[Array[String]]
        for {
          member <- t.members if member.isMethod
          method = member.asMethod if member.nameString == "main"
          params = method.paramss.flatten.map(_.typeOfThis) if !params.isEmpty && params.size == 1 && params.head =:= type_of_array_string
        }
          discoveredEntryPoints += description
      }
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
