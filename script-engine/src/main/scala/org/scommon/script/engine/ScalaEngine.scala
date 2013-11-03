package org.scommon.script.engine

import _root_.scala.tools.nsc
import _root_.scala.collection._
import scala.reflect.io.{Directory, PlainDirectory}
import scala.reflect.internal.util.{NoPosition => SNoPosition, Position => SPosition}

import java.nio.file.{Paths, LinkOption}

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

    val class_filter_class_descriptions =
      mutable.HashMap[String, ClassDescription]()

    val class_filter_discovered_types =
      mutable.HashMap[String, mutable.LinkedHashSet[ClassDescription]]()

    val class_filter_discovered_entry_points =
      mutable.LinkedHashSet[ClassDescription]()

    val class_filters =
      createStandardClassFilterPhaseIntercepts(
          settings.typeFilters
        , class_filter_class_descriptions
        , class_filter_discovered_types
        , class_filter_discovered_entry_points
      )

    val compiler = ScalaCompiler(compiler_settings, class_filters ++ settings.specific.phaseInterceptors) { msg =>
      context.handlers.messageReceived(this, msg)
    } { progress =>
      context.handlers.progressUpdate(this, progress)
    }

    compiler.compile(sources)

    val transformed_discovered_types: CompileResult.SerializableDiscoveredTypeMap = {
      for ((name, descriptions) <- class_filter_discovered_types)
        yield (name, descriptions.toIterable)
    }.toMap withDefaultValue Iterable()

    //The idea here is to locate the bytes associated with found class descriptions.
    //Go through the descriptions and find the associated class files.

    val entries = mutable.Stack[ClassEntry]()

    for ((_, description) <- class_filter_class_descriptions) {
      //Walk the virtual directories and find the associated class files by splitting the .javaClassFileName by
      //a forward slash and then starting at the output_dir, walk forward until we find the class file.
      val split = description.javaClassFileName.split('/').zipWithIndex
      val last = if (split.length > 0) split.length - 1 else 0
      val compiled_class = split.foldLeft(output_dir){ case (dir, (nxt, idx)) => dir.lookupPath(nxt, idx < last) }

      //It's possible that classes will show up in the list of classes but not have a backing class file. If that's the case, then
      //just skip it. I'm not exactly sure why this is the case.

      //if (compiled_class eq null) {
      //  throw new IllegalStateException(s"Scala scripting engine was unable to locate the associated compiled class for ${description.javaClassFileName}")
      //}

      if (compiled_class ne null) {
        entries push ClassEntry(description, ClassContents(compiled_class.sizeOption.getOrElse(0))(compiled_class.toByteArray))
      }
    }

    //Notify everyone that we've completed compilation.
    settings.handlers.sourceCompiled(this, CompileResult(class_filter_discovered_entry_points, transformed_discovered_types, ClassRegistry(entries)))
  }

  private[this] trait ClassFilter {
    def apply(global: nsc.Global)(s: global.Symbol, t: global.Type, description: ClassDescription): Unit
  }

  private[this] def createStandardClassFilterPhaseIntercepts(
    typeFilters: Iterable[CompilerTypeFilter],
    descriptions: mutable.HashMap[String, ClassDescription],
    discoveredTypes: mutable.HashMap[String, mutable.LinkedHashSet[ClassDescription]],
    discoveredEntryPoints: mutable.LinkedHashSet[ClassDescription]
  ) = {
    val discovered = discoveredTypes withDefaultValue mutable.LinkedHashSet()

    def class_filter(global: nsc.Global)(s: global.Symbol, t: global.Type, description: ClassDescription): Unit = {
      import global._

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

    //Creates 2 phase intercepts that look for classes created at multiple points in the compilation process.
    //This is b/c the scala compiler produces different .class files at different points and we'll need to
    //log each of them for later use.
    val extra_filter_pass = new ScalaClassFilter(
        descriptions        = descriptions
      , name                = "extra-filter-pass"
      , runsAfterPhases     = List(CompilerPhase.LambdaLift)
      , runsRightAfterPhase = Some(CompilerPhase.LambdaLift)
      , runsBeforePhases    = List(CompilerPhase.Mixin)
    )

    val type_filter_pass = new ScalaClassFilter(
        descriptions        = descriptions
      , name                = "type-filter-pass"
      , runsAfterPhases     = List(CompilerPhase.Typer)
      , runsRightAfterPhase = Some(CompilerPhase.Pickler)
      , runsBeforePhases    = List(CompilerPhase.Erasure)
      , classFilter       = new ClassFilter {
        def apply(global: nsc.Global)(s: global.Symbol, t: global.Type, description: ClassDescription): Unit =
          class_filter(global)(s, t, description)
      }
    )

    Seq(type_filter_pass, extra_filter_pass)
  }

  private[this] class ScalaClassFilter(
               val descriptions: mutable.HashMap[String, ClassDescription]
    ,          val name: String
    , override val runsBeforePhases: Iterable[CompilerPhase.EnumVal]
    , override val runsAfterPhases: Iterable[CompilerPhase.EnumVal]
    , override val runsRightAfterPhase: Option[CompilerPhase.EnumVal]
    ,              classFilter: ClassFilter = null
  ) extends ScalaPhaseIntercept {
    /** Called when a class or module is found. */
    def classOrModuleDiscovered(global: nsc.Global)(s: global.Symbol, t: global.Type) = {
      import global._

      def addDescription(sym: global.Symbol): ClassDescription = {
        val true_java_class_name =
          ScalaCompilerUtils.trueJavaClassName(global)(sym)

        val description = ClassDescription(
            scalaClassName         = sym.fullNameString //sym.fullNameString //sym.typeOfThis.toLongString
          , javaClassName          = true_java_class_name
          , javaClassFileName      = true_java_class_name.replaceAllLiterally(".", "/") + ".class"
          , purportedJavaClassName = (if (sym.isClass || (sym.isModule && !sym.isMethod)) sym.javaClassName else sym.javaSimpleName).toString
        )

        //Hold on to the list of discovered classes.
        if (!descriptions.contains(description.javaClassFileName))
          descriptions(description.javaClassFileName) = description

        description
      }

      //Create and register a class description for this symbol.
      val description =
        addDescription(s)

      //Modules also have an associated module class that should also be added.
      if (s.isModule)
        addDescription(s.moduleClass)

      if (classFilter ne null) {
        classFilter(global)(s, t, description)
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
              classOrModuleDiscovered(global)(tree.symbol, tree.symbol.typeOfThis)
            case _ =>
          }
          super.traverse(tree)
        }
      }
      traverse.apply(unit.body)
    }
  }
}
