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

    val compiler = ScalaCompiler(compiler_settings, Seq(new FilterForClassesImplementingTrait()) ++ settings.specific.phaseInterceptors) { msg =>
      context.handlers.messageReceived(this, msg)
    } { progress =>
      context.handlers.progressUpdate(this, progress)
    }

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

  trait Foo { self: Compiler with Serializable =>
  }

    import scala.reflect.runtime.universe._

    trait TypeFilter[T] {
      def tag: TypeTag[T]
    }

    def createFilter[T: TypeTag] = new TypeFilter[T] {
      val tag = implicitly[TypeTag[T]]
    }

    def isThisASubtype[T](filter: TypeFilter[T], global: nsc.Global)(s: global.Symbol, t: global.Type) = {
      import global._
      val t3 = filter.tag.in(global.rootMirror).tpe
      val is_subtype = t <:< t3
      println(s"${t.toString} IS SUBTYPE: ${is_subtype}")
    }

    /** Called when a class or module is found. */
    private[this] def callback(global: nsc.Global)(s: global.Symbol, t: global.Type) = {
      val filter = createFilter[AnyRef]

      isThisASubtype(filter, global)(s, t)
//      import scala.reflect.runtime.universe._
//      val mirror = runtimeMirror(Thread.currentThread().getContextClassLoader())
//
//      val x:java.lang.Thread = new Thread()
//      val t1: scala.reflect.runtime.universe.type#Type = t
//      val t2 = scala.reflect.runtime.universe.typeOf[String]
//
//      val is_subtype = t1 <:< t2
//      println(s"${t1.toString} IS SUBTYPE: ${is_subtype}")
    }

//    private[this] def callbackWithType(t: scala.reflect.runtime.universe.type#Type) = {
//      import org.scommon.reflect._
//      val a = "".typeOf
//      println(s"*********************** FOUND WITH TYPE: ${t.toLongString} ${t.baseClasses}") //${t <:< typeOf[MyTestTrait]}
//    }

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
              callback(global)(tree.symbol, tree.symbol.typeOfThis)
            case _ =>
          }
          super.traverse(tree)
        }
      }
      traverse.apply(unit.body)
    }
  }
}
