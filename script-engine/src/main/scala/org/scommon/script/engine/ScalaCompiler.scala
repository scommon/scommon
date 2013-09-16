package org.scommon.script.engine

import _root_.scala.tools.nsc
import _root_.scala.reflect.io.NoAbstractFile

import org.scommon.script.engine.core._
import org.scommon.core.Version
import org.scommon.script.engine.core.CompilerProgress

import java.io.IOException
import java.nio.charset.Charset

trait ScalaPhaseIntercept extends CompilerPhaseIntercept {
  def intercept(global: nsc.Global)(unit: global.CompilationUnit)
}

object ScalaCompiler {
  val name = "scala-compiler"
  val title = "Scala Compiler"
  val version = Version(_root_.scala.util.Properties.versionNumberString)

  implicit def toSubComponent(intercept: ScalaPhaseIntercept, providedGlobal: nsc.Global): nsc.SubComponent = new nsc.SubComponent { self =>
    import providedGlobal._

    val global              = providedGlobal
    val phaseName           = intercept.name
    val name                = phaseName
    val runsAfter           = intercept.runsAfterPhases.map(_.phaseName).toList
    val runsRightAfter      = intercept.runsRightAfterPhase.map(_.phaseName)
    override val runsBefore = intercept.runsBeforePhases.map(_.phaseName).toList

    require(runsBefore     forall CompilerPhase.isKnown, s"All phases specified in ${typeOf[CompilerPhaseIntercept].toLongString}.runsBeforePhases must be a known valid value")
    require(runsAfter      forall CompilerPhase.isKnown, s"All phases specified in ${typeOf[CompilerPhaseIntercept].toLongString}.runsAfterPhases must be a known valid value")
    require(runsRightAfter forall CompilerPhase.isKnown, s"The phase specified in ${typeOf[CompilerPhaseIntercept].toLongString}.runsRightAfterPhase must be a known valid value")

    def newPhase(prev: nsc.Phase): nsc.Phase = new nsc.Phase(prev) {
      val name = phaseName
      def run(): Unit = {
        //Only look for units that are compiling Scala.
        for {
          unit <- currentRun.units
          if !unit.isJava
        } intercept.intercept(providedGlobal)(unit)
      }
    }
  }

  implicit def toAbstractFile(source: CompilerSource[Any], charset: Charset = org.scommon.io.DEFAULT_CHARSET): nsc.io.AbstractFile =
    new nsc.io.AbstractFile {
      /** Returns contents of file (if applicable) in a Char array.
        *  warning: use <code>Global.getSourceFile()</code> to use the proper
        *  encoding when converting to the char array.
        */
      @throws(classOf[IOException])
      override def toCharArray = new String(toByteArray, charset).toCharArray

      /** Returns contents of file (if applicable) in a byte array.
        */
      @throws(classOf[IOException])
      override def toByteArray: Array[Byte] = source.contents
      def input = ???
      def path = source.path
      def name = source.name

      override def hashCode() = path.hashCode()
      override def equals(obj: Any) = path.equals(obj)

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
}

class ScalaCompiler(
  val settings: nsc.Settings,
  val reporter: nsc.reporters.Reporter,
  val intercepts: Iterable[ScalaPhaseIntercept],
      progressListener: Option[CompilerProgressListener] = None
) extends core.Compiler
{
  import ScalaCompiler._

  val name    = ScalaCompiler.name
  val title   = ScalaCompiler.title
  val version = ScalaCompiler.version

  private[this] val compiler =
    new nsc.Global(settings, reporter) {
      override protected def computeInternalPhases () {
        super.computeInternalPhases

        val components = intercepts.map(ScalaCompiler.toSubComponent(_, this))

        for(component <- components)
          addToPhasesSet(component, component.phaseName)
      }
    }

  private[this] val r = new compiler.Run {
    @volatile private[this] var lastPhase = 0
    @volatile private[this] var currentPhase: CompilerPhase.EnumVal = CompilerPhase.Unknown

    override def informUnitStarting(phase: nsc.Phase, unit: compiler.CompilationUnit) {
      super.informUnitStarting(phase, unit)
      currentPhase = CompilerPhase(phase.name)
    }

    override def progress(current: Int, total: Int) {
      super.progress(current, total)
      if (current != lastPhase) {
        lastPhase = current
        if (progressListener.isDefined) {
          progressListener.get.progressUpdate(CompilerProgress(
            phase           = currentPhase,
            phaseIndex      = current,
            totalPhaseCount = total,
            progress        = math.floor((current.toDouble / total.toDouble) * 100.0D) / 100.0D
          ))
        }
      }
    }
  }

  def compile(sources: Iterable[CompilerSource[Any]]) = {
    val form_that_compiler_understands =
      for {
        source <- sources
        abstract_file = toAbstractFile(source)
      } yield abstract_file
    r.compileFiles(form_that_compiler_understands.toList)
  }

  def compile(sources:nsc.io.AbstractFile*): Unit = {
    r.compileFiles(sources.toList)
  }
}