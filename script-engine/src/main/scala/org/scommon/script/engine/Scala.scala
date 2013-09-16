package org.scommon.script.engine

import org.scommon.core._
import org.scommon.reflect._
import org.scommon.script.engine.core._

import scala.language.implicitConversions

import scala.tools.util.PathResolver.Defaults
import scala.tools.nsc

object ScalaDebugLevel extends Enum {
  sealed case class EnumVal private[ScalaDebugLevel](compilerValue: String, title: String) extends Value

  val None        = EnumVal("none", "None")
  val Source      = EnumVal("source", "Source")
  val Line        = EnumVal("line", "Line")
  val Vars        = EnumVal("vars", "Vars")
  val NoTailCalls = EnumVal("notailcalls", "No Tail Calls")
}

object ScalaTarget extends Enum {
  sealed case class EnumVal private[ScalaTarget](compilerValue: String, title: String) extends Value
  val Javav1_6 = EnumVal("jvm-1.6", "Java v1.6")
  val Javav1_7 = EnumVal("jvm-1.7", "Java v1.7")
}

trait Scala extends CompilerSpecificSettings {
  def fields = Iterable(
      Field.withDefault("extdirs",           "Extension Directory", "Override location of installed extensions.", javaextdirs)
    , Field.withDefault("javabootclasspath", "Java Boot Classpath", "Override java boot classpath.", javabootclasspath)
    , Field.withDefault("javaextdirs",       "Java Extension Mechanism Directory", "Override java extdirs classpath.", javaextdirs)
    , Field.withDefault("dependencyfile",    "Dependency Tracking File", "Set dependency tracking file.", dependencyfile)
    , Field.withDefault("deprecation",       "Deprecation", "Emit warning and location for usages of deprecated APIs.", deprecation)
    , Field.withDefault("encoding",          "Encoding", "Specify character encoding used by source.", encoding)
    , Field.withDefault("explaintypes",      "Explain Type Errors", "Explain type errors in more detail.", explaintypes)
    , Field.withDefault("feature",           "Feature Usage Warnings", "Emit warning and location for usages of features that should be imported explicitly.", feature)
    , Field.withDefault("g",                 "Debug Level", "Set level of generated debugging info.", g)
    , Field.withDefault("nowarn",            "No Warnings", "Generate no warnings.", nowarn)
    , Field.withDefault("print",             "Print Program", "Print program with Scala-specific features removed.", print)
    , Field.withDefault("target",            "Class Format", "Target platform for object files.", target)
    , Field.withDefault("unchecked",         "Unchecked", "Enable additional warnings where generated code depends on assumptions.", unchecked)
    , Field.withDefault("uniqid",            "Uniquely Tag Identifiers", "Uniquely tag all identifiers in debugging output.", uniqid)
    , Field.withDefault("usejavacp",         "Use java.class.path", "Utilize the java.class.path in classpath resolution.", usejavacp)
    , Field.withDefault("verbose",           "Verbose", "Output messages about what the compiler is doing.", verbose)
    , Field.withDefault("elidebelow",        "Elide Below", "Calls to @elidable methods are omitted if method priority is lower than argument", elidebelow)
  )

  def extdirs          : String
  def javabootclasspath: String
  def javaextdirs      : String
  def dependencyfile   : String
  def deprecation      : Boolean
  def encoding         : String
  def explaintypes     : Boolean
  def feature          : Boolean
  def g                : ScalaDebugLevel.EnumVal
  def nowarn           : Boolean
  def print            : Boolean
  def target           : ScalaTarget.EnumVal
  def unchecked        : Boolean
  def uniqid           : Boolean
  def usejavacp        : Boolean
  def verbose          : Boolean
  def elidebelow       : Int

  def fnCustomize      : (nsc.Settings => nsc.Settings)
}

object ScalaCompilerSettings {
  def apply(): StandardCompilerSettings[ScalaSpecificSettings] =
    StandardCompilerSettings(specific = ScalaSpecificSettings())

  def apply(fnCustomize: (nsc.Settings => nsc.Settings)): StandardCompilerSettings[ScalaSpecificSettings] =
    StandardCompilerSettings(specific = ScalaSpecificSettings(fnCustomize = fnCustomize))

  private[engine] def toNscSettings(c: CompilerSettings[Scala]): nsc.Settings = {
    val s = new nsc.Settings(/*error fn*/)

    val (success, unprocessed) = s.processArguments(c.options.toList, false)
    if (!success)
      throw CompilerError(s"Invalid arguments provided for the script engine Scala compiler. Arguments not processed: ${unprocessed.mkString(", ")}")

    s.outdir.value            = c.outputDirectory.toAbsolutePath.toString

    s.extdirs.value           = c.specific.extdirs
    s.javabootclasspath.value = c.specific.javabootclasspath
    s.javaextdirs.value       = c.specific.javaextdirs
    s.dependencyfile.value    = c.specific.dependencyfile
    s.deprecation.value       = c.specific.deprecation
    s.encoding.value          = c.specific.encoding
    s.explaintypes.value      = c.specific.explaintypes
    s.feature.value           = c.specific.feature
    s.g.value                 = c.specific.g.compilerValue
    s.nowarn.value            = c.specific.nowarn
    s.print.value             = c.specific.print
    s.target.value            = c.specific.target.compilerValue
    s.unchecked.value         = c.specific.unchecked
    s.uniqid.value            = c.specific.uniqid
    s.usejavacp.value         = c.specific.usejavacp
    s.verbose.value           = c.specific.verbose
    s.elidebelow.value        = c.specific.elidebelow

    s.classpath.value         = c.classPath.mkString(java.io.File.pathSeparator)
    s.bootclasspath.value     = c.bootClassPath.mkString(java.io.File.pathSeparator)

    for(element <- c.relativeCustomClassPath)
      s.classpath.prepend(element)

    c.specific.fnCustomize(s)
  }
}

case class ScalaSpecificSettings(
    var extdirs          : String                  = Defaults.scalaExtDirs
  , var javabootclasspath: String                  = Defaults.javaBootClassPath
  , var javaextdirs      : String                  = Defaults.javaExtDirs
  , var dependencyfile   : String                  = ".scala_dependencies"
  , var deprecation      : Boolean                 = true
  , var encoding         : String                  = nsc.Properties.sourceEncoding
  , var explaintypes     : Boolean                 = false
  , var feature          : Boolean                 = true
  , var g                : ScalaDebugLevel.EnumVal = ScalaDebugLevel.Vars
  , var nowarn           : Boolean                 = false
  , var print            : Boolean                 = false
  , var target           : ScalaTarget.EnumVal     = ScalaTarget.Javav1_6
  , var unchecked        : Boolean                 = true
  , var uniqid           : Boolean                 = false
  , var usejavacp        : Boolean                 = Defaults.useJavaClassPath
  , var verbose          : Boolean                 = false
  , var elidebelow       : Int                     = 900

  ,     fnCustomize      : (nsc.Settings => nsc.Settings) = (x => x)
) extends Scala