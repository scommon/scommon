package org.scommon.script.engine.core

import scala.collection._

import java.nio.file.{LinkOption, Paths, Path}
import org.scommon.io.{PathUtil}
import org.scommon.reflect._

trait CompilerSettings[+TSpecific <: CompilerSpecificSettings] extends StandardFieldMirror with CompilerContext {
  def specific: TSpecific

  def fields = Iterable(
      Field.withDefault("inMemory", "In Memory", "A hint for the engine asking it to compile source in memory if possible.", inMemory)
    , Field.withDefault("relativeDirectory", "Relative Directory", "The directory that will be used to resolve relative paths.", relativeDirectory)
    , Field.withDefault("outputDirectory", "Output Directory", "The directory where any compiled source will be placed.", outputDirectory)
    , Field.withDefault("options", "Additional Options", "Any additional options (arguments) that may be of use to the compiler.", options)
    , Field.withDefault("temporaryDirectory", "Temporary Directory", "The directory to use for any intermediate artifacts needed during compilation.", temporaryDirectory)
    , Field.withDefault("classPath", "Classpath", "The classpath used by the compiler to resolve type references.", classPath)
    , Field.withDefault("bootClassPath", "Boot Classpath", "The classpath used by the compiler for the bootstrap class loader.", bootClassPath)
    , Field.withDefault("customClassPath", "Custom Classpath", "Additional classpath that will be prepended to the standard classpath.", customClassPath)

    //Deliberately omit specific, handlers, typeFilters, and relativeCustomClassPath since they're not meant to be set directly.
  )

  def handlers: CompilerEventHandlers

  def inMemory: Boolean
  def relativeDirectory: Path
  def outputDirectory: Path
  def options: Iterable[String]
  def temporaryDirectory: Path
  def classPath: Iterable[String]
  def bootClassPath: Iterable[String]
  def customClassPath: Iterable[String]
  def typeFilters: Iterable[CompilerTypeFilter]

  final def relativeCustomClassPath: Iterable[String] = {
    val resolved = (
      for {
        custom <- customClassPath
        absolute_path = relativeDirectory.resolve(custom).toFile.getAbsolutePath
      } yield absolute_path
      ).toSeq

    resolved.reverse
  }

  override def toString =
    s"""
    |relativeDirectory:  $relativeDirectory
    |customClassPath:    $customClassPath
    |classPath:          $classPath
    |options:            $options
    |specific:           $specific
    |inMemory:           $inMemory
    |outputDirectory:    $outputDirectory
    |temporaryDirectory: $temporaryDirectory
    |bootClassPath:      $bootClassPath
    """
    .stripMargin
}

case class StandardCompilerSettings[+S <: CompilerSpecificSettings](
    var handlers          : StandardCompilerEventHandlers                  = StandardCompilerEventHandlers()
  , var inMemory          : Boolean                                        = true
  , var relativeDirectory : Path                                           = Paths.get(".").toRealPath(LinkOption.NOFOLLOW_LINKS).toAbsolutePath
  , var outputDirectory   : Path                                           = Paths.get(PathUtil.querySystemUserTempDirectory)
  , var options           : Iterable[String]                               = Iterable.empty
  , var temporaryDirectory: Path                                           = Paths.get(PathUtil.querySystemUserTempDirectory)
  , var classPath         : Iterable[String]                               = Environment.determineFullUserClassPath()
  , var bootClassPath     : Iterable[String]                               = Environment.determineFullUserClassPath()
  , var customClassPath   : Iterable[String]                               = Iterable.empty
  ,     typeFilters       : mutable.LinkedHashSet[CompilerTypeFilter]      = mutable.LinkedHashSet[CompilerTypeFilter]()
  ,     specific          : S
) extends CompilerSettings[S] {
  import scala.reflect.runtime.universe._

  def clearTypeFilters(): this.type = {
    typeFilters.clear()
    this
  }

  def withTypeFilter[T : TypeTag]: this.type = {
    typeFilters += CompilerTypeFilter[T]
    this
  }

  def withTypeFilter(typeFilter: CompilerTypeFilter): this.type = {
    typeFilters += typeFilter
    this
  }

}
