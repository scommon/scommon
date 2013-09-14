package org.scommon.script.engine.core

import java.nio.file.{Paths, Path}
import org.scommon.io.{PathUtil}

case class CompilerOption(name: String, value: String)

trait CompilerSettings[+TCompiler] {
  def compiler: TCompiler

  def inMemory: Boolean =
    true

  def outputDirectory: Option[Path] =
    temporaryDirectory

  def options: Iterable[CompilerOption] =
    Iterable.empty

  def temporaryDirectory: Option[Path] =
    Some(Paths.get(PathUtil.querySystemUserTempDirectory))

  def classPath: Iterable[String] =
    Environment.determineFullUserClassPath()

  def bootClassPath: Iterable[String] =
    classPath
}
