package org.scommon.script.engine.core

import org.scommon.core.Version

trait Compiler {
  def name: String
  def title: String
  def version: Version

  def compile(sources: Iterable[CompilerSource[Any]])
}
