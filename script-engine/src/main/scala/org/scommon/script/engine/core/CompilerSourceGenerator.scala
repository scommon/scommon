package org.scommon.script.engine.core

import scala.collection._

import java.io.{ByteArrayInputStream, File, InputStream}

import scala.language.implicitConversions

object CompilerSourceGenerator {
  type CompilerSourcesAvailable = (Iterable[CompilerSource[Any]]) => Unit

  @inline def fromStrings(strings: String*): CompilerSourceGeneratorBuilder =
    fromStreams(strings.map(s  => new ByteArrayInputStream(s.getBytes("UTF-8"))))

  @inline def fromStreams(streams: InputStream*): CompilerSourceGeneratorBuilder =
    fromStreams(streams.toIterable)

  def fromStreams(streams: Iterable[InputStream]): CompilerSourceGeneratorBuilder =
    new CompilerSourceGeneratorBuilderImpl {
      override lazy val initialSources = (
        for {
          stream <- streams
          source = CompilerSource.fromStream(stream)
        } yield source
      ).toSeq
    }

  def fromDirectory(directory: File): CompilerSourceGeneratorBuilder = {
    require(!directory.exists() || directory.isDirectory, "Compiler source generator must be a directory")

    new CompilerSourceGeneratorBuilderImpl {

    }
  }

  @inline implicit def builder2Generator(builder: CompilerSourceGeneratorBuilder): CompilerSourceGenerator =
    builder.toGenerator
}

trait CompilerSourceGeneratorBuilder {
  import CompilerSourceGenerator._

  /**
   * Routes notification to the provided function body. When all routes have been defined,
   * please call [[org.scommon.script.engine.core.CompilerSourceGeneratorBuilder#compose]]
   * or [[org.scommon.script.engine.core.CompilerSourceGeneratorBuilder#toGenerator]].
   *
   * You can alternatively assign the builder to a concrete
   * [[org.scommon.script.engine.core.CompilerSourceGenerator]] value which will implicitly
   * invoke the [[org.scommon.script.engine.core.CompilerSourceGenerator.builder2Generator()]]
   * method.
   *
   * @param fn Code body to be executed when source changes are detected and propagated.
   * @return This instance to allow multiple routes to be defined.
   */
  def ->>(fn: CompilerSourcesAvailable): this.type

  /**
   * Composes this builder and emits an immutable instance of [[org.scommon.script.engine.core.CompilerSourceGenerator]].
   *
   * @return An instance of [[org.scommon.script.engine.core.CompilerSourceGenerator]].
   */
  def toGenerator: CompilerSourceGenerator

  /**
   * Composes this builder and emits an immutable instance of [[org.scommon.script.engine.core.CompilerSourceGenerator]].
   *
   * @return An instance of [[org.scommon.script.engine.core.CompilerSourceGenerator]].
   */
  def compose: CompilerSourceGenerator = toGenerator
}

sealed private class CompilerSourceGeneratorBuilderImpl extends CompilerSourceGeneratorBuilder { self =>
  import CompilerSourceGenerator._

  protected lazy val initialSources: Seq[CompilerSource[Any]] = Seq.empty

  private[this] val listeners = mutable.LinkedHashSet[CompilerSourcesAvailable]()

  def ->>(fn: CompilerSourcesAvailable) = synchronized {
    listeners += fn
    self
  }

  def toGenerator = new CompilerSourceGenerator {
    private[this] val my_listeners = listeners.toSeq

    /**
     *
     * @param sources Should be traversable more than once since more than one listener may be interested.
     */
    protected def notifySourcesAvailable(sources: Iterable[CompilerSource[Any]]): Unit = synchronized {
      my_listeners.foreach(listener => listener(sources))
    }

    notifySourcesAvailable(initialSources)
  }
}

trait CompilerSourceGenerator { self =>
  /**
   *
   * @param sources Should be traversable more than once since more than one listener may be interested.
   */
  protected def notifySourcesAvailable(sources: Iterable[CompilerSource[Any]]): Unit
}


