package org.scommon.script.engine.core

import scala.collection._

import java.io.{ByteArrayInputStream, File, InputStream}

import scala.language.implicitConversions
import org.scommon.reactive.Generator
import java.net.URI

object CompilerSourceGenerator {
  type CompilerSourcesAvailable = (Iterable[CompilerSource[Any]]) => Unit
  type CompilerSourceGeneration = CompilerSourceGenerator[URI, CompilerSource[URI]]

  @inline def fromStrings(strings: String*): CompilerSourceGeneration =
    fromStrings(strings)

  @inline def fromStrings(strings: Iterable[String]): CompilerSourceGeneration =
    fromStreams(strings.map(s  => new ByteArrayInputStream(s.getBytes("UTF-8"))))

  @inline def fromStreams(streams: InputStream*): CompilerSourceGeneration =
    fromStreams(streams.toIterable)

  def fromStreams(streams: Iterable[InputStream]): CompilerSourceGeneration = {
    val initial = for {
      stream <- streams
      source = CompilerSource.fromStream(stream)
    } yield source

    new CompilerSourceGeneration(initial)
  }

  def fromDirectory(directory: File): CompilerSourceGeneration = {
    require(!directory.exists() || directory.isDirectory, "Compiler source generator must be a directory")

    new CompilerSourceGeneration(Seq()) {

    }
  }

//  @inline def fromStrings(strings: String*): CompilerSourceGeneratorBuilder =
//    fromStreams(strings.map(s  => new ByteArrayInputStream(s.getBytes("UTF-8"))))
//
//  @inline def fromStreams(streams: InputStream*): CompilerSourceGeneratorBuilder =
//    fromStreams(streams.toIterable)
//
//  def fromStreams(streams: Iterable[InputStream]): CompilerSourceGeneratorBuilder =
//    new CompilerSourceGeneratorBuilderImpl {
//      override lazy val initialSources = (
//        for {
//          stream <- streams
//          source = CompilerSource.fromStream(stream)
//        } yield source
//      ).toSeq
//    }
//
//  def fromDirectory(directory: File): CompilerSourceGeneratorBuilder = {
//    require(!directory.exists() || directory.isDirectory, "Compiler source generator must be a directory")
//
//    new CompilerSourceGeneratorBuilderImpl {
//
//    }
//  }
}

sealed class CompilerSourceGenerator[TSource >: URI, TCompilerSource >: CompilerSource[TSource]](
  override val initial: Iterable[TCompilerSource]
) extends Generator[TCompilerSource] {

}
