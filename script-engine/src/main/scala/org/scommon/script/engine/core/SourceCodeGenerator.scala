package org.scommon.script.engine.core

import scala.collection._

import java.io.{ByteArrayInputStream, File, InputStream}
import java.net.URI

import org.scommon.reactive.Generator

import scala.language.implicitConversions

object SourceCodeGenerator {
  type URISourceCodeGenerator = SourceCodeGenerator[URI, SourceCode[URI], CompileContext]

  @inline def fromStrings(strings: String*): URISourceCodeGenerator =
    fromStrings(strings)

  @inline def fromStrings(strings: Iterable[String]): URISourceCodeGenerator =
    fromStreams(strings.map(s  => new ByteArrayInputStream(s.getBytes("UTF-8"))))

  @inline def fromStreams(streams: InputStream*): URISourceCodeGenerator =
    fromStreams(streams.toIterable)

  def fromStreams(streams: Iterable[InputStream]): URISourceCodeGenerator = {
    val initial = for {
      stream <- streams
      source = SourceCode.fromStream(stream)
    } yield source

    new URISourceCodeGenerator(initial)
  }

  def fromDirectory(directory: File): URISourceCodeGenerator = {
    require(!directory.exists() || directory.isDirectory, "Compiler source generator must be a directory")

    new URISourceCodeGenerator(Seq()) {

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

sealed class SourceCodeGenerator[TSource <: URI, TCompilerSource <: SourceCode[TSource], TContext <: CompileContext](
  override val initial: Iterable[TCompilerSource]
) extends Generator[TCompilerSource, TContext] {

}
