package org.scommon.script.engine.core

import java.io.{ByteArrayInputStream, InputStream}
import java.net.URI
import java.util.UUID
import scala.collection.mutable

import org.scommon.io._

trait SourceCode[+TFrom] {
  def contents: Array[Byte]
  def from: TFrom
  def path: String
  def name: String
}

object SourceCode {
  def fromString(value: String) =
    fromStream(new ByteArrayInputStream(value.getBytes("UTF-8")))

  def fromStream(input: InputStream, inputSource: URI = URI.create(s"mem:///stream/${UUID.randomUUID().toString}")): SourceCode[URI] = {
    new SourceCode[URI] {
      lazy val contents = {
        val a = mutable.ArrayBuffer[Byte]()

        //Suboptimal since we're appending to the ArrayBuffer via a conversion to a view first.
        //Reminder: the input stream is automatically closed.
        for((bytes_read, buffer) <- input.toIterator)
          a ++= buffer.view(0, bytes_read)

        a.toArray
      }

      val from = inputSource
      val path = Option(inputSource.getPath) getOrElse ""
      val name = {
        val p = path
        val idx = p.lastIndexOf('/')
        if (idx >= 0) p.substring(idx + 1) else ""
      }

      override def toString = from.toString
    }
  }
}