package org.scommon

import java.io._
import org.scommon.core.{Closeable, CloseableIterator}
import java.nio.charset.{CharsetDecoder, CharsetEncoder, Charset}

package object io {
  implicit lazy val DEFAULT_CHARSET = Charset.forName("UTF-8")

  @inline implicit class PathUtilFileExtensions(f: File) {
    /** @see [[org.scommon.io.PathUtil#tempDirectory]] */
    @inline def tempDirectory: String = PathUtil.tempDirectory

    /** @see [[org.scommon.io.PathUtil.userTempDirectory]] */
    @inline def userTempDirectory: String = PathUtil.userTempDirectory

    /** @see [[org.scommon.io.PathUtil.toTemp()]] */
    @inline def toTemp: File = PathUtil.toTemp(f)

    /** @see [[org.scommon.io.PathUtil.toUserTemp()]] */
    @inline def toUserTemp: File = PathUtil.toUserTemp(f)

    /** @see [[org.scommon.io.PathUtil.deleteAll()]] */
    @inline def deleteAll: Boolean = PathUtil.deleteAll(f)
  }

  @inline implicit class FileUtilFileExtensions(f: File) {
    /** @see [[org.scommon.io.FileUtil.touch()]] */
    @inline def touch: Boolean = FileUtil.touch(f)
    @inline def touch(time: Long): Boolean = FileUtil.touch(f, time)
    @inline def open: OutputStream with Closeable = openForWrite()
    @inline def openForRead: InputStream with Closeable = FileUtil.openForRead(f)
    @inline def openForWrite: OutputStream with Closeable = FileUtil.openForWrite(f, append = false, createIfDoesntExist = true)
    @inline def openForWrite(append:Boolean = false, createIfDoesntExist:Boolean = true): OutputStream with Closeable = FileUtil.openForWrite(f, append, createIfDoesntExist)
  }

  @inline implicit class InputStreamExtensions(input: InputStream) {
    @inline def toIterator: Iterator[(Int, Array[Byte])] with Closeable =
      InputStreamUtil.toIterator(input)
    @inline def toIterator(bufferSize: Int = 1024): Iterator[(Int, Array[Byte])] with Closeable =
      InputStreamUtil.toIterator(input, bufferSize)
    @inline def toCloseable: InputStream with Closeable =
      InputStreamUtil.toCloseable(input)
    @inline def toBufferedReader(charsetName:String):BufferedReader with Closeable =
      InputStreamUtil.toBufferedReader(input, charsetName)
    @inline def toBufferedReader(implicit charset:Charset = DEFAULT_CHARSET):BufferedReader with Closeable =
      InputStreamUtil.toBufferedReader(input, charset)
  }

  @inline implicit class OutputStreamExtensions(output: OutputStream) {
    @inline def toCloseable: OutputStream with Closeable =
      OutputStreamUtil.toCloseable(output)
    @inline def toBufferedOutputStream(bufferSize: Int = 1024) =
      OutputStreamUtil.toBufferedOutputStream(output, bufferSize)
    @inline def toBufferedWriter(charsetName:String):BufferedWriter with Closeable =
      OutputStreamUtil.toBufferedWriter(output, charsetName)
    @inline def toBufferedWriter()(implicit charset:Charset = DEFAULT_CHARSET):BufferedWriter with Closeable =
      OutputStreamUtil.toBufferedWriter(output, charset)
  }
}
