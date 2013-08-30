package org.scommon.io

import scala.Array
import java.io.{InputStreamReader, BufferedReader, InputStream}
import java.nio.charset.{CharsetDecoder, Charset}
import org.scommon.core.{Closeable}

object InputStreamUtil {
  @inline def toIterator(input:InputStream, size:Int = 1024):Iterator[(Int, Array[Byte])] with Closeable = new CloseableInputStreamIterator {
    protected val source = input
    protected val bufferSize = size
  }

  @inline def toCloseable(input:InputStream):InputStream with Closeable = new CloseableInputStream {
    protected val source = input
  }

  @inline def toBufferedReader(input:InputStream, charset:Charset):BufferedReader with Closeable =
    new BufferedReader(new InputStreamReader(input, charset)) with Closeable

  @inline def toBufferedReader(input:InputStream, charsetName:String):BufferedReader with Closeable =
    new BufferedReader(new InputStreamReader(input, charsetName)) with Closeable

  @inline def toBufferedReader(input:InputStream, dec:CharsetDecoder):BufferedReader with Closeable =
    new BufferedReader(new InputStreamReader(input, dec)) with Closeable
}
