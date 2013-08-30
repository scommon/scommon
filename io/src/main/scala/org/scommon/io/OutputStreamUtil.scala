package org.scommon.io

import java.io.{OutputStreamWriter, BufferedWriter, BufferedOutputStream, OutputStream}
import org.scommon.core.{Closeable}
import java.nio.charset.{CharsetEncoder, Charset}

object OutputStreamUtil {
  @inline def toCloseable(output:OutputStream):OutputStream with Closeable = new CloseableOutputStream {
    protected val source = output
  }

  @inline def toBufferedOutputStream(output:OutputStream, bufferSize:Int = 1024):BufferedOutputStream with Closeable =
    new CloseableBufferedOutputStream(output, bufferSize)

  @inline def toBufferedWriter(output:OutputStream, charset:Charset):BufferedWriter with Closeable =
    new BufferedWriter(new OutputStreamWriter(output, charset)) with Closeable

  @inline def toBufferedWriter(output:OutputStream, charsetName:String):BufferedWriter with Closeable =
    new BufferedWriter(new OutputStreamWriter(output, charsetName)) with Closeable

  @inline def toBufferedWriter(output:OutputStream, enc:CharsetEncoder):BufferedWriter with Closeable =
    new BufferedWriter(new OutputStreamWriter(output, enc)) with Closeable

  private class CloseableBufferedOutputStream(output:OutputStream, bufferSize:Int) extends BufferedOutputStream(output, bufferSize) with CloseableOutputStream {
    protected val source = this
    override def write(b: Int) = output.write(b)
  }
}