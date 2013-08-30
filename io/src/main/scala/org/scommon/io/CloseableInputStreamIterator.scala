package org.scommon.io

import java.io.InputStream
import org.scommon.core.{CloseableIterator, Closeable}

/**
 * Creates an [[scala.collection.Iterator]] that represents a provided source.
 * Data is read via [[org.scommon.io.CloseableInputStreamIterator#next()]] and
 * returns a tuple containing the number of bytes read and a buffer containing
 * the bytes.
 */
trait CloseableInputStreamIterator extends CloseableIterator[(Int, Array[Byte])] {
  protected def source:InputStream
  protected def bufferSize:Int

  @volatile private[this] var read = 0

  override def hasDefiniteSize: Boolean = false
  override def isTraversableAgain: Boolean = false

  def close(): Unit = source.close()
  def hasNext: Boolean = read >= 0

  def next(): (Int, Array[Byte]) = {
    val buffer = new Array[Byte](bufferSize)
    val bytes_read = source.read(buffer)
    read = bytes_read

    (bytes_read, buffer)
  }
}
