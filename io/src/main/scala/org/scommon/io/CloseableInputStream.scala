package org.scommon.io

import org.scommon.core.Closeable
import java.io.InputStream

/**
 * Convenience trait to describe an [[java.io.InputStream]] that is also
 * [[org.scommon.core.Closeable]] compliant.
 */
trait CloseableInputStream extends InputStream with Closeable {
  protected def source:InputStream

  def read() = source.read()
  override def read(b: Array[Byte]) = source.read(b)
  override def read(b: Array[Byte], off: Int, len: Int) = source.read(b, off, len)
  override def skip(n: Long) = source.skip(n)

  override def available() = source.available()
  override def mark(readlimit: Int) = source.mark(readlimit)
  override def reset() = source.reset()
  override def markSupported() = source.markSupported()

  override def hashCode() = source.hashCode()
  override def equals(obj: scala.Any) = source.equals(obj)

  override def close() = source.close()
}
