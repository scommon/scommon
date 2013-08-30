package org.scommon.io

import java.io.OutputStream
import org.scommon.core.Closeable

/**
 * Convenience trait to describe an [[java.io.OutputStream]] that is also
 * [[org.scommon.core.Closeable]] compliant.
 */
trait CloseableOutputStream extends OutputStream with Closeable {
  protected def source:OutputStream

  override def hashCode() = source.hashCode()
  override def equals(obj: scala.Any) = source.equals(obj)

  def write(b: Int) = source.write(b)
  override def write(b: Array[Byte]) = source.write(b)
  override def write(b: Array[Byte], off: Int, len: Int) = source.write(b, off, len)
  override def flush() = source.flush()
  override def close() = source.close()
}