package org.scommon.core

/**
 * Should correlate to [[[org.scommon.core!CloseableType]]].
 *
 * It was a conscious decision to keep this in the core package and not in the
 * io package. This is primarily because it's intended use should be
 * transparent and generally under the covers but is still important for things
 * like `using { ... }`.
 */
trait Closeable extends java.io.Closeable {
  def close(): Unit
}

/**
 * Convenience trait for [[scala.collection.Iterator]] ensuring compliance with
 * [[org.scommon.core.CloseableType]].
 *
 * @tparam A type of data over which this instance will iterate
 */
trait CloseableIterator[+A] extends Iterator[A] with Closeable

