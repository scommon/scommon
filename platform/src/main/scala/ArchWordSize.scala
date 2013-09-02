package org.scommon.platform

import org.scommon.core._

/**
 * Describes various processor architectures.
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
object ArchWordSize extends Enum {
  sealed case class EnumVal private[ArchWordSize](title: String) extends Value

  val Unknown     = EnumVal("Unknown")
  val Size8Bits   = EnumVal("Size8Bits")
  val Size16Bits  = EnumVal("Size16Bits")
  val Size32Bits  = EnumVal("Size32Bits")
  val Size64Bits  = EnumVal("Size64Bits")
  val Size128Bits = EnumVal("Size128Bits")
  val Size256Bits = EnumVal("Size256Bits")
  val Size512Bits = EnumVal("Size512Bits")
}