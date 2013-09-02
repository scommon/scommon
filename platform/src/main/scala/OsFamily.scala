package org.scommon.platform

import org.scommon.core._

/**
 * Gathers information about operating systems and the one we're hosted on.
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
object OsFamily extends Enum {

  sealed case class EnumVal private[OsFamily](platformPartName: String, isPosix: Boolean) extends Value

  val Unknown = EnumVal(platformPartName = StringUtil.empty, isPosix = false)
  val Windows = EnumVal(platformPartName = "windows",        isPosix = false)
  val Mac     = EnumVal(platformPartName = "osx",            isPosix = true)
  val Unix    = EnumVal(platformPartName = "unix",           isPosix = true)
  val Solaris = EnumVal(platformPartName = "solaris",        isPosix = true)
  val VMS     = EnumVal(platformPartName = "vms",            isPosix = false)

  def isPOSIX(family: EnumVal): Boolean = family.isPosix
  def isPOSIX(os: Os.EnumVal): Boolean = isPOSIX(os.family)

  def systemOS: Os.EnumVal = Os.systemOS
  def systemOSFamily: EnumVal = Os.systemOSFamily

  def fromName(name: String): EnumVal = {
    if (name.isNullOrEmpty) {
      return Unknown
    }

    for(family <- OsFamily.values)
      if (family.platformPartName.equalsIgnoreCase(name)) {
        return family
      }

    val lower = name.toLowerCase
    if (lower.contains("win")) Windows
    else if (lower.contains("mac")) Mac
    else if (lower.contains("nix") || lower.contains("nux")) Unix
    else if (lower.contains("vms")) VMS
    else if (lower.contains("solaris")) Solaris
    else Unknown
  }
}