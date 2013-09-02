package org.scommon.platform

import org.scommon.core._

/**
 * Gathers information about operating systems and the one we're hosted on.
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
object Os extends Enum {

  sealed case class EnumVal private[Os](family: OsFamily.EnumVal, platformPartName: String, variants: Seq[String] = Seq()) extends Value {
    lazy val isPOSIX = Os.isPOSIX(this)
  }

  val Unknown        = EnumVal(family = OsFamily.Unknown, platformPartName = StringUtil.empty)

  val Windows95      = EnumVal(family = OsFamily.Windows, platformPartName = "windows_95",    variants = List("Windows 95"))
  val Windows98      = EnumVal(family = OsFamily.Windows, platformPartName = "windows_98",    variants = List("Windows 98"))
  val WindowsMe      = EnumVal(family = OsFamily.Windows, platformPartName = "windows_me",    variants = List("Windows Me"))
  val WindowsNT      = EnumVal(family = OsFamily.Windows, platformPartName = "windows_nt",    variants = List("Windows NT"))
  val Windows2000    = EnumVal(family = OsFamily.Windows, platformPartName = "windows_2000",  variants = List("Windows 2000"))
  val WindowsXP      = EnumVal(family = OsFamily.Windows, platformPartName = "windows_xp",    variants = List("Windows XP"))
  val Windows2003    = EnumVal(family = OsFamily.Windows, platformPartName = "windows_2003",  variants = List("Windows 2003"))
  val Windows2008    = EnumVal(family = OsFamily.Windows, platformPartName = "windows_2008",  variants = List("Windows 2008"))
  val WindowsVista   = EnumVal(family = OsFamily.Windows, platformPartName = "windows_vista", variants = List("Windows Vista"))
  val Windows7       = EnumVal(family = OsFamily.Windows, platformPartName = "windows_7",     variants = List("Windows 7"))
  val Windows8       = EnumVal(family = OsFamily.Windows, platformPartName = "windows_8",     variants = List("Windows 8"))
  val Windows9       = EnumVal(family = OsFamily.Windows, platformPartName = "windows_9",     variants = List("Windows 9"))
  val Windows10      = EnumVal(family = OsFamily.Windows, platformPartName = "windows_10",     variants = List("Windows 10"))
  val Windows11      = EnumVal(family = OsFamily.Windows, platformPartName = "windows_11",     variants = List("Windows 11"))
  val WindowsCE      = EnumVal(family = OsFamily.Windows, platformPartName = "windows_ce",    variants = List("Windows CE"))
  val OS2            = EnumVal(family = OsFamily.Windows, platformPartName = "os_2",          variants = List("OS/2"))
  val WindowsUnknown = EnumVal(family = OsFamily.Windows, platformPartName = OsFamily.Windows.platformPartName)


  val MacOSX         = EnumVal(family = OsFamily.Mac,     platformPartName = "osx",           variants = List("Mac OS", "Mac OS X"))
  val MacUnknown     = EnumVal(family = OsFamily.Mac,     platformPartName = OsFamily.Mac.platformPartName)


  val Linux          = EnumVal(family = OsFamily.Unix,    platformPartName = "linux",         variants = List("Linux"))
  val MPE_iX         = EnumVal(family = OsFamily.Unix,    platformPartName = "mpe_ix",        variants = List("MPE/iX"))
  val HP_UX          = EnumVal(family = OsFamily.Unix,    platformPartName = "hp_ux",         variants = List("HP-UX"))
  val AIX            = EnumVal(family = OsFamily.Unix,    platformPartName = "aix",           variants = List("AIX"))
  val FreeBSD        = EnumVal(family = OsFamily.Unix,    platformPartName = "freebsd",       variants = List("FreeBSD"))
  val Irix           = EnumVal(family = OsFamily.Unix,    platformPartName = "irix",          variants = List("Irix"))
  val OS_390         = EnumVal(family = OsFamily.Unix,    platformPartName = "os390",         variants = List("OS/390"))
  val DigitalUnix    = EnumVal(family = OsFamily.Unix,    platformPartName = "digital_unix",  variants = List("Digital Unix"))
  val Netware_4_11   = EnumVal(family = OsFamily.Unix,    platformPartName = "netware_4_11",  variants = List("NetWare 4.11"))
  val OSF1           = EnumVal(family = OsFamily.Unix,    platformPartName = "osf1",          variants = List("OSF1"))
  val SunOS          = EnumVal(family = OsFamily.Unix,    platformPartName = "sunos",         variants = List("SunOS"))
  val UnixUnknown    = EnumVal(family = OsFamily.Unix,    platformPartName = OsFamily.Unix.platformPartName)


  val Solaris        = EnumVal(family = OsFamily.Solaris, platformPartName = "solaris",       variants = List("Solaris"))
  val SolarisUnknown = EnumVal(family = OsFamily.Solaris, platformPartName = OsFamily.Solaris.platformPartName)


  val VMS            = EnumVal(family = OsFamily.VMS,     platformPartName = "openvms",       variants = List("OpenVMS"))
  val VMSUnknown     = EnumVal(family = OsFamily.VMS,     platformPartName = OsFamily.VMS.platformPartName)


  def systemOSName: String = System.getProperty("os.name")
  def systemOS: EnumVal = fromName(systemOSName)
  def systemOSFamily: OsFamily.EnumVal = systemOS.family
  def isPOSIX(os: Os.EnumVal): Boolean = OsFamily.isPOSIX(os)

  def fromName(name: String): EnumVal = {
    if (name.isNullOrEmpty) {
      return Unknown
    }

    for (os <- Os.values)
      for (variant <- os.variants)
        if (variant.equalsIgnoreCase(name)) {
          return os
        }

    val lower = name.toLowerCase
    if (lower.contains("win")) WindowsUnknown
    else if (lower.contains("mac")) MacUnknown
    else if (lower.contains("nix") || lower.contains("nux")) UnixUnknown
    else if (lower.contains("vms")) VMSUnknown
    else if (lower.contains("solaris")) SolarisUnknown
    else Unknown
  }
}