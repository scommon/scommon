package org.scommon.platform

import org.scommon.core._

/**
 * The processor architecture of the system we're executing on.
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
object Arch extends Enum {

  sealed case class EnumVal private[Arch](wordSize: ArchWordSize.EnumVal, platformPartName: String, variants: Seq[String] = Seq()) extends Value {
    lazy val is8Bit: Boolean   = wordSize == ArchWordSize.Size8Bits
    lazy val is16Bit: Boolean  = wordSize == ArchWordSize.Size16Bits
    lazy val is32Bit: Boolean  = wordSize == ArchWordSize.Size32Bits
    lazy val is64Bit: Boolean  = wordSize == ArchWordSize.Size64Bits
    lazy val is128Bit: Boolean = wordSize == ArchWordSize.Size128Bits
    lazy val is256Bit: Boolean = wordSize == ArchWordSize.Size256Bits
    lazy val is512Bit: Boolean = wordSize == ArchWordSize.Size512Bits
  }


  val Unknown  = EnumVal(wordSize = ArchWordSize.Unknown,    platformPartName = StringUtil.empty)

  val x86      = EnumVal(wordSize = ArchWordSize.Size32Bits, platformPartName = "x86",      variants = List("x86", "i386", "i486", "i586", "i686"))
  val x86_64   = EnumVal(wordSize = ArchWordSize.Size64Bits, platformPartName = "x86_64",   variants = List("x86_64", "x64", "amd64"))
  val IA64     = EnumVal(wordSize = ArchWordSize.Size64Bits, platformPartName = "ia64",     variants = List("IA64N"))
  val PPC      = EnumVal(wordSize = ArchWordSize.Size32Bits, platformPartName = "ppc",      variants = List("ppc", "PowerPC", "Power"))
  val PPC64    = EnumVal(wordSize = ArchWordSize.Size64Bits, platformPartName = "ppc64",    variants = List("ppc64"))
  val Arm      = EnumVal(wordSize = ArchWordSize.Size32Bits, platformPartName = "arm",      variants = List("arm"))
  val ArmV4I   = EnumVal(wordSize = ArchWordSize.Size32Bits, platformPartName = "armv4i",   variants = List("armv4i"))
  val Sparc    = EnumVal(wordSize = ArchWordSize.Size64Bits, platformPartName = "sparc",    variants = List("sparc"))
  val PA_RISC  = EnumVal(wordSize = ArchWordSize.Size64Bits, platformPartName = "pa_risc",  variants = List("PA-RISC", "PA_RISC2.0"))
  val POWER_RS = EnumVal(wordSize = ArchWordSize.Unknown,    platformPartName = "power_rs", variants = List("POWER_RS"))
  val MIPS     = EnumVal(wordSize = ArchWordSize.Size64Bits, platformPartName = "mips",     variants = List("mips"))
  val Alpha    = EnumVal(wordSize = ArchWordSize.Size64Bits, platformPartName = "alpha",    variants = List("alpha"))


  def systemArchName: String = System.getProperty("os.arch")
  def systemArch: EnumVal = fromName(systemArchName)

  def fromName(name: String): EnumVal = {
    if (name.isNonEmpty) {
      for (a <- Arch.values)
        for (variant <- a.variants)
          if (variant.equalsIgnoreCase(name)) {
            return a
          }
    }
    Unknown
  }
}