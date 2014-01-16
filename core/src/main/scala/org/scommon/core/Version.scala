package org.scommon.core

import org.joda.time.{ReadableDateTime, DateTime => JodaDateTime}
import java.util.Date
import scala.util.matching.Regex

/**
 * Works with version numbers of the following format:
 *     major.minor.revision
 *
 * For example:
 *     1.0.0
 *
 * Implementers of this interface should be considered immutable and safe for concurrent access.
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
trait Version extends Ordered[Version] {
  def major:Int
  def minor:Option[Int]
  def revision:Option[Int]
  def annotation:Option[String]

  def asString():String
  def asString(builder:StringBuilder):StringBuilder
}

/**
 * Works with version numbers as specified by [[org.scommon.core.Version]]
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
object CommonVersion {
  import StringUtil._

  private val REGEX_VERSION_STRING_MAJOR_MINOR_REVISION_ANNOTATION          = """(\d+)\.(\d+)\.(\d+)([^.].*)""".r
  private val REGEX_VERSION_STRING_MAJOR_MINOR_REVISION                     = """(\d+)\.(\d+)\.(\d+)""".r
  private val REGEX_VERSION_STRING_MAJOR_MINOR_ANNOTATION                   = """(\d+)\.(\d+)([^.].*)""".r
  private val REGEX_VERSION_STRING_MAJOR_MINOR                              = """(\d+)\.(\d+)""".r
  private val REGEX_VERSION_STRING_MAJOR_ANNOTATION                         = """(\d+)([^.].*)""".r
  private val REGEX_VERSION_STRING_MAJOR                                    = """(\d+)""".r

  private val REGEX_VERSION_STRING_MATCH_LIST_IN_ORDER:Seq[Regex]           = Seq(
      REGEX_VERSION_STRING_MAJOR_MINOR_REVISION_ANNOTATION
    , REGEX_VERSION_STRING_MAJOR_MINOR_REVISION
    , REGEX_VERSION_STRING_MAJOR_MINOR_ANNOTATION
    , REGEX_VERSION_STRING_MAJOR_MINOR
    , REGEX_VERSION_STRING_MAJOR_ANNOTATION
    , REGEX_VERSION_STRING_MAJOR
  )

  def parse(version:String):CommonVersion = {
    val parsed = tryParse(version)
    require(parsed.isDefined, s"Invalid version number: $version. Expected format: #.#.#")
    parsed.get
  }

  def tryParse(version:String):Option[CommonVersion] = {
    if (version.isNullOrEmpty) {
      None
    } else {
      for {
        p <- REGEX_VERSION_STRING_MATCH_LIST_IN_ORDER
        m = p.pattern.matcher(version) if m.matches()
        count = m.groupCount()
      } {
        count match {
          case 4 =>
            return Some(new CommonVersion(parseInt(m.group(1)), Some(parseInt(m.group(2))), Some(parseInt(m.group(3))), Some(m.group(4))))
          case 3 if p == REGEX_VERSION_STRING_MAJOR_MINOR_REVISION =>
            return Some(new CommonVersion(parseInt(m.group(1)), Some(parseInt(m.group(2))), Some(parseInt(m.group(3))), None))
          case 3 if p == REGEX_VERSION_STRING_MAJOR_MINOR_ANNOTATION =>
            return Some(new CommonVersion(parseInt(m.group(1)), Some(parseInt(m.group(2))), None, Some(m.group(3))))
          case 2 if p == REGEX_VERSION_STRING_MAJOR_MINOR =>
            return Some(new CommonVersion(parseInt(m.group(1)), Some(parseInt(m.group(2))), None, None))
          case 2 if p == REGEX_VERSION_STRING_MAJOR_ANNOTATION =>
            return Some(new CommonVersion(parseInt(m.group(1)), None, None, Some(m.group(2))))
          case 1 =>
            return Some(new CommonVersion(parseInt(m.group(1)), None, None, None))
        }
      }

      None
    }
  }

  def isValid(version:String) = tryParse(version).isDefined

  private def validateNumbers(major:Int, values:Option[Int]*) = {
    require(major >= 0, "Every part of the version must be a positive integer")
    for (value <- values; part <- value) {
      require(part >= 0, "Every part of the version must be a positive integer")
    }
  }

  def parseInt(value:String):Int =
    try {
      value.toInt
    } catch {
      case _:Throwable => -1
    }
}

/**
 * Works with version numbers as specified by [[org.scommon.core.Version]]
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
class CommonVersion(val major:Int, val minor:Option[Int], val revision:Option[Int], val annotation:Option[String] = None) extends Version {
  import CommonVersion._

  require((minor.isDefined && revision.isDefined) || (minor.isDefined && revision.isEmpty) || (minor.isEmpty && revision.isEmpty), "Minor must also be specified")
  validateNumbers(major, minor, revision)

  def asString(builder:StringBuilder):StringBuilder = {
    val b = if (builder eq null) new StringBuilder(128) else builder
    b.append(asString())
  }

  def asString():String = //major.minor.revision
    s"$major" +
    (if (minor.isDefined) s".${minor.get}" else StringUtil.empty) +
    (if (revision.isDefined) s".${revision.get}" else StringUtil.empty) +
    (if (annotation.isDefined) s"${annotation.get}" else StringUtil.empty)

  override def toString:String = asString()

  override def equals(o:Any):Boolean = o match {
    case v:Version => major == v.major && minor == v.minor && revision == v.revision && annotation == v.annotation
    case _ => false
  }

  override def hashCode:Int = {
    var result = major
    result = 31 * result + minor.hashCode()
    result = 31 * result + revision.hashCode()
    result = 31 * result + annotation.hashCode()
    result
  }

  def compare(that: Version): Int = Version.compare(this, that)
}

/**
 * Works with version numbers as specified by [[org.scommon.core.Version]]
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
object Version extends Ordering[Version] {
  def apply(major:Int) = new CommonVersion(major, None, None, None)
  def apply(major:Int, minor:Int) = new CommonVersion(major, Some(minor), None, None)
  def apply(major:Int, minor:Int, maintenance:Int) = new CommonVersion(major, Some(minor), Some(maintenance), None)
  def apply(major:Int, annotation:String) = new CommonVersion(major, None, None, Some(annotation))
  def apply(major:Int, minor:Int, annotation:String) = new CommonVersion(major, Some(minor), None, Some(annotation))
  def apply(major:Int, minor:Int, maintenance:Int, annotation:String) = new CommonVersion(major, Some(minor), Some(maintenance), Some(annotation))
  def apply(v: String):Version = CommonVersion.parse(v)
  def tryParse(v:String):Option[Version] = CommonVersion.tryParse(v)

  def unapply(v: Version):Option[(Int, Option[Int], Option[Int], Option[String])] =
    Some(v.major, v.minor, v.revision, v.annotation)

  def compare(x: Version, y: Version) = {
    if (x.major < y.major) -1
    else if (x.major > y.major) 1
    else {
      if (x.minor.isEmpty && y.minor.isDefined) -1
      else if (x.minor.isDefined && y.minor.isEmpty) 1
      else {
        val x_minor = x.minor.get
        val y_minor = y.minor.get
        if (x_minor < y_minor) -1
        else if (x_minor > y_minor) 1
        else {
          if (x.revision.isEmpty && y.revision.isDefined) -1
          else if (x.revision.isDefined && y.revision.isEmpty) 1
          else {
            val x_revision = x.revision.get
            val y_revision = y.revision.get
            if (x_revision < y_revision) -1
            else if (x_revision > y_revision) 1
            else 0
          }
        }
      }
    }
  }
}
