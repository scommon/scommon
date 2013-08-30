package org.scommon.core

import language.implicitConversions

/**
 * Extensions for String and Option[String].
 */
object StringUtil {
  val empty = ""
  val newLine = querySystemNewLine

  /** Requests the system's default new line character(s). Could be \n (Unix) or \r\n (Windows) or something else. **/
  @inline def querySystemNewLine = System.getProperty("line.separator")

  /** Returns if the string is either null or empty. */
  @inline def isNullOrEmpty(s: Option[String]): Boolean = isNoneOrEmpty(s)

  /** Returns if the string is either null or empty. */
  @inline def isNullOrEmpty(s: String): Boolean = s == null /* ignore style check */ || s.isEmpty || (empty eq s) || empty == s

  /** Returns if the string is either null, None, or empty. */
  @inline def isNoneOrEmpty(s: Option[String]): Boolean = s == null /* ignore style check */ || s.isEmpty || (empty eq s.get) || empty == s.get

  /** Returns if the string is not null and not empty. */
  @inline def isNonEmpty(s: String): Boolean = !isNullOrEmpty(s)

  /** Returns if the string is not null, not None, and not empty. */
  @inline def isNonEmpty(s: Option[String]): Boolean = !isNoneOrEmpty(s)

  /** Returns the string if it is not null and not empty, otherwise the empty string. */
  @inline def checked(s: String): String = if (isNullOrEmpty(s)) empty else s

  /** Returns the string if it is not null, not None, and not empty, otherwise the empty string. */
  @inline def checked(s: Option[String]): String = if (isNoneOrEmpty(s)) empty else s.get

  /** Returns a string that represents a class' package name and it's simple name appended with a period (.). */
  def toKeyPrefix(c: Class[_]): String = {
    val package_name = c.getPackage.getName
    val simple_name = c.getSimpleName
    val name = if (simple_name.endsWith("$")) simple_name.substring(0, simple_name.length - "$".length) else simple_name
    package_name + "." + name + "."
  }

  /** Returns the string as a valid Java/Scala identifier. */
  def toValidIdentifier(s: String): String = {

    def toValidIdentifier0(s: String): String =
      s.map(c => if (isValidChar(c)) c else '_')

    def isValidChar(c: Char): Boolean =
      Character.isLetterOrDigit(c) || c == '_' || c == '$'

    if (isNonEmpty(s)) {
      if (Character.isLetter(s.head)) {
        toValidIdentifier0(s)
      } else if (Character.isDigit(s.head)) {
        "_" + toValidIdentifier0(s)
      } else {
        toValidIdentifier0(s)
      }
    } else {
      "_"
    }
  }
}