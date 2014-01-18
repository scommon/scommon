package org.scommon.script.engine.core

/**
 * Indicates a position in the source that could indicate an error, a warning, or information.
 */
sealed trait Position {
  def line: Int
  def column: Int

  def isKnown: Boolean = !isUnknown
  def isUnknown: Boolean = this match {
    case UnknownPosition => true
    case _ => line <= 0 && column <= 0
  }
}

object Position {
  private[this] sealed case class StandardPosition(
      line: Int
    , column: Int
  ) extends Position

  def apply(line: Int, column: Int): Position =
    StandardPosition(line, column)
}

case object UnknownPosition extends Position {
  val line = 0
  val column = 0
}