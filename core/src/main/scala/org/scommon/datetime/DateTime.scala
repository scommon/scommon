package org.scommon.datetime

import org.joda.time.{DateTime => JodaDateTime}
import org.joda.time.ReadableDateTime
import org.joda.time.format.DateTimeFormatter

object DateTime {
  def apply(str:String)
  = JodaDateTime.parse(str)

  def apply(formatter:DateTimeFormatter)(str:String)
  = JodaDateTime.parse(str, formatter)

  def apply(instant:Long)
  = new JodaDateTime(instant)

  def apply(year:Int   = 1970,
            month:Int  = 1,
            day:Int    = 1,
            hour:Int   = 0,
            minute:Int = 0,
            second:Int = 0,
            millis:Int = 0): ReadableDateTime
  = new JodaDateTime(year, month, day, hour, minute, second, millis)
}