package org.scommon

import org.joda.time.{DateTime => JodaDateTime, ReadableDateTime, ReadableInstant}

package object datetime {
  val readableInstantOrdering = implicitly[Ordering[ReadableInstant]]

  implicit val ReadableDateTimeOrdering = new Ordering[ReadableDateTime] {
    def compare(a:ReadableDateTime, b:ReadableDateTime) = a.compareTo(b)
  }

  implicit val DateTimeOrdering = new Ordering[JodaDateTime] {
    def compare(a:JodaDateTime, b:JodaDateTime) = a.compareTo(b)
  }
}
