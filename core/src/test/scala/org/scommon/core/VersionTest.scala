package org.scommon.core

import org.scalatest.junit.JUnitRunner
import org.scalatest.{SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class VersionTest extends FunSuite
                     with ShouldMatchers
                     with SeveredStackTraces {
  private val FULL_VERSION_NUMBER:String = "1.2.3"

  test("Parsing full version number works") {
    Version.tryParse(FULL_VERSION_NUMBER).isDefined should be (true)
  }

  test("Parsing full version number as a string works") {
    Version(FULL_VERSION_NUMBER).asString() should be (FULL_VERSION_NUMBER)
  }

  test("Parsing partial version number as a string works") {
    Version("1.2.3") should be (Version(1, 2, 3))
    Version("1.2") should be (Version(1, 2))
    Version("1") should be (Version(1))
    Version("1").toString() should be ("1")
    Version("1.2").toString() should be ("1.2")
    Version("1.2.3").toString() should be ("1.2.3")
  }

  test("Full version number string conversion should be idempotent") {
    val TIMES:Int = 1000
    var value:String = FULL_VERSION_NUMBER
    for(i <- 0 until TIMES) {
      value = Version(value).asString()
      value should be (FULL_VERSION_NUMBER)
    }
  }

  test("Invalid numbers should not parse") {
    Version.tryParse("1.2.3A")                        should be (None) //Appended a non-digit to the end
    Version.tryParse("1.2_3")                         should be (None) //Period replaced with underscore
    Version.tryParse("1.2.")                          should be (None) //Left a trailing period
    Version.tryParse("1.A")                           should be (None) //Minor version replaced with non-digit
    Version.tryParse("1.")                            should be (None) //Left a trailing period
    Version.tryParse("1_")                            should be (None) //Left a trailing underscore
    Version.tryParse("?")                             should be (None) //Simple version, non-digit for major version
    Version.tryParse(" ")                             should be (None) //Just whitespace
    Version.tryParse("")                              should be (None) //Empty string
    Version.tryParse(null)                            should be (None) //Null version
  }

  test("Ordering is correct") {
    Version(1) < Version(2) should be (true)
    Version(2) > Version(1) should be (true)
    Version(1, 2) > Version(1) should be (true)
    Version(1, 2, 3) > Version(1, 2) should be (true)
    Version(1, 2, 3) < Version(1, 2, 4) should be (true)
    Version(1, 2, 3) < Version(1, 3) should be (true)
    Version(1, 2, 3) < Version(2) should be (true)
    Version(1, 2) < Version(1, 3, 3) should be (true)
    Version(1, 2) < Version(2) should be (true)
  }
}
