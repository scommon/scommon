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
    Version("1.2.3-RC2.1") should be (Version(1, 2, 3, "-RC2.1"))
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

  test("Basic extractor works") {
    Version(1, 2, 3, "-RC1") match {
      case Version(major, Some(minor), Some(revision), Some(annotation)) =>
        major should be (1)
        minor should be (2)
        revision should be (3)
        annotation should be ("-RC1")
      case _ =>
        fail("Extractor not working correctly")
    }

    Version(1, 2) match {
      case Version(major, Some(minor), revision @ None, annotation @ None) =>
        major should be (1)
        minor should be (2)
        revision should be (None)
        annotation should be (None)
      case _ =>
        fail("Extractor not working correctly")
    }

    Version(1) match {
      case Version(major, minor @ None, revision @ None, annotation @ None) =>
        major should be (1)
        minor should be (None)
        revision should be (None)
        annotation should be (None)
      case _ =>
        fail("Extractor not working correctly")
    }
  }

  test("Invalid numbers should not parse") {
    Version.tryParse("1.2.3.RC1")                     should be (None) //Period beginning an annotation
    Version.tryParse("1.2.3.")                        should be (None) //Period beginning an annotation and the rest blank
    Version.tryParse("1.2.")                          should be (None) //Left a trailing period
    Version.tryParse("1.A")                           should be (None) //Minor version replaced with non-digit
    Version.tryParse("1.")                            should be (None) //Left a trailing period
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
