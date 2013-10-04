package org.scommon.security

import org.scalatest.junit.JUnitRunner
import org.scalatest.{SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers

import org.scommon.security2.{Permission, Permissions}


@RunWith(classOf[JUnitRunner])
class PermissionsTest extends FunSuite
with ShouldMatchers
with SeveredStackTraces {
  test("Basic permission creation works") {
    val p = Permission("java.util.PropertyPermission", "file.separator", "read")
    val ps = Permissions(p)
    p.validate(new java.util.PropertyPermission("file.separator", "read")) should be (true)
    ps.validate(new java.util.PropertyPermission("file.separator", "read")) should be (true)
  }

  test("Unknown permission throws exception") {
    intercept[IllegalArgumentException] {
      Permission("abc.def.UnknownPermission", "<name>", "<actions>")
    }
  }

  test("Valid class that does not implement the Permission trait throws exception") {
    intercept[IllegalArgumentException] {
      Permission(s"${classOf[ValidClassNotImplementingPermission].getName}", "<name>", "<actions>")
    }
  }
}

class ValidClassNotImplementingPermission(name: String, actions: String)