package org.scommon.security2

import org.scalatest.junit.JUnitRunner
import org.scalatest.{SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class PermissionsTest extends FunSuite
with ShouldMatchers
with SeveredStackTraces {
  test("Basic permission creation works") {
    val p = Permission("java.util.PropertyPermission", "file.separator", "read")
    val ps = Permissions(p)
    p.validate(new java.util.PropertyPermission("file.separator", "read")) should be (true)
    p.validate(new java.util.PropertyPermission("file.separator", "write")) should be (false)
    ps.validate(new java.util.PropertyPermission("file.separator", "read")) should be (true)

    val p_type_name_only = Permission("java.lang.RuntimePermission", "accessDeclaredMembers")
    p_type_name_only.validate(new RuntimePermission("accessDeclaredMembers")) should be (true)
    p_type_name_only.validate(new RuntimePermission("exit")) should be (false)

    val p_type_only = Permission("some.custom.Permission")
    p_type_only.validate(new WrappedBasicPermission("some.custom.Permission")) should be (true)
    p_type_only.validate(new WrappedBasicPermission("some.other.custom.Permission")) should be (false)
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

  test("Sandbox does not require a security manager") {
    import Sandbox._

    SecurityManager.isAnyInstalled should be (false)
    Sandbox.run {
      SecurityManager.isAnyInstalled should be (false)
    }
  }
}

class ValidClassNotImplementingPermission(name: String, actions: String)