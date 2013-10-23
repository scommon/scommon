package org.scommon.security2

import org.scalatest.junit.JUnitRunner
import org.scalatest.{SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import java.security.AccessControlException

@RunWith(classOf[JUnitRunner])
class PermissionsTest extends FunSuite
with ShouldMatchers
with SeveredStackTraces {
//  test("Basic permission creation works") {
//    val p = Permission("java.util.PropertyPermission", "file.separator", "read")
//    val ps = Permissions(p)
//    p.validate(new java.util.PropertyPermission("file.separator", "read")) should be (true)
//    p.validate(new java.util.PropertyPermission("file.separator", "write")) should be (false)
//    ps.validate(new java.util.PropertyPermission("file.separator", "read")) should be (true)
//
//    val p_type_name_only = Permission("java.lang.RuntimePermission", "accessDeclaredMembers")
//    p_type_name_only.validate(new RuntimePermission("accessDeclaredMembers")) should be (true)
//    p_type_name_only.validate(new RuntimePermission("exit")) should be (false)
//
//    val p_type_only = Permission("some.custom.Permission")
//    p_type_only.validate(new WrappedBasicPermission("some.custom.Permission")) should be (true)
//    p_type_only.validate(new WrappedBasicPermission("some.other.custom.Permission")) should be (false)
//  }
//
//  test("Unknown permission throws exception") {
//    intercept[IllegalArgumentException] {
//      Permission("abc.def.UnknownPermission", "<name>", "<actions>")
//    }
//  }
//
//  test("Valid class that does not implement the Permission trait throws exception") {
//    intercept[IllegalArgumentException] {
//      Permission(s"${classOf[ValidClassNotImplementingPermission].getName}", "<name>", "<actions>")
//    }
//  }
//
//  test("Sandbox run operates with a security manager") {
//    implicit val permissive = CONTEXT_PERMISSIVE
//    Sandbox.run {
//      System.getSecurityManager().isInstanceOf[SecurityManager]
//      Sandbox.securityManager.hasContext should be (true)
//    }
//  }
//
//  test("Switching between unsecure and secure works") {
//    implicit val permissive = CONTEXT_PERMISSIVE
//    for(_ <- 0 until 50) {
//      Sandbox.runUnsecurely {
//        Sandbox.securityManager.hasContext should be (false)
//      }
//      Sandbox.runSecurely {
//        Sandbox.securityManager.hasContext should be (true)
//      }
//    }
//  }

  test("Cannot run an insecure run within a secure run") {
    implicit val permissive = CONTEXT_PERMISSIVE
//    intercept[AccessControlException] {
//      Sandbox.runSecurely {
//        Sandbox.runUnsecurely {
//          fail("Somehow an insecure run was allowed from within a secure one.")
//        }
//      }
//    }

    val t = new Thread(new Runnable {
      def run() = intercept[AccessControlException] {
        Sandbox.runUnsecurely {
          fail("Should be unable to run something in a sandbox if currently running something else.")
        }
      }
    })
    Sandbox.runSecurely {
      //Launch a new thread.
      t.start()
      t.join()
    }
  }
}

class ValidClassNotImplementingPermission(name: String, actions: String)