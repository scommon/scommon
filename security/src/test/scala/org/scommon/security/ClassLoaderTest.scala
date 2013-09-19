package org.scommon.security

import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import net.datenwerke.sandbox.{SandboxedEnvironment, SandboxContext, SandboxServiceImpl}
import net.datenwerke.sandbox.SandboxContext.{Mode, AccessType}
import java.util.PropertyPermission
import net.datenwerke.sandbox.permissions.SecurityPermission
import java.io.Serializable


@RunWith(classOf[JUnitRunner])
class ClassLoaderTest extends FunSuite
with ShouldMatchers
with SeveredStackTraces {
  class Sandboxed() {
    def run(): Unit = {
      val service = SandboxServiceImpl.initLocalSandboxService()
      val context = Sandbox.default

      val result = service.runSandboxed(classOf[MySandboxedEnvironment], context, "some value")

      println(s"GOT: ${result.get()}")
    }
  }

  test("foo") {
    val default = Sandbox.default
    new Sandboxed().run()
  }
}

private[security] class MySandboxedEnvironment(value: String) extends SandboxedEnvironment[Boolean] {
  def execute(): Boolean = {
    try {
      import org.scommon.core._
      val c = scala.collection.mutable.LinkedHashSet("test")
      println(c.head.isNullOrEmpty)

      println(scala.util.Random.nextDouble())
      //Untrusted code
      //System.exit(-1)
      println(System.getProperties)
      //throw new IllegalArgumentException()

      true
    } catch {
      case t:Throwable =>
        t.printStackTrace()
        false
    }
  }
}