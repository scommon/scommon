package org.scommon.security

import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import net.datenwerke.sandbox.{SandboxedEnvironment, SandboxContext, SandboxServiceImpl}
import net.datenwerke.sandbox.SandboxContext.{Mode, AccessType}
import java.util.PropertyPermission
import net.datenwerke.sandbox.permissions.SecurityPermission
import java.io.{InputStreamReader, BufferedReader, Serializable}

import org.scommon.security.Sandbox._
import com.google.inject.Guice
import com.typesafe.config.ConfigFactory
import java.net._
import scala.Some


@RunWith(classOf[JUnitRunner])
class ClassLoaderTest extends FunSuite
with ShouldMatchers
with SeveredStackTraces {
  test("foo") {
    val profile = Sandbox.defaultProfile
    //profile.context.setMaximumRunTime(3000L)
    //profile.context.addSecurityPermission()
    //profile.context.setDebug(false)
    //profile.context.setRunInThread(false)

    val sandbox = Sandbox(profile)

    val result = sandbox.run {
      //println(s"${System.getProperties}")
      //System.exit(0)
      //println(s"${System.getProperty("os.vendor")}")

//      val h = new URL("http://www.google.com").openConnection()
//      val br = new BufferedReader(new InputStreamReader(h.getInputStream()))
//      println(br.readLine())
//      br.close()

//      //Create a socket
//      val s = new Socket("google.com", 80)
//      s.getOutputStream().write("GET / HTTP/1.0\n\n".getBytes("ASCII"))
//      val is = s.getInputStream()
//      is.read()
//      is.close()

//      //Can listen on a socket
//      val listen_sock = new ServerSocket(0)
//      println(s"Listening on ${listen_sock.getLocalPort()}")
//      val sock = listen_sock.accept()
//      val os = sock.getOutputStream()
//      val is = sock.getInputStream()
//      val echo = is.read()
//      os.write("hello ".getBytes("ASCII"))
//      os.write(echo)
//      is.close()

      //Can create a new thread.
      //Without a specified thread group, it should use the security manager's.
      //This will cause its uncaught exception handler to be used. But if you
      //specify your own thread group, our uncaught exception handler will not
      //be used.
      val t = new Thread(new Runnable {
        def run() {
          throw new IllegalStateException()
          for(i <- 0 to 20) {
            println(s"$i...")
            Thread.sleep(1000L)
          }
        }
      })
      t.start()
      t.join()
    }
    println(result)
  }
}