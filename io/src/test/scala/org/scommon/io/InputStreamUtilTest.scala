package org.scommon.io

import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import java.util.UUID

import org.scommon.core._
import org.scommon.io._
import Utils._

@RunWith(classOf[JUnitRunner])
class InputStreamUtilTest extends FunSuite
                     with Matchers
                     with SeveredStackTraces
                     with BeforeAndAfterAll {

  val PARENT_WORKING_DIR = Path(s"input-stream-util-test").toUserTemp

  override protected def beforeAll() {
    PARENT_WORKING_DIR.mkdirs()
  }

  override protected def afterAll() {
    PARENT_WORKING_DIR.deleteAll
  }

  test("Input and output stream implicits work correctly") {
    val working = Path(PARENT_WORKING_DIR, s"${UUID.randomUUID().toString}")

    try {
      for(w <- Path(working, "1").openForWrite.toBufferedWriter) {
        //By default the file and all its parent directories should be created.
        //By default the file should be closed automatically.
        w.write("line #1"); w.newLine()
        w.write("line #2"); w.newLine()
        w.write("line #3"); w.newLine()
      }

      for {
        r <- Path(working, "1").openForRead.toBufferedReader
        line1 = r.readLine()
        line2 = r.readLine()
      }{
        val line3 = r.readLine()
        line1 should equal("line #1")
        line2 should equal("line #2")
        line3 should equal("line #3")
      }

    } catch {
      case t:Throwable =>
        t.printStackTrace()
        fail(t)
    } finally {
      working.deleteAll should be (true)
      working.exists should be (false)
    }
  }

}
