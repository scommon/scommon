package org.scommon.io

import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import java.util.UUID

import org.scommon.datetime.DateTime

import Utils._

@RunWith(classOf[JUnitRunner])
class FileUtilTest extends FunSuite
                     with Matchers
                     with SeveredStackTraces
                     with BeforeAndAfterAll {

  val PARENT_WORKING_DIR = Path(s"file-util-test").toUserTemp

  override protected def beforeAll() {
    PARENT_WORKING_DIR.mkdirs()
  }

  override protected def afterAll() {
    PARENT_WORKING_DIR.deleteAll
  }

  test("Touch a file works") {
    val working = Path(PARENT_WORKING_DIR, s"${UUID.randomUUID().toString}")
    val file = Path(working, "test.txt")

    try {
      file.touch should be (true)
      file.exists should be (true)
      file.delete should be (true)

      file.touch(DateTime(2000, 1, 15, 8, 0, 0, 0).toInstant.getMillis) should be (true)
      file.exists should be (true)
    } finally {
      working.deleteAll should be (true)
    }
  }

}
