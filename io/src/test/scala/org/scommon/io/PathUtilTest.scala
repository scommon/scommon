package org.scommon.io

import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import java.util.UUID

import Utils._

@RunWith(classOf[JUnitRunner])
class PathUtilTest extends FunSuite
                     with Matchers
                     with SeveredStackTraces
                     with BeforeAndAfterAll {

  val PARENT_WORKING_DIR = Path(s"path-util-test").toUserTemp

  override protected def beforeAll() {
    PARENT_WORKING_DIR.mkdirs()
  }

  override protected def afterAll() {
    PARENT_WORKING_DIR.deleteAll
  }

  test("Deleting a full directory structure works") {
    val working = Path(PARENT_WORKING_DIR, s"${UUID.randomUUID().toString}")

    try {
      Path(Path(working, "A"), "1").touch should be (true)
      Path(Path(working, "A"), "2").touch should be (true)
      Path(Path(working, "A"), "3").touch should be (true)

      Path(Path(working, "B"), "1").touch should be (true)
      Path(Path(working, "B"), "2").touch should be (true)

      Path(Path(working, "C"), "1").touch should be (true)

      Path(working, "D").mkdirs should be (true)

      Path(Path(working, "A/A.A"), "1").touch should be (true)
      Path(Path(working, "A/A.B"), "1").touch should be (true)
      Path(Path(working, "A/A.C"), "1").touch should be (true)

      Path(Path(working, "A/A.A/A.A.A"), "1").touch should be (true)
      Path(Path(working, "A/A.A/A.A.A"), "2").touch should be (true)

      Path(working, "B/B.B/B.B.B/B.B.B.1").mkdirs should be (true)
      Path(working, "B/B.B/B.B.B/B.B.B.2").mkdirs should be (true)
      Path(working, "B/B.B/B.B.B/B.B.B.3").mkdirs should be (true)
    } finally {
      working.deleteAll should be (true)
      working.exists should be (false)
    }
  }

}
