package org.scommon.io

import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.concurrent.AsyncAssertions
import org.junit.runner.RunWith

import java.nio.file.{Paths}
import java.util.concurrent.{Semaphore, TimeUnit}

import scala.collection._
import scala.concurrent.duration._

import Utils._

/**
 * @author David Hoyt &lt;dhoyt@hoytsoft.org&gt;
 */
@RunWith(classOf[JUnitRunner])
class WatcherTest
  extends FunSuite
  with ShouldMatchers
  with BeforeAndAfterAll
  with AsyncAssertions
{
  val PARENT_WORKING_DIR = Path(s"watcher-test").toUserTemp

  override protected def beforeAll() {
    PARENT_WORKING_DIR.exists() || PARENT_WORKING_DIR.mkdirs() should be (true)
  }

  override protected def afterAll() {
    PARENT_WORKING_DIR.deleteAll should be(true)
  }

  def createDir(root: java.nio.file.Path = Paths.get(PARENT_WORKING_DIR.toURI), path: String = ""): java.nio.file.Path = {
    val p = root.resolve(path)
    val f = p.toFile
    if (f.exists())
      f.deleteAll

    f.mkdirs() should be (true)
    p
  }

  implicit def failOnWatcherError(implicit w:Waiter): Watcher.ErrorReceived[java.nio.file.Path] = (_, cause) => {
    w {
      fail(cause)
    }
    w.dismiss()
  }

  test("Basic watcher for a directory works") {
    implicit val w = new Waiter
    val dir = createDir(path = "basic-watcher-works")

    val wait_to_start = new Semaphore(0)
    val wait_to_stop = new Semaphore(0)
    val wait_for_event_received = new Semaphore(0)
    val future = Watcher(Seq(dir), 10.milliseconds) { (root, source, event) =>
      println(s"event: $event")
      event match {
        case Watcher.STARTED => wait_to_start.release()
        case Watcher.CREATED => wait_for_event_received.release()
        case Watcher.STOPPED => wait_to_stop.release()
        case _ =>
      }
      w.dismiss()
    }

    //Wait 3 seconds and see if we've started.
    w.await()
    wait_to_start.tryAcquire(3L, TimeUnit.SECONDS) should be (true)

    val dirA = createDir(dir, "A")
    w.await()
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    dirA.resolve("B").toFile.touch
    w.await()
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    createDir(dir, "C")
    w.await()
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    dirA.resolve("D").toFile.touch
    w.await()
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    future.cancel(3.seconds) should be (true)

    w.await()
    wait_to_stop.tryAcquire(3L, TimeUnit.SECONDS) should be (true)
  }
}