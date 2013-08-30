package org.scommon.io

import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.concurrent.AsyncAssertions
import org.junit.runner.RunWith

import java.io.{PrintWriter, File}
import java.nio.file.{Path, Paths}
import java.util.concurrent.{Semaphore, TimeUnit}

import scala.collection._
import scala.concurrent.duration._

import org.apache.commons.io.FileUtils

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
  val TEMP_DIR = Paths.get(System.getProperty("java.io.tmpdir"), "directory-watcher-test").toFile

  override protected def beforeAll() {
    TEMP_DIR.exists() || TEMP_DIR.mkdirs() should be (true)
  }

  override protected def afterAll() {
    FileUtils.deleteDirectory(TEMP_DIR)
  }

  def createDir(root: Path = Paths.get(TEMP_DIR.toURI), path: String = ""): Path = {
    val p = root.resolve(path)
    val f = p.toFile
    if (f.exists())
      FileUtils.deleteDirectory(f)

    f.mkdirs() should be (true)
    p
  }

  def touch(f: File, time:Long = System.currentTimeMillis()): Boolean = {
    if (f.isDirectory) {
      if (!f.exists())
        f.mkdirs()
      f.setLastModified(time)
    } else {
      if (!f.exists()) {
        f.getParentFile.mkdirs()
        new PrintWriter(f).close()
      }
      f.setLastModified(time)
    }
  }

  implicit def failOnWatcherError(implicit w:Waiter): Watcher.ErrorReceived[Path] = (_, cause) => {
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

    touch(dirA.resolve("B").toFile)
    w.await()
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    createDir(dir, "C")
    w.await()
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    touch(dirA.resolve("D").toFile)
    w.await()
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    future.cancel(3.seconds) should be (true)

    w.await()
    wait_to_stop.tryAcquire(3L, TimeUnit.SECONDS) should be (true)
  }
}