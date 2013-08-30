package org.scommon.io

import java.io.File
import java.nio.file._
import java.nio.file.WatchEvent.Kind
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.{TimeUnit, Callable, ThreadFactory, Executors}

import scala.util.control.Breaks._
import scala.concurrent.duration._
import scala.collection.JavaConversions._
import scala.concurrent.duration.Duration

import scala.language.implicitConversions
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.atomic.AtomicBoolean

object Watcher {
  type ErrorReceived[TRoot <: Watchable] = (TRoot, Throwable) => Unit
  type EventReceived[TRoot <: Watchable, TSource <: Watchable] = (TRoot, TSource, EventType) => Unit

  trait EventMagnet[TRoot <: Watchable, TSource <: Watchable] {
    def eventReceived(root: TRoot, source: TSource, eventType: EventType): Unit = {}
    def errorReceived(root: TRoot, error: Throwable): Unit = {}
  }

  trait EventListener[TRoot <: Watchable, TSource <: Watchable] extends EventMagnet[TRoot, TSource] {
    def started (root: TRoot, source: TSource):  Unit = {}
    def stopped (root: TRoot, source: TSource):  Unit = {}
    def created (root: TRoot, source: TSource):  Unit = {}
    def modified(root: TRoot, source: TSource):  Unit = {}
    def deleted (root: TRoot, source: TSource):  Unit = {}
    def error   (root: TRoot, error: Throwable): Unit = {}

    final override def errorReceived(root: TRoot, err: Throwable) =
      error(root, err)

    final override def eventReceived(root: TRoot, source: TSource, eventType: EventType) = eventType match {
      case STARTED  => started (root, source)
      case STOPPED  => stopped (root, source)
      case CREATED  => modified(root, source)
      case MODIFIED => created (root, source)
      case DELETED  => deleted (root, source)
    }
  }

  trait EventType extends Kind[AnyRef]
  private[this] class CustomKind(val name:String) extends EventType {
    def `type`() = classOf[AnyRef]
    override def toString = name
  }

  val STARTED:  EventType = new CustomKind("STARTED")
  val STOPPED:  EventType = new CustomKind("STOPPED")
  val CREATED:  EventType = new CustomKind("CREATED")
  val MODIFIED: EventType = new CustomKind("MODIFIED")
  val DELETED:  EventType = new CustomKind("DELETED")

  val ALL_KNOWN_EVENT_TYPES: Iterable[EventType] = Iterable(
      STARTED
    , CREATED
    , MODIFIED
    , DELETED
    , STOPPED
  )

  implicit val NOOP_ERRORRECEIVED: ErrorReceived[Watchable] = (_, _) => {}
  implicit val NOOP_EVENTRECEIVED: EventReceived[Watchable, Watchable] = (_, _, _) => {}
  implicit val NOOP_EVENTMAGNET  : EventMagnet[Watchable, Watchable]   = new EventMagnet[Watchable, Watchable] {}

  def singleEventReceived2EventMagnet[TRoot <: Watchable, TSource <: Watchable](
    callback: EventReceived[TRoot, TSource]
  ): EventMagnet[TRoot, TSource] = {
    new EventMagnet[TRoot, TSource] {
      final override def eventReceived(root: TRoot, source: TSource, eventType: EventType): Unit =
        callback(root, source, eventType)
    }
  }

  def singleErrorReceived2EventMagnet[TRoot <: Watchable](
    callback: ErrorReceived[TRoot]
  ): EventMagnet[TRoot, TRoot] = {
    new EventMagnet[TRoot, TRoot] {
      final override def errorReceived(root: TRoot, error: Throwable): Unit =
        callback(root, error)
    }
  }

  def eventReceivedAndErrorReceived2EventMagnet[TRoot <: Watchable, TSource <: Watchable](
    fnEvent: EventReceived[TRoot, TSource],
    fnError: ErrorReceived[TRoot]
  ): EventMagnet[TRoot, TSource] = {
    new EventMagnet[TRoot, TSource] {
      final override def eventReceived(root: TRoot, source: TSource, eventType: EventType): Unit =
        fnEvent(root, source, eventType)
      final override def errorReceived(root: TRoot, error: Throwable): Unit =
        fnError(root, error)
    }
  }

  implicit def multipleEventReceiveds2EventMagnet[TRoot <: Watchable, TSource <: Watchable](
    started:  EventReceived[TRoot, TSource] = NOOP_EVENTRECEIVED,
    stopped:  EventReceived[TRoot, TSource] = NOOP_EVENTRECEIVED,
    created:  EventReceived[TRoot, TSource] = NOOP_EVENTRECEIVED,
    modified: EventReceived[TRoot, TSource] = NOOP_EVENTRECEIVED,
    deleted:  EventReceived[TRoot, TSource] = NOOP_EVENTRECEIVED
  ): EventMagnet[TRoot, TSource] = {
    new EventListener[TRoot, TSource] {
      override def started (root: TRoot, source: TSource) = started (root, source)
      override def stopped (root: TRoot, source: TSource) = stopped (root, source)
      override def created (root: TRoot, source: TSource) = created (root, source)
      override def modified(root: TRoot, source: TSource) = modified(root, source)
      override def deleted (root: TRoot, source: TSource) = deleted (root, source)
    }
  }

  implicit def files2Watchable(files: TraversableOnce[File]): TraversableOnce[Watchable] =
    files.map(x => Paths.get(x.toURI))

  def apply[TWatchable <: Watchable](watched: TraversableOnce[TWatchable], fallbackDuration: Duration = 1.second)(fnEvent: EventReceived[TWatchable, TWatchable])(implicit fnError: ErrorReceived[TWatchable] = NOOP_ERRORRECEIVED): Watcher[TWatchable, TWatchable] =
    apply[TWatchable](watched, fallbackDuration, Executors.defaultThreadFactory())(eventReceivedAndErrorReceived2EventMagnet(fnEvent, fnError))

  def apply[TWatchable <: Watchable](watched: TraversableOnce[TWatchable], fallbackDuration: Duration, threadFactory: ThreadFactory)(implicit listener: EventMagnet[TWatchable, TWatchable]): Watcher[TWatchable, TWatchable] =
    new WatcherImpl[TWatchable, TWatchable](watched, listener, fallbackDuration, threadFactory)
}

trait Watcher[TWatchable <: Watchable, TSource <: Watchable] {
  def cancel(timeout:Duration = 0.seconds): Boolean
}

sealed class WatcherImpl[TWatchable <: Watchable, TSource <: Watchable] (
  watched:          TraversableOnce[TWatchable],
  listener:         Watcher.EventMagnet[TWatchable, TSource],
  fallbackDuration: Duration,
  threadFactory:    ThreadFactory
) extends Watcher[TWatchable, TSource] {

  require(fallbackDuration > 0.seconds, "The fallback duration must be greater than 0 seconds")

  private[this] val executor = Executors.newCachedThreadPool(threadFactory)
  private[this] val fallback_nanos = fallbackDuration.toNanos

  //Exception that we'll use to break out of the runner. Pre-create an instance that
  //we can re-use multiple times if necessary.
  private class WatchedCompletelyRemoved extends Throwable
  private object WatchedCompletelyRemoved {
    val instance = new WatchedCompletelyRemoved
  }

  //Because watched is a TraversableOnce[], be sure we iterate over the collection
  //only ONCE -- it's possible the collection can be traversed multiple times, but
  //we shouldn't make any assumptions.

  private[this] val tasks = (
    for (watch <- watched) yield {
      val stop = new AtomicBoolean(false)
      val future = executor.submit(new Callable[Unit] {
        def call(): Unit = {
          try {
            while(!stop.get()) {
              try {
                watch match {
                  case x: Path =>
                    if (x.toFile.exists()) {
                      run(watch)
                    } else {
                      LockSupport.parkNanos(fallback_nanos)
                      //Thread.sleep(fallback_millis, fallback_nanos)
                    }
                  case x: Watchable =>
                    run(watch)
                }
              } catch {
                case _:WatchedCompletelyRemoved =>
                  //Do nothing, just let it loop back around.
                case _:InterruptedException =>
                  stop.set(true)
              }
            }
          } catch {
            case _:InterruptedException =>
              //Do nothing, just exit.
            case t:Throwable =>
              throw t
          }
        }
      })
      (stop, future)
    }
  ).toIterable

  def cancel(timeout:Duration = 0.seconds):Boolean = {
    for ((stop, future) <- tasks) {
      stop.set(true)
      future.cancel(true)
    }
    executor.shutdownNow()
    executor.awaitTermination(timeout.toNanos, TimeUnit.NANOSECONDS)
  }

  private[this] def run(watch: TWatchable) = {
    var keys: Map[WatchKey, Watchable] = Map()
    var watcher_service:WatchService = null

    try {
      watcher_service = FileSystems.getDefault.newWatchService()

      def register(watchItem: Watchable) = {
        keys += watchItem.register(watcher_service, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY) -> watchItem
      }

      def registerPath(path: Path) = {
        Files.walkFileTree(path, new SimpleFileVisitor[Path]() {
          override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
            register(dir)
            FileVisitResult.CONTINUE
          }
        })
      }

      watch match {
        case x: Path => registerPath(x)
        case x: Watchable => register(x)
      }

      try {
        //Ensure that STARTED is always sent first. This still happens even though we've already
        //registered b/c we don't block for work until we call .take() later on.
        listener.eventReceived(watch, watch.asInstanceOf[TSource], Watcher.STARTED)
      } catch {
        case t:InterruptedException =>
          throw t
        case t:Throwable =>
          listener.errorReceived(watch, t)
      }

      breakable {
        while (true) {
          //Wait for key to be signalled.
          var key: WatchKey = null
          try {
            key = watcher_service.take()
          } catch {
            case _:Throwable =>
              break()
          }

          keys.get(key) match {
            case Some(value) => {
              for (ev <- key.pollEvents().toIterable) {
                val kind = ev.kind()
                kind match {
                  case ENTRY_MODIFY | ENTRY_CREATE | ENTRY_DELETE =>
                    value match {
                      case dir:Path => {
                        val name = ev.context().asInstanceOf[Path]
                        val child = dir.resolve(name)

                        // if directory is created, and watching recursively, then
                        // register it and its sub-directories
                        if (kind == ENTRY_CREATE) {
                          try {
                            if (Files.isDirectory(child, LinkOption.NOFOLLOW_LINKS))
                              registerPath(child)
                          } catch {
                            case _:Throwable =>
                              break()
                          }
                        }
                      }
                    }

                    val equivalent = kind match {
                      case ENTRY_MODIFY => Watcher.MODIFIED
                      case ENTRY_CREATE => Watcher.CREATED
                      case ENTRY_DELETE => Watcher.DELETED
                    }

                    //let everyone know about it.
                    listener.eventReceived(watch, value.asInstanceOf[TSource], equivalent)
                }
              }

              //Reset key and remove from set if watched is no longer accessible
              if (!key.reset()) {
                keys = keys - key

                //All watches are inaccessible
                if (keys.isEmpty) {
                  throw WatchedCompletelyRemoved.instance
                }
              }
            }
            case None =>
          }
        }
      }
    } catch {
      case t:InterruptedException =>
        throw t
      case t:Throwable =>
        listener.errorReceived(watch, t)
    } finally {
      if (watcher_service != null) {
        watcher_service.close()
      }
      //Stopped should always come last.
      listener.eventReceived(watch, watch.asInstanceOf[TSource], Watcher.STOPPED)
    }
  }
}