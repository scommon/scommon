package org.scommon.reactive

import scala.collection._
import java.util.concurrent.locks.ReentrantLock

import org.scommon.core.Closeable

import scala.language.implicitConversions

object Generator {
  case class Received(data: Option[Any] = None, children: Seq[Option[Received]] = Seq())
  type BarrierProcessor = Received => Boolean
  type JunctionProcessor[+T] = Any => T

  private[reactive] trait Receiver {
    private[reactive] def receive(source: Generator[Any], data: Received): Unit
  }

  implicit def traversable2Generator[T](iter: Traversable[T]): Generator[T] = new Generator[T] {
    override val initial = iter.toIterable
  }

  implicit def generators2Embrace(g: Seq[Generator[Any]]): Embrace = EmbraceImpl(g)

  implicit def generator2Embrace(g: Generator[Any]): Embrace = EmbraceImpl(Seq(g))

  implicit def processor2BarrierMagnet(fn: BarrierProcessor): BarrierMagnet = new BarrierMagnet {
    def apply(data: Received) = fn(data)
  }

  implicit def processor2JunctionMagnet[T](fn: JunctionProcessor[T]): JunctionMagnet[T] = new JunctionMagnet[T] {
    def apply(data: Any): T = fn(data)
  }

  implicit def processor2JunctionMagnet[T](fn: PartialFunction[Any, T]): JunctionMagnet[T] = new JunctionMagnet[T] {
    def apply(data: Any): T = {
      if (!fn.isDefinedAt(data))
        throw new IllegalStateException(s"Partial function is not defined for $data")
      fn(data)
    }
  }

  implicit val waitForAll: BarrierMagnet = new BarrierMagnet {
    def apply(x: Received) = {
      var found = false
      val stack = mutable.ArrayStack[Received]()

      stack.push(x)

      while(stack.nonEmpty && !found) {
        val next = stack.pop()

        found = next.data.isEmpty || next.children.exists(c => {
          if (c.isDefined)
            stack.push(c.get)
          c.isEmpty
        })
      }
      !found
    }
  }
}

import Generator._

trait Generator[+TGenerate] extends Closeable {
  @volatile private[reactive] var started  = false
  private[reactive] val receivers_lock     = new ReentrantLock()
  private[reactive] val receivers          = mutable.LinkedHashSet[Receiver]()

  def ~:[T](g: Generator[T]): Embrace      = embrace_right_associative[T](g)
  def ~[T](g: Generator[T]): Embrace       = embrace_left_associative[T](g)
  def embrace[T](g: Generator[T]): Embrace = embrace_left_associative[T](g)


  private[reactive] def embrace_left_associative[T](generator: Generator[T]): Embrace =
    generators2Embrace(Seq(this, generator))

  private[reactive] def embrace_right_associative[T](generator: Generator[T]): Embrace =
    embrace_left_associative[T](generator)

  def close(): Unit = {}

  /** Monoidal in nature. */
  def initial: Iterable[TGenerate] =
    Iterable.empty

  final def push[T >: TGenerate](instance: T): Unit =
    push(Iterable(instance))

  final def push[T >: TGenerate](instances: Iterable[T]): Unit = {
    receivers_lock.lock()
    try {
      val to_send = Received(Some(instances))
      for(receiver <- receivers)
        receiver.receive(this, to_send)
    } finally {
      receivers_lock.unlock()
    }
  }

  private[reactive] def register(receiver: Receiver): Unit = {
    receivers_lock.lock()
    try {
      if (!receivers.contains(receiver))
        receivers += receiver
    } finally {
      receivers_lock.unlock()
    }
  }

  private[reactive] def start(): Unit = {
    require(!started, s"Cannot call start more than once on an instance of ${classOf[Generator[TGenerate]].getName}")

    push(initial)

    started = true
  }
}



trait Embrace extends Generator[Any] { self: Receiver =>
  import Generator._

  private[reactive] val generators: Seq[Generator[Any]] = Seq()
  private[reactive] val barriers: Seq[BarrierMagnet] = Seq()

  final def -(barrier: BarrierMagnet): Embrace                        = remove_left_associative(barrier)
  final def -[T](generator: Generator[T]): Embrace                    = remove_left_associative(generator)

  final override def ~[T](g: Generator[T]): Embrace                   = embrace_left_associative[T](g)
  final override def ~:[T](g: Generator[T]): Embrace                  = embrace_right_associative[T](g)
  final override def embrace[T](g: Generator[T]): Embrace             = embrace_left_associative[T](g)

  final def |>>(fn: BarrierProcessor): Barrier                        = barrier_left_associative(processor2BarrierMagnet(fn))
  final def <<|:(fn: BarrierProcessor): Barrier                       = barrier_right_associative(processor2BarrierMagnet(fn))
  final def barrier(fn: BarrierProcessor): Barrier                    = barrier_left_associative(processor2BarrierMagnet(fn))

  final def |>>(implicit magnet: BarrierMagnet): Barrier              = barrier_left_associative(magnet)
  final def <<|:(implicit magnet: BarrierMagnet): Barrier             = barrier_right_associative(magnet)
  final def barrier(implicit magnet: BarrierMagnet): Barrier          = barrier_left_associative(magnet)

  final def >>[T](fn: JunctionProcessor[T]): Junction                 = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def <<:[T](fn: JunctionProcessor[T]): Junction                = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: JunctionProcessor[T]): Junction           = junction_left_associative[T](processor2JunctionMagnet(fn))

  final def >>[T](fn: PartialFunction[Any, T]): Junction                 = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def <<:[T](fn: PartialFunction[Any, T]): Junction                = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: PartialFunction[Any, T]): Junction           = junction_left_associative[T](processor2JunctionMagnet(fn))

  final def >>[T](implicit magnet: JunctionMagnet[T]): Junction       = junction_left_associative[T](magnet)
  final def <<:[T](implicit magnet: JunctionMagnet[T]): Junction      = junction_right_associative[T](magnet)
  final def junction[T](implicit magnet: JunctionMagnet[T]): Junction = junction_left_associative[T](magnet)

  private[reactive] def remove_left_associative(barrier: BarrierMagnet): Embrace
  private[reactive] def remove_left_associative[T](generator: Generator[T]): Embrace

  private[reactive] def embrace_left_associative[T](generator: Generator[T]): Embrace
  private[reactive] def embrace_right_associative[T](generator: Generator[T]): Embrace

  private[reactive] def barrier_left_associative(implicit magnet: BarrierMagnet): Barrier
  private[reactive] def barrier_right_associative(implicit magnet: BarrierMagnet): Barrier

  private[reactive] def junction_left_associative[T](fn: JunctionMagnet[T]): Junction
  private[reactive] def junction_right_associative[T](fn: JunctionMagnet[T]): Junction

  /** Monoidal in nature. */
  final override def initial: Iterable[Any] =
    for(g <- generators)
      yield Some(g.initial)

  final private[reactive] override def register(receiver: Receiver): Unit = {
    super.register(receiver)
    for(g <- generators)
      g.register(this)
  }

  final private[reactive] override def start(): Unit = {
    require(!started, s"Cannot call start more than once on an instance of ${classOf[Embrace].getName}")

    for(g <- generators)
      g.start()

    started = true
  }

  final override def close(): Unit =
    for(g <- generators)
      g.close()
}

private[reactive] sealed case class EmbraceImpl(
  override val generators: Seq[Generator[Any]],
  override val barriers: Seq[BarrierMagnet] = Seq()
) extends Embrace with Receiver {
  import Generator._

  private[reactive] val receive_lock = new ReentrantLock()
  private[reactive] val receive_cache = mutable.LinkedHashMap[Generator[Any], Received]()


  private[reactive] def remove_left_associative(barrier: BarrierMagnet): Embrace =
    EmbraceImpl(generators, barriers diff Seq(barrier))

  private[reactive] def remove_left_associative[T](g: Generator[T]): Embrace =
    EmbraceImpl(generators diff Seq(g), barriers)


  private[reactive] override def embrace_left_associative[T](generator: Generator[T]): Embrace =
    EmbraceImpl(generators :+ generator, barriers)


  private[reactive] def barrier_left_associative(implicit magnet: BarrierMagnet): Barrier =
    BarrierImpl(this, barriers :+ magnet)

  private[reactive] def barrier_right_associative(implicit magnet: BarrierMagnet): Barrier =
    BarrierImpl(this, magnet +: barriers)


  private[reactive] def junction_left_associative[T](magnet: JunctionMagnet[T]): Junction =
    JunctionImpl(this, barriers, Seq(magnet))

  private[reactive] def junction_right_associative[T](magnet: JunctionMagnet[T]): Junction =
    JunctionImpl(this, barriers, Seq(magnet))


  private[reactive] def receive(source: Generator[Any], data: Received): Unit = {
    receive_lock.lock()
    try {

      receive_cache.update(source, data)
      val new_children =
        for {
          g <- generators
          c = receive_cache.getOrElse(g, Received())
        } yield Some(c)

      val received = Received(Some(new_children), new_children)

      val all_barriers_passed = !barriers.exists(!_(received))
      if (all_barriers_passed) {
        receivers.foreach(_.receive(this, received))
      }
    } finally {
      receive_lock.unlock()
    }
  }
}



trait BarrierMagnet extends BarrierProcessor

trait Barrier {
  private[reactive] val associated_embrace: Embrace
  private[reactive] val barriers: Seq[BarrierMagnet]

  final def -(barrier: BarrierMagnet): Barrier                        = remove_left_associative(barrier)

  final def ~:[T](g: Generator[T]): Embrace                           = embrace_right_associative[T](g)
  final def ~[T](g: Generator[T]): Embrace                            = embrace_left_associative[T](g)
  final def embrace[T](g: Generator[T]): Embrace                      = embrace_left_associative[T](g)

  final def <<|:(fn: BarrierProcessor): Barrier                       = barrier_right_associative(processor2BarrierMagnet(fn))
  final def |>>(fn: BarrierProcessor): Barrier                        = barrier_left_associative(processor2BarrierMagnet(fn))
  final def barrier(fn: BarrierProcessor): Barrier                    = barrier_left_associative(processor2BarrierMagnet(fn))

  final def <<|:(implicit magnet: BarrierMagnet): Barrier             = barrier_right_associative(magnet)
  final def |>>(implicit magnet: BarrierMagnet): Barrier              = barrier_left_associative(magnet)
  final def barrier(implicit magnet: BarrierMagnet): Barrier          = barrier_left_associative(magnet)

  final def <<:[T](fn: JunctionProcessor[T]): Junction                = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def >>[T](fn: JunctionProcessor[T]): Junction                 = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: JunctionProcessor[T]): Junction           = junction_left_associative[T](processor2JunctionMagnet(fn))

  //final def <<:[T](implicit magnet: JunctionMagnet[T]): Junction      = junction_right_associative[T](magnet)
  //final def >>[T](implicit magnet: JunctionMagnet[T]): Junction       = junction_left_associative[T](magnet)
  //final def junction[T](implicit magnet: JunctionMagnet[T]): Junction = junction_left_associative[T](magnet)

  private[reactive] def remove_left_associative(barrier: BarrierMagnet): Barrier

  private[reactive] def embrace_left_associative[T](generator: Generator[T]): Embrace
  private[reactive] def embrace_right_associative[T](generator: Generator[T]): Embrace

  private[reactive] def barrier_left_associative(implicit magnet: BarrierMagnet): Barrier
  private[reactive] def barrier_right_associative(implicit magnet: BarrierMagnet): Barrier

  private[reactive] def junction_left_associative[T](magnet: JunctionMagnet[T]): Junction
  private[reactive] def junction_right_associative[T](magnet: JunctionMagnet[T]): Junction
}

private[reactive] sealed case class BarrierImpl(
  override val associated_embrace: Embrace,
  override val barriers: Seq[BarrierMagnet]
) extends Barrier {

  private[reactive] def remove_left_associative(barrier: BarrierMagnet): Barrier =
    BarrierImpl(associated_embrace, barriers diff Seq(barrier))


  private[reactive] def embrace_left_associative[T](g: Generator[T]): Embrace =
    EmbraceImpl(Seq(EmbraceImpl(associated_embrace.generators, barriers), g))

  private[reactive] def embrace_right_associative[T](g: Generator[T]): Embrace =
    EmbraceImpl(Seq(g, EmbraceImpl(associated_embrace.generators, barriers)))


  private[reactive] def barrier_right_associative(implicit magnet: BarrierMagnet): Barrier =
    BarrierImpl(associated_embrace, magnet +: barriers)

  private[reactive] def barrier_left_associative(implicit magnet: BarrierMagnet): Barrier =
    BarrierImpl(associated_embrace, barriers :+ magnet)


  private[reactive]def junction_left_associative[T](magnet: JunctionMagnet[T]): Junction =
    JunctionImpl(associated_embrace, barriers, Seq(magnet))

  private[reactive]def junction_right_associative[T](magnet: JunctionMagnet[T]): Junction =
    JunctionImpl(associated_embrace, barriers, Seq(magnet))
}



trait JunctionCancellable {
  def cancel(): Unit
}

trait JunctionMagnet[+T] extends JunctionProcessor[T]

trait Junction extends Closeable {
  private[reactive] val associated_embrace: Embrace
  private[reactive] val associated_barriers: Seq[BarrierMagnet]
  private[reactive] val junctions: Seq[JunctionMagnet[Any]]

  final def -[TGenerator](generator: Generator[TGenerator]): Junction           = remove_left_associative(generator)
  final def -[TJunction](junction: JunctionMagnet[TJunction]): Junction         = remove_left_associative(junction)
  final def -(barrier: BarrierMagnet): Junction                                 = remove_left_associative(barrier)
  final def remove[TGenerator](generator: Generator[TGenerator]): Junction      = remove_left_associative(generator)
  final def remove[TJunction](junction: JunctionMagnet[TJunction]): Junction    = remove_left_associative(junction)
  final def remove(barrier: BarrierMagnet): Junction                            = remove_left_associative(barrier)

  final def >>[T](fn: JunctionProcessor[T]): Junction                           = junction_left_associative[T](processor2JunctionMagnet[T](fn))
  final def <<:[T](fn: JunctionProcessor[T]): Junction                          = junction_right_associative[T](processor2JunctionMagnet[T](fn))
  final def junction[T](fn: JunctionProcessor[T]): Junction                     = junction_left_associative[T](processor2JunctionMagnet[T](fn))

  final def >>[T](implicit magnet: JunctionMagnet[T]): Junction                 = junction_left_associative[T](magnet)
  final def <<:[T](implicit magnet: JunctionMagnet[T]): Junction                = junction_right_associative[T](magnet)
  final def junction[T](implicit magnet: JunctionMagnet[T]): Junction           = junction_left_associative[T](magnet)

  private[reactive] def remove_left_associative(barrier: BarrierMagnet): Junction
  private[reactive] def remove_left_associative[T](junction: JunctionMagnet[T]): Junction
  private[reactive] def remove_left_associative[T](generator: Generator[T]): Junction

  private[reactive] def junction_left_associative[T](magnet: JunctionMagnet[T]): Junction
  private[reactive] def junction_right_associative[T](magnet: JunctionMagnet[T]): Junction

  def begin: JunctionCancellable
}

private[reactive] sealed case class JunctionImpl(
  override val associated_embrace: Embrace,
  override val associated_barriers: Seq[BarrierMagnet],
  override val junctions: Seq[JunctionMagnet[Any]]
) extends Junction with Receiver {
  @volatile private[this] var cancelled = false


  private[reactive] def remove_left_associative(barrier: BarrierMagnet): Junction =
    JunctionImpl(associated_embrace, associated_barriers diff Seq(barrier), junctions)

  private[reactive] def remove_left_associative[T](junction: JunctionMagnet[T]): Junction =
    JunctionImpl(associated_embrace, associated_barriers, junctions diff Seq(junction))

  private[reactive] def remove_left_associative[T](generator: Generator[T]): Junction =
    JunctionImpl(EmbraceImpl(associated_embrace.generators diff Seq(generator), associated_embrace.barriers), associated_barriers, junctions)


  private[reactive] def junction_left_associative[T](magnet: JunctionMagnet[T]): Junction =
    JunctionImpl(associated_embrace, associated_barriers, junctions :+ magnet)

  private[reactive] def junction_right_associative[T](magnet: JunctionMagnet[T]): Junction =
    JunctionImpl(associated_embrace, associated_barriers, magnet +: junctions)


  private[reactive] def receive(source: Generator[Any], received: Received): Unit = {
    if (cancelled)
      return

    if (received.data.isEmpty)
      return

    val all_barriers_passed = !associated_barriers.exists(!_(received))
    if (all_barriers_passed) {
      val data = received.data.get
      //Lift the data out of Received. Takes something that looks like:
      //  <Sequence of Options of Receiveds>
      //  Seq(Some(Received(<data> Some(Seq("x", "y", "z")), <children> Seq())), Some(Received(<data> Some(Seq(1, 2, 3)), <children> Seq())))
      //and produces:
      //  Seq(Some(Seq("x", "y", "z")), Some(Seq(1, 2, 3)))
      //
      //Doing this is relatively easy -- the leaf nodes hold the data. We just
      //need to traverse the data structure and save off the data of any leaf
      //node we encounter.
      val stack = mutable.Stack[Received]()
      val lifted = mutable.Stack[Option[Any]]()

      stack.push(received)

      while(stack.nonEmpty) {
        val next = stack.pop()

        if (next.children.isEmpty)
          lifted.push(next.data)

        for(c <- next.children; n <- c)
         stack.push(n)
      }

      var last:Any = lifted.toSeq
      for (j <- junctions)
        last = j(last)
    }
  }

  final def begin: JunctionCancellable = {
    associated_embrace.register(this)
    associated_embrace.start()

    new JunctionCancellable {
      def cancel(): Unit = {
        cancelled = true
        close()
      }
    }
  }

  final override def close(): Unit = {
    associated_embrace.close()
  }
}