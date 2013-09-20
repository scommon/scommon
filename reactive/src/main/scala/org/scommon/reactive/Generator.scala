package org.scommon.reactive

import scala.collection._
import java.util.concurrent.locks.ReentrantLock

import org.scommon.core.Closeable

import scala.language.implicitConversions

object Generator {
  case class Received(data: Option[Any] = None, children: Seq[Option[Received]] = Seq())
  type BarrierProcessor[TContext] = (Option[TContext], Received) => Boolean
  type JunctionProcessor[TContext, +T] = (Option[TContext], Any) => T
  type SimpleJunctionProcessor[+T] = (Any) => T

  private[reactive] trait Receiver[TContext] {
    private[reactive] def receive(source: Generator[Any, TContext], context: Option[TContext], data: Received): Unit
  }

  implicit def traversable2Generator[T](iter: Traversable[T]): Generator[T, Nothing] = new Generator[T, Nothing] {
    override val initial = iter.toIterable
  }

  implicit def generators2Embrace[TContext](g: Seq[Generator[Any, TContext]]): Embrace[TContext] =
    EmbraceImpl(g)

  implicit def generator2Embrace[TContext](g: Generator[Any, TContext]): Embrace[TContext] =
    EmbraceImpl(Seq(g))

  implicit def processor2BarrierMagnet[TContext](fn: BarrierProcessor[TContext]): BarrierMagnet[TContext] = new BarrierMagnet[TContext] {
    def apply(context: Option[TContext], data: Received) =
      fn(context, data)
  }

  implicit def processor2JunctionMagnet[TContext, T](fn: SimpleJunctionProcessor[T]): JunctionMagnet[TContext, T] = new JunctionMagnet[TContext, T] {
    def apply(context: Option[TContext], data: Any): T =
      fn(data)
  }

  implicit def processor2JunctionMagnet[TContext, T](fn: JunctionProcessor[TContext, T]): JunctionMagnet[TContext, T] = new JunctionMagnet[TContext, T] {
    def apply(context: Option[TContext], data: Any): T =
      fn(context, data)
  }

  implicit def processor2JunctionMagnet[TContext, T](fn: PartialFunction[(Option[TContext], Any), T]): JunctionMagnet[TContext, T] = new JunctionMagnet[TContext, T] {
    def apply(context: Option[TContext], data: Any): T = {
      val tuple = (context, data)
      if (!fn.isDefinedAt(tuple))
        throw new IllegalStateException(s"Partial function is not defined for $data")
      fn(tuple)
    }
  }

  val waitForAll: BarrierMagnet[Nothing] = waitForAllWithContext[Nothing]

  def waitForAllWithContext[TContext]: BarrierMagnet[TContext] = new BarrierMagnet[TContext] {
    def apply(c: Option[TContext], x: Received) = {
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

trait Generator[+TGenerate, TContext] extends Closeable {
  @volatile private[this] var default_context: Option[TContext] = None
  @volatile private[reactive] var started                       = false
  private[reactive] val receivers_lock                          = new ReentrantLock()
  private[reactive] val receivers                               = mutable.LinkedHashSet[Receiver[TContext]]()

  def ~:[T](g: Generator[T, TContext]): Embrace[TContext]      = embrace_right_associative[T](g)
  def ~[T](g: Generator[T, TContext]): Embrace[TContext]       = embrace_left_associative[T](g)
  def embrace[T](g: Generator[T, TContext]): Embrace[TContext] = embrace_left_associative[T](g)


  private[reactive] def embrace_left_associative[T](generator: Generator[T, TContext]): Embrace[TContext] =
    generators2Embrace(Seq(this, generator))

  private[reactive] def embrace_right_associative[T](generator: Generator[T, TContext]): Embrace[TContext] =
    embrace_left_associative[T](generator)

  def close(): Unit = {}

  def withDefaultContext(context: TContext): this.type = {
    default_context = Some(context)
    this
  }

  def defaultContext: Option[TContext] =
    default_context

  /** Monoidal in nature. */
  def initial: Iterable[TGenerate] =
    Iterable.empty

  final def push[T >: TGenerate](instance: T): Unit =
    push(defaultContext, Iterable(instance))

  final def push[T >: TGenerate](instances: Iterable[T]): Unit =
    push(defaultContext, instances)

  final def push[T >: TGenerate](context: Option[TContext], instances: Iterable[T]): Unit = {
    receivers_lock.lock()
    try {
      val to_send = Received(Some(instances))
      for(receiver <- receivers)
        receiver.receive(this, context, to_send)
    } finally {
      receivers_lock.unlock()
    }
  }

  private[reactive] def register(receiver: Receiver[TContext]): Unit = {
    receivers_lock.lock()
    try {
      if (!receivers.contains(receiver))
        receivers += receiver
    } finally {
      receivers_lock.unlock()
    }
  }

  private[reactive] def start(): Unit = {
    require(!started, s"Cannot call start more than once on an instance of ${classOf[Generator[TGenerate, TContext]].getName}")

    push(initial)

    started = true
  }
}



trait Embrace[TContext] extends Generator[Any, TContext] { self: Receiver[TContext] =>
  import Generator._

  private[reactive] val generators: Seq[Generator[Any, TContext]] = Seq()
  private[reactive] val barriers: Seq[BarrierMagnet[TContext]] = Seq()

  final def -(barrier: BarrierMagnet[TContext]): Embrace[TContext]                           = remove_left_associative(barrier)
  final def -[T](generator: Generator[T, TContext]): Embrace[TContext]                       = remove_left_associative(generator)

  final override def ~:[T](g: Generator[T, TContext]): Embrace[TContext]                     = embrace_right_associative[T](g)
  final override def ~[T](g: Generator[T, TContext]): Embrace[TContext]                      = embrace_left_associative[T](g)
  final override def embrace[T](g: Generator[T, TContext]): Embrace[TContext]                = embrace_left_associative[T](g)

  final def <<|:(fn: BarrierProcessor[TContext]): Barrier[TContext]                          = barrier_right_associative(processor2BarrierMagnet(fn))
  final def |>>(fn: BarrierProcessor[TContext]): Barrier[TContext]                           = barrier_left_associative(processor2BarrierMagnet(fn))
  final def barrier(fn: BarrierProcessor[TContext]): Barrier[TContext]                       = barrier_left_associative(processor2BarrierMagnet(fn))

  final def <<|:(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]                = barrier_right_associative(magnet)
  final def |>>(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]                 = barrier_left_associative(magnet)
  final def barrier(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]             = barrier_left_associative(magnet)

  final def <<:[T](fn: SimpleJunctionProcessor[T]): Junction[TContext]                       = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def >>[T](fn: SimpleJunctionProcessor[T]): Junction[TContext]                        = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: SimpleJunctionProcessor[T]): Junction[TContext]                  = junction_left_associative[T](processor2JunctionMagnet(fn))

  final def <<:[T](fn: JunctionProcessor[TContext, T]): Junction[TContext]                   = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def >>[T](fn: JunctionProcessor[TContext, T]): Junction[TContext]                    = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: JunctionProcessor[TContext, T]): Junction[TContext]              = junction_left_associative[T](processor2JunctionMagnet(fn))

  final def <<:[T](fn: PartialFunction[(Option[TContext], Any), T]): Junction[TContext]      = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def >>[T](fn: PartialFunction[(Option[TContext], Any), T]): Junction[TContext]       = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: PartialFunction[(Option[TContext], Any), T]): Junction[TContext] = junction_left_associative[T](processor2JunctionMagnet(fn))

  final def <<:[T](implicit magnet: JunctionMagnet[TContext, T]): Junction[TContext]         = junction_right_associative[T](magnet)
  final def >>[T](implicit magnet: JunctionMagnet[TContext, T]): Junction[TContext]          = junction_left_associative[T](magnet)
  final def junction[T](implicit magnet: JunctionMagnet[TContext, T]): Junction[TContext]    = junction_left_associative[T](magnet)

  private[reactive] def remove_left_associative(barrier: BarrierMagnet[TContext]): Embrace[TContext]
  private[reactive] def remove_left_associative[T](generator: Generator[T, TContext]): Embrace[TContext]

  private[reactive] def embrace_left_associative[T](generator: Generator[T, TContext]): Embrace[TContext]
  private[reactive] def embrace_right_associative[T](generator: Generator[T, TContext]): Embrace[TContext]

  private[reactive] def barrier_left_associative(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]
  private[reactive] def barrier_right_associative(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]

  private[reactive] def junction_left_associative[T](fn: JunctionMagnet[TContext, T]): Junction[TContext]
  private[reactive] def junction_right_associative[T](fn: JunctionMagnet[TContext, T]): Junction[TContext]

  /** Monoidal in nature. */
  final override def initial: Iterable[Any] =
    for(g <- generators)
      yield Some(g.initial)

  final private[reactive] override def register(receiver: Receiver[TContext]): Unit = {
    super.register(receiver)
    for(g <- generators)
      g.register(this)
  }

  final private[reactive] override def start(): Unit = {
    require(!started, s"Cannot call start more than once on an instance of ${classOf[Embrace[TContext]].getName}")

    for(g <- generators)
      g.start()

    started = true
  }

  final override def close(): Unit =
    for(g <- generators)
      g.close()
}

private[reactive] sealed case class EmbraceImpl[TContext](
    override val generators: Seq[Generator[Any, TContext]]
  , override val barriers: Seq[BarrierMagnet[TContext]] = Seq()
) extends Embrace[TContext] with Receiver[TContext] {
  import Generator._

  private[reactive] val receive_lock = new ReentrantLock()
  private[reactive] val receive_cache = mutable.LinkedHashMap[Generator[Any, TContext], Received]()


  private[reactive] def remove_left_associative(barrier: BarrierMagnet[TContext]): Embrace[TContext] =
    EmbraceImpl(generators, barriers diff Seq(barrier))

  private[reactive] def remove_left_associative[T](g: Generator[T, TContext]): Embrace[TContext] =
    EmbraceImpl(generators diff Seq(g), barriers)


  private[reactive] override def embrace_left_associative[T](generator: Generator[T, TContext]): Embrace[TContext] =
    EmbraceImpl(generators :+ generator, barriers)


  private[reactive] def barrier_left_associative(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext] =
    BarrierImpl(this, barriers :+ magnet)

  private[reactive] def barrier_right_associative(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext] =
    BarrierImpl(this, magnet +: barriers)


  private[reactive] def junction_left_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext] =
    JunctionImpl(this, barriers, Seq(magnet))

  private[reactive] def junction_right_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext] =
    JunctionImpl(this, barriers, Seq(magnet))


  private[reactive] def receive(source: Generator[Any, TContext], context: Option[TContext], data: Received): Unit = {
    receive_lock.lock()
    try {

      receive_cache.update(source, data)
      val new_children =
        for {
          g <- generators
          c = receive_cache.getOrElse(g, Received())
        } yield Some(c)

      val received = Received(Some(new_children), new_children)

      val all_barriers_passed = !barriers.exists(!_(context, received))
      if (all_barriers_passed) {
        receivers.foreach(_.receive(this, context, received))
      }
    } finally {
      receive_lock.unlock()
    }
  }
}



trait BarrierMagnet[TContext] extends BarrierProcessor[TContext]

trait Barrier[TContext] {
  private[reactive] val associated_embrace: Embrace[TContext]
  private[reactive] val barriers: Seq[BarrierMagnet[TContext]]

  final def -(barrier: BarrierMagnet[TContext]): Barrier[TContext]                           = remove_left_associative(barrier)

  final def ~:[T](g: Generator[T, TContext]): Embrace[TContext]                              = embrace_right_associative[T](g)
  final def ~[T](g: Generator[T, TContext]): Embrace[TContext]                               = embrace_left_associative[T](g)
  final def embrace[T](g: Generator[T, TContext]): Embrace[TContext]                         = embrace_left_associative[T](g)

  final def <<|:(fn: BarrierProcessor[TContext]): Barrier[TContext]                          = barrier_right_associative(processor2BarrierMagnet(fn))
  final def |>>(fn: BarrierProcessor[TContext]): Barrier[TContext]                           = barrier_left_associative(processor2BarrierMagnet(fn))
  final def barrier(fn: BarrierProcessor[TContext]): Barrier[TContext]                       = barrier_left_associative(processor2BarrierMagnet(fn))

  final def <<|:(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]                = barrier_right_associative(magnet)
  final def |>>(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]                 = barrier_left_associative(magnet)
  final def barrier(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]             = barrier_left_associative(magnet)

  final def <<:[T](fn: SimpleJunctionProcessor[T]): Junction[TContext]                       = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def >>[T](fn: SimpleJunctionProcessor[T]): Junction[TContext]                        = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: SimpleJunctionProcessor[T]): Junction[TContext]                  = junction_left_associative[T](processor2JunctionMagnet(fn))

  final def <<:[T](fn: JunctionProcessor[TContext, T]): Junction[TContext]                   = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def >>[T](fn: JunctionProcessor[TContext, T]): Junction[TContext]                    = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: JunctionProcessor[TContext, T]): Junction[TContext]              = junction_left_associative[T](processor2JunctionMagnet(fn))

  final def <<:[T](fn: PartialFunction[(Option[TContext], Any), T]): Junction[TContext]      = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def >>[T](fn: PartialFunction[(Option[TContext], Any), T]): Junction[TContext]       = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: PartialFunction[(Option[TContext], Any), T]): Junction[TContext] = junction_left_associative[T](processor2JunctionMagnet(fn))

  //final def <<:[T](implicit magnet: JunctionMagnet[T]): Junction[TContext]      = junction_right_associative[T](magnet)
  //final def >>[T](implicit magnet: JunctionMagnet[T]): Junction[TContext]       = junction_left_associative[T](magnet)
  //final def junction[T](implicit magnet: JunctionMagnet[T]): Junction[TContext] = junction_left_associative[T](magnet)

  private[reactive] def remove_left_associative(barrier: BarrierMagnet[TContext]): Barrier[TContext]

  private[reactive] def embrace_left_associative[T](generator: Generator[T, TContext]): Embrace[TContext]
  private[reactive] def embrace_right_associative[T](generator: Generator[T, TContext]): Embrace[TContext]

  private[reactive] def barrier_left_associative(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]
  private[reactive] def barrier_right_associative(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext]

  private[reactive] def junction_left_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext]
  private[reactive] def junction_right_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext]
}

private[reactive] sealed case class BarrierImpl[TContext](
    override val associated_embrace: Embrace[TContext]
  , override val barriers: Seq[BarrierMagnet[TContext]]
) extends Barrier[TContext] {

  private[reactive] def remove_left_associative(barrier: BarrierMagnet[TContext]): Barrier[TContext] =
    BarrierImpl(associated_embrace, barriers diff Seq(barrier))


  private[reactive] def embrace_left_associative[T](g: Generator[T, TContext]): Embrace[TContext] =
    EmbraceImpl(Seq(EmbraceImpl(associated_embrace.generators, barriers), g))

  private[reactive] def embrace_right_associative[T](g: Generator[T, TContext]): Embrace[TContext] =
    EmbraceImpl(Seq(g, EmbraceImpl(associated_embrace.generators, barriers)))


  private[reactive] def barrier_right_associative(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext] =
    BarrierImpl(associated_embrace, magnet +: barriers)

  private[reactive] def barrier_left_associative(implicit magnet: BarrierMagnet[TContext]): Barrier[TContext] =
    BarrierImpl(associated_embrace, barriers :+ magnet)


  private[reactive]def junction_left_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext] =
    JunctionImpl(associated_embrace, barriers, Seq(magnet))

  private[reactive]def junction_right_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext] =
    JunctionImpl(associated_embrace, barriers, Seq(magnet))
}



trait JunctionCancellable {
  def cancel(): Unit
}

trait JunctionMagnet[TContext, +T] extends JunctionProcessor[TContext, T]

trait Junction[TContext] extends Closeable {
  private[reactive] val associated_embrace: Embrace[TContext]
  private[reactive] val associated_barriers: Seq[BarrierMagnet[TContext]]
  private[reactive] val junctions: Seq[JunctionMagnet[TContext, Any]]

  final def -[TGenerator](generator: Generator[TGenerator, TContext]): Junction[TContext]           = remove_left_associative(generator)
  final def -[TJunction](junction: JunctionMagnet[TContext, TJunction]): Junction[TContext]         = remove_left_associative(junction)
  final def -(barrier: BarrierMagnet[TContext]): Junction[TContext]                                 = remove_left_associative(barrier)
  final def remove[TGenerator](generator: Generator[TGenerator, TContext]): Junction[TContext]      = remove_left_associative(generator)
  final def remove[TJunction](junction: JunctionMagnet[TContext, TJunction]): Junction[TContext]    = remove_left_associative(junction)
  final def remove(barrier: BarrierMagnet[TContext]): Junction[TContext]                            = remove_left_associative(barrier)

  final def >>[T](fn: SimpleJunctionProcessor[T]): Junction[TContext]                               = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def <<:[T](fn: SimpleJunctionProcessor[T]): Junction[TContext]                              = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: SimpleJunctionProcessor[T]): Junction[TContext]                         = junction_left_associative[T](processor2JunctionMagnet(fn))

  final def >>[T](fn: JunctionProcessor[TContext, T]): Junction[TContext]                           = junction_left_associative[T](processor2JunctionMagnet(fn))
  final def <<:[T](fn: JunctionProcessor[TContext, T]): Junction[TContext]                          = junction_right_associative[T](processor2JunctionMagnet(fn))
  final def junction[T](fn: JunctionProcessor[TContext, T]): Junction[TContext]                     = junction_left_associative[T](processor2JunctionMagnet(fn))

  final def >>[T](implicit magnet: JunctionMagnet[TContext, T]): Junction[TContext]                 = junction_left_associative[T](magnet)
  final def <<:[T](implicit magnet: JunctionMagnet[TContext, T]): Junction[TContext]                = junction_right_associative[T](magnet)
  final def junction[T](implicit magnet: JunctionMagnet[TContext, T]): Junction[TContext]           = junction_left_associative[T](magnet)

  private[reactive] def remove_left_associative(barrier: BarrierMagnet[TContext]): Junction[TContext]
  private[reactive] def remove_left_associative[T](junction: JunctionMagnet[TContext, T]): Junction[TContext]
  private[reactive] def remove_left_associative[T](generator: Generator[T, TContext]): Junction[TContext]

  private[reactive] def junction_left_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext]
  private[reactive] def junction_right_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext]

  def begin: JunctionCancellable
}

private[reactive] sealed case class JunctionImpl[TContext](
    override val associated_embrace: Embrace[TContext]
  , override val associated_barriers: Seq[BarrierMagnet[TContext]]
  , override val junctions: Seq[JunctionMagnet[TContext, Any]]
) extends Junction[TContext] with Receiver[TContext] {
  @volatile private[this] var cancelled = false


  private[reactive] def remove_left_associative(barrier: BarrierMagnet[TContext]): Junction[TContext] =
    JunctionImpl(associated_embrace, associated_barriers diff Seq(barrier), junctions)

  private[reactive] def remove_left_associative[T](junction: JunctionMagnet[TContext, T]): Junction[TContext] =
    JunctionImpl(associated_embrace, associated_barriers, junctions diff Seq(junction))

  private[reactive] def remove_left_associative[T](generator: Generator[T, TContext]): Junction[TContext] =
    JunctionImpl(EmbraceImpl(associated_embrace.generators diff Seq(generator), associated_embrace.barriers), associated_barriers, junctions)


  private[reactive] def junction_left_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext] =
    JunctionImpl(associated_embrace, associated_barriers, junctions :+ magnet)

  private[reactive] def junction_right_associative[T](magnet: JunctionMagnet[TContext, T]): Junction[TContext] =
    JunctionImpl(associated_embrace, associated_barriers, magnet +: junctions)


  private[reactive] def receive(source: Generator[Any, TContext], context: Option[TContext], received: Received): Unit = {
    if (cancelled)
      return

    if (received.data.isEmpty)
      return

    val all_barriers_passed = !associated_barriers.exists(!_(context, received))
    if (all_barriers_passed) {
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
        last = j(context, last)
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