package org.scommon.script.engine.core

import com.typesafe.config.{ConfigFactory, Config}
import scala.reflect.internal.MissingRequirementError
import scala.collection.JavaConversions
import java.util.concurrent.locks.ReentrantLock
import java.net.URI
import org.scommon.reactive.Generator
import org.scommon.reflect._
import rx.lang.scala.{Subscription, Observable}

import org.scommon.core._
import org.scommon.script.engine.{Scala, ScalaEngine}

object Engine {
  import scala.language.implicitConversions

  //Lazily gets the list of factories. Deliberately do *not* use lazy vals here because this could
  //be updated later on if factories() is called again with a different configuration.
  private[this] val configuration_load_lock = new ReentrantLock()
  private[this] var lazy_configuration: Option[FactoriesFromConfiguration] = None

  case class FactoriesFromConfiguration(default: EngineFactory[CompilerSpecificSettings], factories: Iterable[EngineFactory[CompilerSpecificSettings]])

  def default[S <: CompilerSpecificSettings]: EngineFactory[S] =
    default()

  /**
   * Warning: the first time this is called will cache and lock the produced list of
   * [[org.scommon.script.engine.core.EngineFactory]] instances. If you need to reload the list, you should call
   * [[org.scommon.script.engine.core.Engine#factoriesFromConfiguration]] instead.
   *
   * @param context [[java.lang.ClassLoader]] instance to use when loading the configuration. The configuration API
   *                will search this space according to [[https://github.com/typesafehub/config/blob/v1.0.2/README.md]]
   *                (please see the section titled "Standard behavior").
   * @param configuration [[com.typesafe.config.Config]] instance to use when loading the configuration.
   * @return The default [[org.scommon.script.engine.core.EngineFactory]] instance.
   */
  def default[S <: CompilerSpecificSettings](context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): EngineFactory[S] = {
    lazy_configuration
      .getOrElse(lazy_load_factories(context, configuration))
      .default
      .asInstanceOf[EngineFactory[S]]
  }

  def factories: Iterable[EngineFactory[CompilerSpecificSettings]] =
    factories()

  /**
   * Warning: the first time this is called will cache and lock the produced list of
   * [[org.scommon.script.engine.core.EngineFactory]] instances. If you need to reload the list, you should call
   * [[org.scommon.script.engine.core.Engine#factoriesFromConfiguration]] instead.
   *
   * @param context [[java.lang.ClassLoader]] instance to use when loading the configuration. The configuration API
   *                will search this space according to [[https://github.com/typesafehub/config/blob/v1.0.2/README.md]]
   *                (please see the section titled "Standard behavior").
   * @param configuration [[com.typesafe.config.Config]] instance to use when loading the configuration.
   * @return [[scala.collection.Iterable]] of [[org.scommon.script.engine.core.EngineFactory]] instances.
   */
  def factories(context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): Iterable[EngineFactory[CompilerSpecificSettings]] = {
    lazy_configuration
      .getOrElse(lazy_load_factories(context, configuration))
      .factories
  }

  private[this] def lazy_load_factories(context: ClassLoader, configuration: Config) = {
    configuration_load_lock.lock()
    try {
      val result =
        factoriesFromConfiguration(context, configuration)

      lazy_configuration = Some(result)

      result
    } finally {
      configuration_load_lock.unlock()
    }
  }

  def factoriesFromConfiguration(context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): FactoriesFromConfiguration = {
    import scala.reflect.runtime.universe
    import scala.collection.JavaConversions._

    val config =
      if (configuration eq null)
        ConfigFactory.load(context)
      else
        configuration

    config.checkValid(ConfigFactory.defaultReference(context), "script-engine")

    //Get the runtime mirror that we'll use to find a corresponding module
    //for the provided factory.
    val runtime_mirror = universe.runtimeMirror(context)

    var default:EngineFactory[CompilerSpecificSettings] = null
    val scoped = config.getConfig("script-engine")
    val script_engines = scoped.getStringList("engines")
    val script_default = scoped.getString("default")

    val engines =
      for {
        script_engine <- script_engines
        e       = scoped.getObject(script_engine).toConfig
        name    = e.getString("name")
        factory = e.getString("factory")
      } yield {
        try {
          val module          = runtime_mirror.staticModule(factory)
          val obj             = runtime_mirror.reflectModule(module)
          val class_of_module = obj.instance.getClass

          if (!classOf[EngineFactory[CompilerSpecificSettings]].isAssignableFrom(class_of_module)) {
            throw new IllegalStateException(s"${obj.symbol.fullName} must be a subtype of ${classOf[EngineFactory[CompilerSpecificSettings]].getName}")
          }

          val instance = obj.instance.asInstanceOf[EngineFactory[CompilerSpecificSettings]]
          val requested_instance = instance.instance

          if (script_default == name)
            default = requested_instance

          requested_instance
        } catch {
          case mre:MissingRequirementError =>
            throw new IllegalStateException(s"Unable to find factory of type ${classOf[EngineFactory[CompilerSpecificSettings]].getName} for $name:$factory")
        }
      }

    require(default ne null, s"No provided engine with the name '$script_default' (the specified default engine for the script engine)")

    FactoriesFromConfiguration(default, engines)
  }

  def newEngine[S <: CompilerSpecificSettings](sourceCode: String*): Engine[S, URI] =
    newEngine[S, URI](CompilerSourceGenerator.fromStrings(sourceCode))

  def newEngine[S <: CompilerSpecificSettings](settings: CompilerSettings[S], sourceCode: String*): Engine[S, URI] =
    newEngine[S, URI](settings, CompilerSourceGenerator.fromStrings(sourceCode))

  def newEngine[S <: CompilerSpecificSettings](settings: CompilerSettings[S], sourceCode: Iterable[String]): Engine[S, URI] =
    newEngine[S, URI](settings, CompilerSourceGenerator.fromStrings(sourceCode))

  def apply[S <: CompilerSpecificSettings, T](generator: Generator[CompilerSource[T], CompilerContext]): Engine[S, T] =
    newEngine[S, T](generator)

  def newEngine[S <: CompilerSpecificSettings, T](generator: Generator[CompilerSource[T], CompilerContext]): Engine[S, T] = {
    //Do it this way b/c it's possible the default changes from underneath
    //you between calls (very unlikely but still possible).
    val d = default[S]
    val settings = d.details.defaultSettings
    d.newEngine[S, T](settings, generator withDefaultContext settings)
  }

  def apply[S <: CompilerSpecificSettings, T](settings: CompilerSettings[S], generator: Generator[CompilerSource[T], CompilerContext]): Engine[S, T] =
    newEngine[S, T](settings, generator)

  def newEngine[S <: CompilerSpecificSettings, T](settings: CompilerSettings[S], generator: Generator[CompilerSource[T], CompilerContext]): Engine[S, T] =
    default[S].newEngine(settings, generator withDefaultContext settings)

  def newScalaEngine[T, C >: CompilerContext <: CompilerContext](settings: CompilerSettings[Scala], generator: Generator[CompilerSource[T], CompilerContext]): Engine[Scala, T] =
    ScalaEngine.newEngine[Scala, T](settings, generator withDefaultContext settings)
}

trait CompilerSpecificSettings extends StandardFieldMirror

trait EngineFactory[+S <: CompilerSpecificSettings] {
  def instance: EngineFactory[S]
  def details: EngineDetails[S]
  def newEngine[U >: S <: CompilerSpecificSettings, T](settings: CompilerSettings[U], generator: Generator[CompilerSource[T], CompilerContext]): Engine[S, T]
  override def toString = s"$details"
}

trait EngineDetails[+S <: CompilerSpecificSettings] {
  def name: String
  def title: String
  def version: Version
  def defaultSettings: CompilerSettings[S]
  override def toString = s"$title v$version"
}

trait Engine[+S <: CompilerSpecificSettings, +T] extends Closeable {
  import java.util.concurrent.CopyOnWriteArrayList
  import JavaConversions._

  type TSource <: T

  def details: EngineDetails[S]
  def settings: CompilerSettings[S]
  def generator: Generator[CompilerSource[T], CompilerContext]

  def push[CS <: CompilerSource[TSource]](source: CS) =
    generator.push(source)
  def push[CS <: CompilerSource[TSource]](source: Iterable[CS]) =
    generator.push(source)
  def push[CS <: CompilerSource[TSource]](context: CompilerContext, source: CS) =
    generator.push(Some(context), source)
  def push[CS <: CompilerSource[TSource]](context: CompilerContext, source: Iterable[CS]) =
    generator.push(Some(context), source)

  def pushSource(source: String, addlSource: String*) =
    generator.push((source +: addlSource) map CompilerSource.fromString)
  def pushSource(source: Iterable[String]) =
    generator.push(source map CompilerSource.fromString)
  def pushSource(context: CompilerContext, source: String, addlSource: String*) =
    generator.push(Some(context), (source +: addlSource) map CompilerSource.fromString)
  def pushSource(context: CompilerContext, source: Iterable[String]) =
    generator.push(Some(context), source map CompilerSource.fromString)

  private[this] val engine_listeners: CopyOnWriteArrayList[CompileListener] =
    new CopyOnWriteArrayList[CompileListener]()

  protected def onMessageReceived(context: CompilerContext, message: CompilerMessage): Unit = {
    val update = StandardCompileUpdate(
        completed = false
      , message   = Some(message)
    )
    context.handlers.messageReceived(this, message)
    for(listener <- engine_listeners)
      listener.onUpdate(update)
  }

  protected def onProgressUpdate(context: CompilerContext, progress: CompilerProgress): Unit = {
    val update = StandardCompileUpdate(
        completed = false
      , progress  = Some(progress)
    )
    context.handlers.progressUpdate(this, progress)
    for(listener <- engine_listeners)
      listener.onUpdate(update)
  }

  protected def onSourceCompiled(context: CompilerContext, result: CompileResult): Unit = {
    val update = StandardCompileUpdate(
        completed = true
      , result    = Some(result)
    )
    context.handlers.sourceCompiled(this, result)
    for(listener <- engine_listeners)
      listener.onUpdate(update)
  }

  def toObservable: Observable[CompileUpdate] = {
    Observable.create[CompileUpdate] { subscriber =>
      val listener = StandardCompileListener(
          fnUpdate = (x) => subscriber.onNext(x)
        , fnClose  = ()  => subscriber.onCompleted()
      )
      engine_listeners.add(listener)
      Subscription {
        engine_listeners.remove(listener)
        ()
      }
    }
  }
}
