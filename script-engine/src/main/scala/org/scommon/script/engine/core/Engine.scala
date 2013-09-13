package org.scommon.script.engine.core

import org.scommon.core._
import org.scommon.script.engine.ScalaEngine
import com.typesafe.config.{ConfigFactory, Config}
import scala.reflect.internal.MissingRequirementError
import java.util.concurrent.locks.ReentrantLock
import java.net.URI
import org.scommon.reactive.Generator

object Engine {
  //Lazily gets the list of factories. Deliberately do *not* use lazy vals here because this could
  //be updated later on if factories() is called again with a different configuration.
  private[this] val configuration_load_lock = new ReentrantLock()
  private[this] var lazy_configuration: Option[FactoriesFromConfiguration] = None

  case class FactoriesFromConfiguration(default: EngineFactory, factories: Iterable[EngineFactory])

  def default: EngineFactory =
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
  def default(context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): EngineFactory = {
    lazy_configuration
      .getOrElse(lazy_load_factories(context, configuration))
      .default
  }

  def factories: Iterable[EngineFactory] =
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
  def factories(context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): Iterable[EngineFactory] = {
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

    var default:EngineFactory = null
    val scoped = config.getConfig("script-engine")
    val script_engines = scoped.getObjectList("engines")
    val script_default = scoped.getString("default")

    val engines =
      for {
        e <- script_engines
        name    = e.get("name").unwrapped().toString
        factory = e.get("factory").unwrapped().toString
      } yield {
        try {
          val module          = runtime_mirror.staticModule(factory)
          val obj             = runtime_mirror.reflectModule(module)
          val class_of_module = obj.instance.getClass

          if (!classOf[EngineFactory].isAssignableFrom(class_of_module)) {
            throw new IllegalStateException(s"${obj.symbol.fullName} must be a subtype of ${classOf[EngineFactory].getName}")
          }

          val instance = obj.instance.asInstanceOf[EngineFactory]
          val requested_instance = instance.instance

          if (script_default == name)
            default = requested_instance

          requested_instance
        } catch {
          case mre:MissingRequirementError =>
            throw new IllegalStateException(s"Unable to find factory of type ${classOf[EngineFactory].getName} for $name:$factory")
        }
      }

    require(default ne null, s"No provided engine with the name '$script_default' (the specified default engine for the script engine)")

    FactoriesFromConfiguration(default, engines)
  }

  def newEngine(sourceCode: String, addlSourceCode: String*): Engine =
    newEngine(CompilerSourceGenerator.fromStrings(sourceCode +: addlSourceCode))

  def newEngine(settings: CompilerSettings, sourceCode: String, addlSourceCode: String*): Engine =
    newEngine(settings, CompilerSourceGenerator.fromStrings(sourceCode +: addlSourceCode))

  def apply[T](generator: Generator[CompilerSource[T]]): Engine =
    newEngine(generator)

  def newEngine[T](generator: Generator[CompilerSource[T]]): Engine = {
    //Do it this way b/c it's possible the default changes from underneath
    //you between calls (very unlikely but still possible).
    val d = default
    d.newEngine(d.details.defaultSettings, generator)
  }

  def apply[T](settings: CompilerSettings, generator: Generator[CompilerSource[T]]): Engine =
    newEngine(settings, generator)

  def newEngine[T](settings: CompilerSettings, generator: Generator[CompilerSource[T]]): Engine =
    default.newEngine(settings, generator)

  def newScalaEngine[T](settings: CompilerSettings, generator: Generator[CompilerSource[T]]): Engine =
    ScalaEngine.newEngine(settings, generator)
}

trait EngineFactory {
  def instance: EngineFactory
  def details: EngineDetails
  def newEngine[T](settings: CompilerSettings, generator: Generator[CompilerSource[T]]): Engine
  override def toString = s"$details"
}

trait EngineDetails {
  def name: String
  def title: String
  def version: Version
  def defaultSettings: CompilerSettings
  override def toString = s"$title v$version"
}

trait Engine {
  def settings: CompilerSettings
}


