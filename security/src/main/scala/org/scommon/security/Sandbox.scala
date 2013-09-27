package org.scommon.security

import scala.util.{Success, Failure, Try}
import com.typesafe.config.{ConfigObject, ConfigFactory, Config}
import java.util.concurrent.locks.ReentrantLock
import net.datenwerke.sandbox._
import net.datenwerke.sandbox.SandboxContext.{Mode, AccessType}
import net.datenwerke.sandbox.permissions.SecurityPermission
import org.scommon.core._

import java.io.{ByteArrayInputStream, ObjectInputStream, ByteArrayOutputStream, ObjectOutputStream}
import java.lang.Thread.UncaughtExceptionHandler

object Sandbox {
  //Lazily gets the list of profiles. Deliberately do *not* use lazy vals here because this could
  //be updated later on if profiles() is called again with a different configuration.
  private[this] val configuration_load_lock = new ReentrantLock()
  private[this] var lazy_configuration: Option[ProfilesFromConfiguration] = None

  val UNCAUGHT_EXCEPTION_SENTINEL: Throwable = new IllegalStateException()

  implicit val DEFAULT_SANDBOX_SERVICE: SandboxService = SandboxServiceImpl.initLocalSandboxService()

  case class ProfilesFromConfiguration(default: SandboxProfile, profiles: Iterable[SandboxProfile])

  /** Warning: This instance of [[net.datenwerke.sandbox.SandboxContext]] is mutable! */
	def defaultContext: SandboxContext =
		defaultProfile.context

  def defaultProfile: SandboxProfile =
    defaultProfile()

  def defaultProfile(context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): SandboxProfile = {
    lazy_configuration
      .getOrElse(lazy_load_profiles(context, configuration))
      .default
  }

  private[this] def lazy_load_profiles(context: ClassLoader, configuration: Config) = {
    configuration_load_lock.lock()
    try {
      val result =
        profilesFromConfiguration(context, configuration)

      lazy_configuration = Some(result)

      result
    } finally {
      configuration_load_lock.unlock()
    }
  }

  def profilesFromConfiguration(context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): ProfilesFromConfiguration = {
    import scala.collection.JavaConversions._

    val config =
      if (configuration eq null)
        ConfigFactory.load(context)
      else
        configuration

    config.checkValid(ConfigFactory.defaultReference(context), "sandbox")

    var default:SandboxProfile = null
    val scoped = config.getConfig("security.sandbox")
    val security_profiles = scoped.getStringList("profiles")
    val profile_default = scoped.getString("default")

    val profile_fallback = ConfigFactory.parseString(
      """
        |enable-home-dir = true
        |enable-temp-dir = true
        |enable-work-dir = true
        |load-classpath  = true
        |packages-for-app-loader = []
        |allow-permissions = []
        |allow-packages = []
        |allow-classes = []
        |deny-permissions = []
        |deny-packages = []
        |deny-classes = []
        |allow-socket-permissions = []
      """.stripMargin)

    val socket_permissions_fallback = ConfigFactory.parseString(
      """
        |host    = ""
        |actions = ""
      """.stripMargin)

    val permissions_fallback = ConfigFactory.parseString(
      """
        |type    = ""
        |name    = ""
        |actions = ""
      """.stripMargin)

    val profiles =
      for {
        security_profile <- security_profiles
				p                        = scoped.getObject(security_profile)
        p_config                 = p.toConfig.withFallback(profile_fallback)
        name                     = p_config.getString("name")

        enable_home_dir          = p_config.getBoolean("enable-home-dir")
        enable_temp_dir          = p_config.getBoolean("enable-temp-dir")
        enable_work_dir          = p_config.getBoolean("enable-work-dir")
        load_classpath           = p_config.getBoolean("load-classpath")
        packages_for_app_loader  = p_config.getStringList("packages-for-app-loader")
        allow_permissions        = p_config.getObjectList("allow-permissions")
        allow_packages           = p_config.getStringList("allow-packages")
        allow_classes            = p_config.getStringList("allow-classes")
        deny_permissions         = p_config.getObjectList("deny-permissions")
        deny_packages            = p_config.getStringList("deny-packages")
        deny_classes             = p_config.getStringList("deny-classes")
        allow_socket_permissions = p_config.getObjectList("allow-socket-permissions")
      } yield {
        try {
          val sandbox_context = new SandboxContext(false)

          def process_permissions(access: AccessType, permissions: Iterable[ConfigObject]) = {
            for {
              permission <- permissions
              p2 = permission.toConfig.withFallback(permissions_fallback)
              t = p2.getString("type")
              n = p2.getString("name")
              a = p2.getString("actions")
            } {
              if (n.isNonEmpty && a.isNonEmpty)
                sandbox_context.addSecurityPermission(access, new SecurityPermission(t, n, a))
              else if (n.isNonEmpty && a.isNullOrEmpty)
                sandbox_context.addSecurityPermission(access, new SecurityPermission(t, n))
              else
                sandbox_context.addSecurityPermission(access, new SecurityPermission(t))
            }
          }
					
					def process_socket_permissions(permissions: Iterable[ConfigObject]) = {
            for {
              permission <- permissions
              p2 = permission.toConfig.withFallback(socket_permissions_fallback)
              h = p2.getString("host")
              a = p2.getString("actions")
            } {
						  require(h.isNonEmpty && a.isNonEmpty, "Both host and actions fields must be defined for a socket permission")
							sandbox_context.addSocketPermission(new java.net.SocketPermission(h, a))
            }
          }

          def process_packages(access: AccessType, packages: Iterable[String]) = {
            for (p <- packages)
              sandbox_context.addClassPermission(access, Mode.PREFIX, p)
          }

          def process_classes(access: AccessType, classes: Iterable[String]) = {
            for (c <- classes)
              sandbox_context.addClassPermission(access, Mode.NORMAL, c)
          }

          def process_packages_for_app_loader(packages: Iterable[String]) = {
            for (p <- packages)
              sandbox_context.addClassForApplicationLoader(p, Mode.PREFIX)
          }

          process_permissions(AccessType.PERMIT, allow_permissions)
          process_permissions(AccessType.DENY, deny_permissions)
          process_packages(AccessType.PERMIT, allow_packages)
          process_packages(AccessType.DENY, deny_packages)
          process_classes(AccessType.PERMIT, allow_classes)
          process_classes(AccessType.DENY, deny_classes)
          process_packages_for_app_loader(packages_for_app_loader)
					process_socket_permissions(allow_socket_permissions)

          if (enable_home_dir)
            sandbox_context.addHome()

          if (enable_temp_dir)
            sandbox_context.addTempDir()

          if (enable_work_dir)
            sandbox_context.addWorkDir()
					
					if (load_classpath)
						sandbox_context.addClasspath()

          val profile = StandardSandboxProfile(name, sandbox_context)

          if (profile_default == name)
            default = profile

          profile
        } catch {
          case t:Throwable =>
            throw new IllegalStateException(s"Unable to load profile for $name")
        }
      }

    require(default ne null, s"No provided profile with the name '$profile_default' (the specified default profile for the security sandbox)")

    ProfilesFromConfiguration(default, profiles)
  }

  def apply(): MutableSandbox =
    apply(Thread.currentThread().getContextClassLoader())

  def apply(classLoader: ClassLoader): MutableSandbox =
    apply(defaultContext, None, classLoader)

  def apply(enhancer: SandboxLoaderEnhancer): MutableSandbox =
    apply(defaultContext, Some(enhancer))

  def apply(sandboxProfile: SandboxProfile): MutableSandbox =
    apply(sandboxProfile, None, Thread.currentThread().getContextClassLoader())

  def apply(sandboxProfile: SandboxProfile, classLoader: ClassLoader): MutableSandbox =
    apply(sandboxProfile.context, None, classLoader)

  def apply(sandboxProfile: SandboxProfile, enhancer: Option[SandboxLoaderEnhancer], classLoader: ClassLoader): MutableSandbox =
    apply(sandboxProfile.context, enhancer, classLoader)

  def apply(sandboxContext: SandboxContext, enhancer: Option[SandboxLoaderEnhancer] = None, classLoader: ClassLoader = Thread.currentThread().getContextClassLoader()): MutableSandbox =
    new MutableSandbox(context = sandboxContext, enhancer = enhancer, classLoader = classLoader)
}

import Sandbox._

trait Sandbox {
  def handlers: SandboxEventHandlers
  def context: SandboxContext
  def enhancer: Option[SandboxLoaderEnhancer]
  def classLoader: ClassLoader

  protected def execute[T <: java.io.Serializable](callable: SandboxCallable[T], service: SandboxService, useDefault: Boolean, default: => T): T

  def run[T <: java.io.Serializable](fn: => T)(implicit service: SandboxService = DEFAULT_SANDBOX_SERVICE): T =
    execute[T](callByName2SandboxCallable(fn), service, false, defaultValue[T])

  def run[T <: java.io.Serializable](callable: SandboxCallable[T])(implicit service: SandboxService = DEFAULT_SANDBOX_SERVICE): T =
    execute[T](callable, service, false, defaultValue[T])

  def runWithDefault[T <: java.io.Serializable](callable: SandboxCallable[T])(default: => T = defaultValue[T])(implicit service: SandboxService = DEFAULT_SANDBOX_SERVICE): T =
    execute[T](callable, service, true, default)
}

case class MutableSandbox(
    var handlers: MutableSandboxEventHandlers = MutableSandboxEventHandlers()
  , val context: SandboxContext
  , val enhancer: Option[SandboxLoaderEnhancer]
  , val classLoader: ClassLoader
) extends Sandbox {
  protected def execute[T <: java.io.Serializable](callable: SandboxCallable[T], service: SandboxService, useDefault: Boolean, default: => T): T = {
    require(service ne null, s"service must be defined")

    //Serialize to a byte array that we'll then de-serialize back to the callable from within the sandbox.
    val out = new ByteArrayOutputStream(1024)
    using(new ObjectOutputStream(out)) { stream =>
      stream.writeObject(callable)
    }

    //Important since we're about to overwrite some things that may have been set.
    //We want to ensure that we have our own copy to play with. Any modifications at this point
    //should be *minimal* since we want to preserve the context's settings as much as possible.
    val previous_uncaught_exception_handler = context.getUncaughtExceptionHandler()
    val using_context = context.clone()

    //Set enhancer if provided.
    if (enhancer.isDefined)
      using_context.setLoaderEnhancer(enhancer.get)

    //Provide a way for our handler to be called.
    using_context.setUncaughtExceptionHandler(new UncaughtExceptionHandler {
      def uncaughtException(t: Thread, e: Throwable): Unit = {
        handlers.exceptionReceived(MutableSandbox.this, e)
        if (previous_uncaught_exception_handler ne null)
          previous_uncaught_exception_handler.uncaughtException(t, e)
      }
    })

    val loader = service.initClassLoader(classLoader, using_context)
    Thread.currentThread().setContextClassLoader(loader)
    val result = service.runSandboxed(classOf[SandboxedEnvironmentForRunningCallables[T]], using_context, loader, out.toByteArray)

    val ret = result.get().asInstanceOf[Try[T]] match {
      case Success(value) =>
        value
      case Failure(thrown) =>
        handlers.exceptionReceived(this, thrown)
        default
    }

    ret
  }
}

private[security] class SandboxedEnvironmentForRunningCallables[T <: java.io.Serializable](callableBytes: Array[Byte]) extends SandboxedEnvironment[Try[T]] {
  def execute(): Try[T] = {
    try {
      using(new ObjectInputStream(new ByteArrayInputStream(callableBytes))) { stream =>
        val callable = stream.readObject().asInstanceOf[SandboxCallable[T]]
        Success(callable.run())
      }
    } catch {
      case t:Throwable =>
        Failure(t)
    }
  }
}
