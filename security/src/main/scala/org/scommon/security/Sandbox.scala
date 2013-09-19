package org.scommon.security

import net.datenwerke.sandbox.{SandboxedEnvironment, SandboxContext, SandboxServiceImpl}
import net.datenwerke.sandbox.SandboxContext.{Mode, AccessType}
import java.util.concurrent.locks.ReentrantLock
import com.typesafe.config.{ConfigObject, ConfigFactory, Config}
import org.scommon.core._
import net.datenwerke.sandbox.permissions.SecurityPermission

trait SandboxProfile {
  def name: String
  def context: SandboxContext
}

case class StandardSandboxProfile(name: String, context: SandboxContext) extends SandboxProfile

object Sandbox {
  //Lazily gets the list of profiles. Deliberately do *not* use lazy vals here because this could
  //be updated later on if profiles() is called again with a different configuration.
  private[this] val configuration_load_lock = new ReentrantLock()
  private[this] var lazy_configuration: Option[ProfilesFromConfiguration] = None

  case class ProfilesFromConfiguration(default: SandboxProfile, profiles: Iterable[SandboxProfile])
	
	def default: SandboxContext = 
		defaultProfile.context.clone()

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
    val scoped = config.getConfig("sandbox")
    val security_profiles = scoped.getObjectList("profiles")
    val profile_default = scoped.getString("default")

    val profile_fallback = ConfigFactory.parseString(
      """
        |enable-home-dir = true
        |enable-temp-dir = true
        |enable-work-dir = true
        |packages-for-app-loader = []
        |allow-permissions = []
        |allow-packages = []
        |allow-classes = []
        |deny-permissions = []
        |deny-packages = []
        |deny-classes = []
      """.stripMargin)

    val permissions_fallback = ConfigFactory.parseString(
      """
        |type    = ""
        |name    = ""
        |actions = ""
      """.stripMargin)

    val profiles =
      for {
        p <- security_profiles
        p_config                = p.toConfig.withFallback(profile_fallback)
        name                    = p_config.getString("name")

        enable_home_dir         = p_config.getBoolean("enable-home-dir")
        enable_temp_dir         = p_config.getBoolean("enable-temp-dir")
        enable_work_dir         = p_config.getBoolean("enable-work-dir")
        packages_for_app_loader = p_config.getStringList("packages-for-app-loader")
        allow_permissions       = p_config.getObjectList("allow-permissions")
        allow_packages          = p_config.getStringList("allow-packages")
        allow_classes           = p_config.getStringList("allow-classes")
        deny_permissions        = p_config.getObjectList("deny-permissions")
        deny_packages           = p_config.getStringList("deny-packages")
        deny_classes            = p_config.getStringList("deny-classes")
      } yield {
        try {
          val sandbox_context = new SandboxContext(true)

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

          if (enable_home_dir)
            sandbox_context.addHome()

          if (enable_temp_dir)
            sandbox_context.addTempDir()

          if (enable_work_dir)
            sandbox_context.addWorkDir()

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
}
class Sandbox() {

}
