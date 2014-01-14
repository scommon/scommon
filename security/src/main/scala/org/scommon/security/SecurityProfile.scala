package org.scommon.security

import scala.util.{Success, Failure, Try}

import java.util.concurrent.locks.ReentrantLock
import java.io.FilePermission
import java.nio.file.Paths

import com.typesafe.config.{ConfigObject, ConfigFactory, Config}

import org.scommon.io.PathUtil
import org.scommon.core._

import scala.language.implicitConversions

trait SecurityProfile extends SecurityContext {
  def name: String
  def description: String
}

@SerialVersionUID(234908562243L)
private sealed class SecurityProfileImpl(val name: String, val description: String, val grants: Permissions) extends SecurityProfile {
  override def hashCode(): Int = grants.hashCode()
  override def toString: String = s"SecurityProfile(name: $name, grants: $grants, description: $description)"
  override def equals(obj: scala.Any): Boolean = obj match {
    case o: SecurityProfile =>
      name == o.name && grants == o.grants
    case _ =>
      false
  }
}

object SecurityProfile {
  //Lazily gets the list of profiles. Deliberately do *not* use lazy vals here because this could
  //be updated later on if profiles() is called again with a different configuration.
  private[this] val configuration_load_lock = new ReentrantLock()
  private[this] var lazy_configuration: Option[ProfilesFromConfiguration] = None

  case class ProfilesFromConfiguration(default: SecurityProfile, profiles: Iterable[SecurityProfile])

  implicit def default: SecurityProfile =
    default()

  def default(context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): SecurityProfile = {
    lazy_configuration
      .getOrElse(lazy_load_profiles(context, configuration))
      .default
  }

  def get(name: String, context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): Option[SecurityProfile] =
    lazy_configuration
      .getOrElse(lazy_load_profiles(context, configuration))
      .profiles
      .find(_.name == name)

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

  def apply(name: String, context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): SecurityProfile =
    get(name, context, configuration) match {
      case Some(profile) => profile
      case _ => throw new IllegalStateException(s"No security profile matching $name")
    }

  def apply(name: String, description: String, grants: Permissions): SecurityProfile =
    new SecurityProfileImpl(name, description, grants)

  def profilesFromConfiguration(context: ClassLoader = Thread.currentThread().getContextClassLoader(), configuration: Config = null): ProfilesFromConfiguration = {
    import scala.collection.JavaConversions._

    val config =
      if (configuration eq null)
        ConfigFactory.load(context)
      else
        configuration

    config.checkValid(ConfigFactory.defaultReference(context), "sandbox")

    var default:SecurityProfile = null
    val scoped = config.getConfig("security")
    val security_profiles = scoped.getObject("profiles")
    val profile_default = scoped.getString("default-profile")

    val profile_fallback = ConfigFactory.parseString(
      """
        |home-dir = {
        |  enabled = false
        |  actions = ""
        |}
        |
        |temp-dir = {
        |  enabled = false
        |  actions = ""
        |}
        |
        |work-dir = {
        |  enabled = false
        |  actions = ""
        |}
        |
        |grants = []
      """.stripMargin)

    val special_directory_fallback = ConfigFactory.parseString(
      """
        |enabled = false
        |actions = ""
      """.stripMargin)

    val system_path_fallback = ConfigFactory.parseString(
      """
        |enabled = false
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
        (name, _) <- security_profiles
        p_config        = security_profiles.toConfig.getObject(name).toConfig
        description     = p_config.getString("description")
        home_dir        = p_config.getObject("home-dir")
        temp_dir        = p_config.getObject("temp-dir")
        work_dir        = p_config.getObject("work-dir")
        app_dir         = p_config.getObject("application-dir")
        sys_path        = p_config.getObject("system-path")
        grants          = p_config.getObjectList("grants")
      } yield {
        try {
          def process_permissions(permissions: Iterable[ConfigObject]) = Permissions(
            for {
              permission <- permissions
              p2 = permission.toConfig.withFallback(permissions_fallback)
              t = p2.getString("type")
              n = p2.getString("name")
              a = p2.getString("actions")
              perm = Permission(t, if (n.isNonEmpty) n else null, if (a.isNonEmpty) a else null)
            } yield perm
          )

          def process_special_directory(obj: ConfigObject, directory: => String): Seq[Permission] = {
            val special = obj.toConfig.withFallback(special_directory_fallback)
            val enabled = special.getBoolean("enabled")
            val actions = special.getString("actions")
            val path = Paths.get(directory).toAbsolutePath.toString
            if (enabled)
              Seq(Permission(new FilePermission(path, actions)), Permission(new FilePermission(recursivePath(path), actions)))
            else
              Seq()
          }

          def process_system_path(obj: ConfigObject): Seq[Permission] = {
            val special = obj.toConfig.withFallback(system_path_fallback)
            val enabled = special.getBoolean("enabled")
            val actions = special.getString("actions")
            if (enabled) {
              try {
                (
                  for (path <- PathUtil.querySystemPath.split(PathUtil.queryPathSeparator.head).toSeq)
                    yield Seq(Permission(new FilePermission(path, actions)), Permission(new FilePermission(recursivePath(path), actions)))
                ).flatten
              } catch {
                case _: Throwable =>
                  Seq()
              }
              //Seq(Permission(new FilePermission(path, actions)), Permission(new FilePermission(recursivePath(path), actions)))
              Seq()
            } else Seq()
          }

          def recursivePath(path: String): String = {
            val sep = PathUtil.fileSeparator
            if (path.endsWith(sep))
              path + "-"
            else
              path + sep + "-"
          }

          val permissions = process_permissions(grants) ++
            process_system_path(sys_path) ++
            process_special_directory(home_dir, PathUtil.queryUserHomeDirectory) ++
            process_special_directory(temp_dir, PathUtil.querySystemUserTempDirectory) ++
            process_special_directory(work_dir, PathUtil.queryWorkingDirectory) ++
            process_special_directory(app_dir,  PathUtil.queryApplicationDirectory)

          val profile = SecurityProfile(name, description, permissions)

          if (profile_default == name)
            default = profile

          profile
        } catch {
          case t:Throwable =>
            throw new IllegalStateException(s"Unable to load profile for $name", t)
        }
      }

    require(default ne null, s"No provided profile with the name '$profile_default' (the specified default security profile)")

    ProfilesFromConfiguration(default, profiles)
  }
}
