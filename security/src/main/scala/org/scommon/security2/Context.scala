package org.scommon.security2

import java.security.{Permission => JPermission}

object Context {
  def apply(permitAll: Boolean = false, blacklist: Permissions = Permissions(), whitelist: Permissions = Permissions(), packageBlacklist: Traversable[String] = Seq(), packageWhitelist: Traversable[String] = Seq()) =
    new ContextImpl(permitAll, blacklist, whitelist, packageBlacklist, packageWhitelist)
}

trait Context extends Serializable {
  def blacklist: Permissions
  def whitelist: Permissions

  def packageBlacklist: Traversable[String]
  def packageWhitelist: Traversable[String]

  def permitAll: Boolean = false
  def handleUncaughtException(t: Thread, e: Throwable): Unit = {}
  def checkPermission(perm: Permission): Boolean =
    checkPermission(perm.asJavaPermission())
  def checkPermission(perm: JPermission): Boolean =
    permitAll || (!blacklist.validate(perm) && whitelist.validate(perm))
  def checkPackageAccess(pkg: String): Boolean =
    permitAll || (!packageBlacklist.exists(_ == pkg) && (packageWhitelist.isEmpty || packageWhitelist.exists(_ == pkg)))

}

@SerialVersionUID(29809023490L)
private[security2] sealed case class ContextImpl(
    override val permitAll: Boolean
  ,              blacklist: Permissions
  ,              whitelist: Permissions
  ,              packageBlacklist: Traversable[String]
  ,              packageWhitelist: Traversable[String]
) extends Context