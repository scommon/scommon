package org.scommon.security

import java.security.{Permission => JPermission, Permissions => JPermissions}
import java.net.URI

object SecurityContext {
  def apply(uri: URI = null, codeSigners: Array[java.security.CodeSigner] = null, permitAll: Boolean = false, grants: Permissions = Permissions()) =
    new SecurityContextImpl(uri, codeSigners, permitAll, grants)
}

trait SecurityContext extends Serializable {
  def grants: Permissions

  def uri: URI = null
  def codeSigners: Array[java.security.CodeSigner] = null

  def permitAll: Boolean = false

  def tryCheckPermission(perm: JPermission): Boolean =
    permitAll || checkPermission(perm)
  def checkPermission(perm: Permission): Boolean =
    permitAll || checkPermission(perm.asJavaPermission())
  def checkPermission(perm: JPermission): Boolean =
    permitAll || (grants.validate(perm))

  def toJavaPermissions(): java.security.PermissionCollection = {
    if (!permitAll) grants.toJavaPermissions else {
      val p = new JPermissions()
      p.add(new java.security.AllPermission())
      p
    }
  }

  def toProtectionDomain(classLoader: ClassLoader = null): java.security.ProtectionDomain = {
    val cs = new java.security.CodeSource(if (uri ne null) uri.toURL else null, codeSigners)
    val perms = toJavaPermissions()
    if (classLoader eq null)
      new java.security.ProtectionDomain(cs, perms)
    else
      new java.security.ProtectionDomain(cs, perms, classLoader, Array())
  }

  def toAccessControlContext(classLoader: ClassLoader = null): java.security.AccessControlContext = {
    //http://media.techtarget.com/tss/static/articles/content/dm_security/Java2Sec.pdf
    val pd_grants = toProtectionDomain(classLoader)

    val combiner = new java.security.DomainCombiner {
      def combine(currentDomains: Array[java.security.ProtectionDomain], assignedDomains: Array[java.security.ProtectionDomain]) = {
        if (assignedDomains ne null)
          assignedDomains
        else if (currentDomains ne null)
          currentDomains
        else
          Array[java.security.ProtectionDomain]()
      }
    }

    val acc1 = new java.security.AccessControlContext(Array(pd_grants))
    new java.security.AccessControlContext(acc1, combiner)
  }
}

@SerialVersionUID(29809023490L)
private[security] sealed case class SecurityContextImpl(
    override val uri: URI
  , override val codeSigners: Array[java.security.CodeSigner]
  , override val permitAll: Boolean
  ,              grants: Permissions
) extends SecurityContext