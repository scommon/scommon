package org.scommon.security2

import java.security.{Permission => JPermission, Permissions => JPermissions}

object Context {
  def apply(permitAll: Boolean = false, grants: Permissions = Permissions()) =
    new ContextImpl(permitAll, grants)
}

trait Context extends Serializable {
  def grants: Permissions

  def permitAll: Boolean = false

  def tryCheckPermission(perm: JPermission): Boolean =
    permitAll || checkPermission(perm)
  def checkPermission(perm: Permission): Boolean =
    permitAll || checkPermission(perm.asJavaPermission())
  def checkPermission(perm: JPermission): Boolean =
    permitAll || (grants.validate(perm))

  def toAccessControlContext(useContextClassLoader: Boolean = false): java.security.AccessControlContext = {
    //http://media.techtarget.com/tss/static/articles/content/dm_security/Java2Sec.pdf
    val cs = new java.security.CodeSource(null, null.asInstanceOf[Array[java.security.CodeSigner]])
    val perms = if (!permitAll) grants.toJavaPermissions else {
      val p = new JPermissions()
      p.add(new java.security.AllPermission())
      p
    }

    val pd_grants =
      if (!useContextClassLoader)
        new java.security.ProtectionDomain(cs, perms)
      else
        new java.security.ProtectionDomain(cs, perms, SecurityManager.privilegedContextClassLoader, Array())

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
private[security2] sealed case class ContextImpl(
    override val permitAll: Boolean
  ,              grants: Permissions
) extends Context