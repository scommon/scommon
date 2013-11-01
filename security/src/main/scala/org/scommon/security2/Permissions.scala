package org.scommon.security2

import java.security.{Permission => JPermission, Permissions => JPermissions, PermissionCollection => JPermissionCollection}

object Permissions {
  def apply(): Permissions =
    new PermissionsImpl()

  def apply(permission: Permission, permissions: Permission*): Permissions =
    new PermissionsImpl().append(permissions :+ permission)

  def apply(permissions: Traversable[Permission]): Permissions =
    new PermissionsImpl().append(permissions)

  def apply(permission: JPermission, permissions: JPermission*): Permissions =
    new PermissionsImpl().append(Permission.fromJava(permission) +: permissions.map(Permission.fromJava))

  def apply(permissions: JPermissionCollection): Permissions = {
    import scala.collection.JavaConversions._
    new PermissionsImpl().append(permissions.elements().map(Permission.fromJava).toIterator)
  }
}

trait Permissions extends Serializable {
  def toJavaPermissions: JPermissions
  def permissions: Traversable[Permission]
  def validate(permission: JPermission): Boolean
  def append(permissions: TraversableOnce[Permission]): Permissions

  def validate(permission: Permission): Boolean =
    validate(permission.asJavaPermission())
  def append(permission: Permission, permissions: Permission*): Permissions =
    append(permission +: permissions)
  def ++(permissions: TraversableOnce[Permission]): Permissions =
    append(permissions)
  def ++(permission: Permission, permissions: Permission*): Permissions =
    append(permission +: permissions)
  def ++(permissions: Permissions): Permissions =
    append(permissions.permissions)
}

@SerialVersionUID(798267429369L)
private[security2] sealed class PermissionsImpl(val permissions: Seq[Permission] = Seq(), private[this] val my_permissions: JPermissions = new JPermissions()) extends Permissions {
import scala.collection.JavaConversions._
  override def hashCode() = my_permissions.hashCode()
  override def equals(obj: Any) = my_permissions.equals(obj)
  override def toString = my_permissions.elements().mkString("Permissions(", ", ", ")")

  def toJavaPermissions =
    my_permissions

  def append(permissions_to_append: TraversableOnce[Permission]): Permissions = {
    import scala.collection._

    val new_perms = new JPermissions()
    val perms = my_permissions.elements()

    while(perms.hasMoreElements) {
      new_perms.add(perms.nextElement())
    }

    val mutable_perms = mutable.ArrayBuffer[Permission]()
    for(perm <- permissions_to_append; if perm ne null) {
      new_perms.add(perm.asJavaPermission())
      mutable_perms += perm
    }

    new PermissionsImpl(permissions ++ permissions_to_append, new_perms)
  }

  def validate(permission: JPermission): Boolean =
    my_permissions.implies(permission)
}
