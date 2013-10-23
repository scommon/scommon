package org.scommon.security2

import java.security.{Permission => JPermission, Permissions => JPermissions}

object Permissions {
  def apply(): Permissions =
    new PermissionsImpl()

  def apply(permission: Permission, permissions: Permission*): Permissions =
    new PermissionsImpl().append(permissions :+ permission)

  def apply(permissions: Iterable[Permission]): Permissions =
    new PermissionsImpl().append(permissions)
}

trait Permissions extends Serializable {
  def validate(permission: JPermission): Boolean
  def append(permissions: Iterable[Permission]): Permissions

  def validate(permission: Permission): Boolean =
    validate(permission.asJavaPermission())
  def append(permission: Permission, permissions: Permission*): Permissions =
    append(permissions :+ permission)
  def ++(permissions: Iterable[Permission]): Permissions =
    append(permissions)
  def ++(permission: Permission, permissions: Permission*): Permissions =
    append(permissions :+ permission)
}

@SerialVersionUID(798267429369L)
private[security2] sealed class PermissionsImpl(private[this] val my_permissions: JPermissions = new JPermissions()) extends Permissions {
import scala.collection.JavaConversions._
  override def hashCode() = my_permissions.hashCode()
  override def equals(obj: Any) = my_permissions.equals(obj)
  override def toString = my_permissions.elements().mkString("Permissions(", ", ", ")")

  def append(permissions: Iterable[Permission]): Permissions = {
    val new_perms = new JPermissions()
    val perms = my_permissions.elements()

    while(perms.hasMoreElements) {
      new_perms.add(perms.nextElement())
    }

    for(perm <- permissions)
      new_perms.add(perm.asJavaPermission())

    new PermissionsImpl(new_perms)
  }

  def validate(permission: JPermission): Boolean =
    my_permissions.implies(permission)
}
