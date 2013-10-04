package org.scommon.security2

import java.security.{Permission => JPermission}

import scala.language.implicitConversions

/**
 * @see [[java.security.Permission]]
 */
trait Permission {
  /** Provide a view of this object as a [[java.security.Permission]]. */
  def asJavaPermission(): JPermission

  /** Checks if the specified permission's actions are "implied by" this object's actions. */
  def validate(permission: JPermission): Boolean =
    permission.implies(permission)

  /** Checks if the specified permission's actions are "implied by" this object's actions. */
  def validate(permission: Permission): Boolean =
    validate(permission.asJavaPermission())
}

object Permission {
  def apply(tpe: String, name: String, actions: String = null): Permission = tpe match {
    case _ =>
      try {
        val cls = Class.forName(tpe)
        require(classOf[JPermission].isAssignableFrom(cls), s"$cls must be a subclass of ${classOf[JPermission]}")

        val constructor_params =
          if (actions ne null)
            Seq(classOf[String], classOf[String])
          else
            Seq(classOf[String])
        val constructor = cls.getConstructor(constructor_params:_*)

        //Instantiate a java permission object and then wrap it and return the wrapped instance.s
        fromJava(constructor.newInstance(name, actions).asInstanceOf[JPermission])
      } catch {
        case t: ReflectiveOperationException =>
          throw new IllegalArgumentException(s"Unknown class $tpe", t)
      }
  }

  implicit def fromJava(permission: JPermission): Permission =
    new WrappedJavaPermission(permission)
}

class WrappedJavaPermission(val java_permission: JPermission) extends Permission {
  override def hashCode() = java_permission.hashCode()
  override def equals(obj: Any) = java_permission.equals(obj)

  def asJavaPermission(): JPermission =
    java_permission
}