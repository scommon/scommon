package org.scommon.security2

import java.security.{Permission => JPermission, BasicPermission => JBasicPermission}

import scala.language.implicitConversions
import org.scommon.core.StringUtil

/**
 * @see [[java.security.Permission]]
 */
trait Permission {
  /** Provide a view of this object as a [[java.security.Permission]]. */
  def asJavaPermission(): JPermission

  /** Checks if the specified permission's actions are "implied by" this object's actions. */
  def validate(permission: JPermission): Boolean =
    asJavaPermission().implies(permission)

  /** Checks if the specified permission's actions are "implied by" this object's actions. */
  def validate(permission: Permission): Boolean =
    validate(permission.asJavaPermission())
}

object Permission {
  def apply(tpe: String, name: String = null, actions: String = null): Permission = tpe match {
    case _ =>
      require(!((name eq null) && (actions ne null)), s"If actions is defined, then name must be as well")
      try {
        val cls = Class.forName(tpe)
        require(classOf[JPermission].isAssignableFrom(cls), s"$cls must be a subclass of ${classOf[JPermission]}")

        val constructor_params =
          if ((name ne null) && (actions ne null))
            Seq(classOf[String], classOf[String])
          else if (name ne null)
            Seq(classOf[String])
          else
            Seq()

        val constructor = cls.getConstructor(constructor_params:_*)

        //Instantiate a java permission object and then wrap it and return the wrapped instance.
        fromJava((
          if ((name ne null) && (actions ne null))
            constructor.newInstance(name, actions)
          else if (name ne null)
            constructor.newInstance(name)
          else
            constructor.newInstance()
        ).asInstanceOf[JPermission])
      } catch {
        case t: ReflectiveOperationException =>
          if ((tpe ne null) && (name eq null) && (actions ne null))
            new WrappedBasicPermission(tpe, actions)
          else if ((tpe ne null) && (name eq null) && (actions eq null))
            new WrappedBasicPermission(tpe)
          else
            throw new IllegalArgumentException(s"Unknown class $tpe", t)
      }
  }

  implicit def fromJava(permission: JPermission): Permission =
    new WrappedJavaPermission(permission)
}

@SerialVersionUID(23490821123098L)
class WrappedJavaPermission(val java_permission: JPermission) extends Permission {
  override def hashCode() = java_permission.hashCode()
  override def equals(obj: Any) = java_permission.equals(obj)

  def asJavaPermission(): JPermission =
    java_permission
}

@SerialVersionUID(12312938023234L)
class WrappedBasicPermission(val name: String, val actions: String = null) extends JBasicPermission(name, if (actions ne null) actions else "") with Permission {
  def asJavaPermission(): JPermission =
    this
}