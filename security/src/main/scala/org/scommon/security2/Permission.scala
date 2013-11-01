package org.scommon.security2

import java.security.{Permission => JPermission, BasicPermission => JBasicPermission}

import scala.language.implicitConversions
import org.scommon.core.StringUtil
import scala.collection.LinearSeq
import scala.util.control.Breaks

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
  val EMPTY: Permission = null

  private[this] def attemptToInstantiate(cls: Class[_], name: String, actions: String): Option[JPermission] = {
    type PermutationFilter = (String, String) => Boolean
    val PARAMETER_PERMUTATIONS: LinearSeq[(LinearSeq[String], PermutationFilter)] = LinearSeq(
        (LinearSeq(name, actions), (n, a) => (n ne null) && (a ne null))
      , (LinearSeq(name),          (_, _) => true)
      , (LinearSeq(name, actions), (_, _) => true)
      , (LinearSeq(),              (_, _) => true)
    )
      for {
        (permutation, filter) <- PARAMETER_PERMUTATIONS if filter(name, actions)
        constructor_params = permutation.map(_ => classOf[String])
      } {
        try {
          val constructor = cls.getConstructor(constructor_params:_*)
          return Some(constructor.newInstance(permutation:_*).asInstanceOf[JPermission])
        } catch {
          case _: ReflectiveOperationException =>
            //Do nothing
        }
      }
    None
  }

  def apply(tpe: String, name: String = null, actions: String = null): Permission = tpe match {
    case _ =>
      require(!((name eq null) && (actions ne null)), s"If actions is defined, then name must be as well")

      try {
        val cls = Class.forName(tpe)
        require(classOf[JPermission].isAssignableFrom(cls), s"$cls must be a subclass of ${classOf[JPermission]}")

        attemptToInstantiate(cls, name, actions) match {
          case Some(instance) =>
            fromJava(instance)
          case _ =>
            if ((tpe ne null) && (name eq null) && (actions ne null))
              new WrappedBasicPermission(tpe, actions)
            else if ((tpe ne null) && (name eq null) && (actions eq null))
              new WrappedBasicPermission(tpe)
            else
              throw new IllegalArgumentException(s"Unknown class $tpe")
      }
      } catch {
        case _: ClassNotFoundException =>
          if (name ne null)
            throw new IllegalArgumentException(s"Unknown class $tpe")
          new WrappedBasicPermission(tpe, actions)
      }
  }

  def apply(permission: JPermission): Permission =
    fromJava(permission)

  implicit def fromJava(permission: JPermission): Permission =
    new WrappedJavaPermission(permission)
}

@SerialVersionUID(23490821123098L)
sealed class WrappedJavaPermission(val java_permission: JPermission) extends Permission {
  override def toString = s"WrappedJavaPermission${java_permission.toString}"
  override def hashCode() = java_permission.hashCode()
  override def equals(obj: Any) = obj match {
    case o: WrappedJavaPermission =>
      java_permission.equals(o.java_permission)
    case o: java.security.Permission =>
      java_permission.equals(o)
    case o: Permission =>
      java_permission.equals(o.asJavaPermission())
    case _ =>
      false
  }

  def asJavaPermission(): JPermission =
    java_permission
}

@SerialVersionUID(12312938023234L)
sealed class WrappedBasicPermission(val name: String, val actions: String = null) extends JBasicPermission(name, if (actions ne null) actions else "") with Permission {
  override def toString = s"WrappedBasicPermission${super.toString}"
  override def equals(obj: Any) = obj match {
    case o: WrappedJavaPermission =>
      super.equals(o.java_permission)
    case o: java.security.Permission =>
      super.equals(o)
    case o: Permission =>
      super.equals(o.asJavaPermission())
    case _ =>
      false
  }

  def asJavaPermission(): JPermission =
    this
}