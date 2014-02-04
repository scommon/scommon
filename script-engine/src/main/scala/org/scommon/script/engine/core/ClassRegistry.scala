package org.scommon.script.engine.core

import scala.collection._
import scala.collection.generic.{FilterMonadic, CanBuildFrom}

import org.scommon.security.SecurityContext

trait ClassContents extends Serializable {
  def size: Long
  def payload: Array[Byte]
}

object ClassContents {
  @SerialVersionUID(234242438L)
  private[core] sealed case class InnerClassContents(
      eventual_size: () => Long
    , eventual_payload: () => Array[Byte]
  ) extends ClassContents {
    lazy val size: Long = eventual_size()
    lazy val payload: Array[Byte] = eventual_payload()
    override def toString = s"ClassContents(size = <lazily eval>, payload = <lazily eval>)"
  }

  def apply(payload: Array[Byte]): ClassContents =
    InnerClassContents(() => payload.length, () => payload)

  def apply(size: => Long)(payload: => Array[Byte]): ClassContents =
    InnerClassContents(() => size, () => payload)

  def unapply(c: ClassContents) = c match {
    case i: InnerClassContents => InnerClassContents.unapply(i)
    case _ => None
  }
}

sealed case class ClassDescription(
    scalaClassName        : String
  , javaClassName         : String
  , javaClassFileName     : String
  , purportedJavaClassName: String
  , isTermName            : Boolean = false
  , isTypeName            : Boolean = true
) extends Serializable

sealed case class ClassEntry (
    description: ClassDescription
  , contents   : ClassContents
)

object ClassRegistry {
  private val MINIMAL_PROTECTION_DOMAIN =
    new java.security.ProtectionDomain(null, new java.security.Permissions())

  @SerialVersionUID(34535556L)
  private[this] sealed case class InnerClassRegistry(
      protected val entries: Map[String, ClassEntry]
  ) extends ClassRegistry {
    override def toString = s"ClassRegistry($entries)"
  }

  def apply(entries: Iterable[ClassEntry]): ClassRegistry = {
    val m = mutable.HashMap[String, ClassEntry]()
    for (e <- entries)
      m += e.description.javaClassName -> e
    InnerClassRegistry(m)
  }
}

trait ClassRegistry extends FilterMonadic[ClassEntry, Iterable[ClassEntry]]
with Serializable  {
  import ClassRegistry._

  protected def entries: Map[String, ClassEntry]

  def map[B, That](f: (ClassEntry) => B)(implicit bf: CanBuildFrom[Iterable[ClassEntry], B, That]) =
    entries.values.map(f)(bf)

  def flatMap[B, That](f: (ClassEntry) => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Iterable[ClassEntry], B, That]) =
    entries.values.flatMap(f)(bf)

  def foreach[U](f: (ClassEntry) => U) =
    entries.values.foreach(f)

  def withFilter(p: (ClassEntry) => Boolean) =
    entries.values.withFilter(p)

  def toClassLoader(parent: ClassLoader = Thread.currentThread().getContextClassLoader())(implicit context: SecurityContext): ClassLoader = new ClassLoader(parent) {
    val protection_domain = context.toProtectionDomain(this)

    override def findClass(name: String): Class[_] = {
      entries.get(name) match {
        case Some(entry) =>
          defineClass(name, entry.contents.payload, 0, entry.contents.size.toInt, protection_domain)
        case _ =>
          throw new ClassNotFoundException(s"Unable to locate class named $name")
      }
    }
  }
}
