package org.scommon.script.engine.core

import scala.collection.generic.{FilterMonadic, CanBuildFrom}
import scala.collection._


object ClassContents {
  def apply(payload: Array[Byte]) =
    StandardClassContents(payload.length, payload)
}

trait ClassContents extends Serializable {
  def size: Long
  def payload: Array[Byte]
}

@SerialVersionUID(234242438L)
private[core] sealed case class StandardClassContents(
    size: Long
  , payload: Array[Byte]
) extends ClassContents

object ClassDescription {
  def apply(scalaClassName: String, javaClassName: String, javaClassFileName: String, purportedJavaClassName: String): ClassDescription =
    StandardClassDescription(scalaClassName, javaClassName, javaClassFileName, purportedJavaClassName)
}

trait ClassDescription extends Serializable {
  def scalaClassName: String
  def javaClassName: String
  def javaClassFileName: String
  def purportedJavaClassName: String
}

private[core] sealed case class StandardClassDescription(
    scalaClassName:         String
  , javaClassName:          String
  , javaClassFileName:      String
  , purportedJavaClassName: String
) extends ClassDescription

object ClassEntry {
  def apply(description: ClassDescription, contents: ClassContents) =
    StandardClassEntry(description, contents)
}

trait ClassEntry {
  def description: ClassDescription
  def contents: ClassContents
}

private[core] sealed case class StandardClassEntry(
    description: ClassDescription
  , contents: ClassContents
) extends ClassEntry

object ClassRegistry {
  private val MINIMAL_PROTECTION_DOMAIN =
    new java.security.ProtectionDomain(null, new java.security.Permissions())

  def apply(entries: Iterable[ClassEntry]): ClassRegistry = {
    val m = mutable.HashMap[String, ClassEntry]()
    for (e <- entries)
      m += e.description.javaClassName -> e
    StandardClassRegistry(m)
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

  def toClassLoader(parent: ClassLoader = Thread.currentThread().getContextClassLoader()): ClassLoader = new ClassLoader(parent) {
    override def findClass(name: String): Class[_] = {
      entries.get(name) match {
        case Some(entry) =>
          defineClass(name, entry.contents.payload, 0, entry.contents.size.toInt, MINIMAL_PROTECTION_DOMAIN)
        case _ =>
          throw new ClassNotFoundException(s"Unable to locate class named $name")
      }
    }
  }
}

@SerialVersionUID(34535556L)
private[core] sealed case class StandardClassRegistry(
    protected val entries: Map[String, ClassEntry]
) extends ClassRegistry
