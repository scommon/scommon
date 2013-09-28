package org.scommon.script.engine.core

import scala.collection.generic.{FilterMonadic, CanBuildFrom}
import scala.collection.GenTraversableOnce
import scala.collection.immutable.HashMap
import scala.collection._

import net.datenwerke.sandbox.{SandboxLoader, SandboxLoaderEnhancer}

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
  def apply(entries: Iterable[ClassEntry]): ClassRegistry = {
    val m = mutable.HashMap[String, ClassEntry]()
    for {
      e <- entries
      e2 <- Seq(
        //e.description.scalaClassName         -> e,
        e.description.javaClassName          -> e
      )
    } m += e2
    StandardClassRegistry(m)
  }
}

trait ClassRegistry extends FilterMonadic[ClassEntry, Iterable[ClassEntry]]
with Serializable  {

  protected def classes: Map[String, ClassEntry]

  def map[B, That](f: (ClassEntry) => B)(implicit bf: CanBuildFrom[Iterable[ClassEntry], B, That]) =
    classes.values.map(f)(bf)

  def flatMap[B, That](f: (ClassEntry) => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Iterable[ClassEntry], B, That]) =
    classes.values.flatMap(f)(bf)

  def foreach[U](f: (ClassEntry) => U) =
    classes.values.foreach(f)

  def withFilter(p: (ClassEntry) => Boolean) =
    classes.values.withFilter(p)

  def toClassLoader(parent: ClassLoader = Thread.currentThread().getContextClassLoader()): ClassLoader = new ClassLoader(parent) {
    override def findClass(name: String): Class[_] = {
      classes.get(name) match {
        case Some(entry) =>
          defineClass(name, entry.contents.payload, 0, entry.contents.size.toInt)
        case _ =>
          throw new ClassNotFoundException(s"Unable to locate class named $name")
      }
    }
  }

  def toEnhancer(): SandboxLoaderEnhancer = new SandboxLoaderEnhancer {
    def classtoBeLoaded(sandboxLoader: SandboxLoader, name: String, resolve: Boolean): Unit = {}
    def classLoaded(sandboxLoader: SandboxLoader, name: String, clazz: Class[_]): Unit = {}
    def isLoadClassWithApplicationLoader(name: String) = false
    def enhance(sandboxLoader: SandboxLoader, name: String, cBytes: Array[Byte]) = cBytes //Do not manipulate the bytes

    def checkClassAccess(name: String) =
      classes.contains(name)

    def loadClass(sandboxLoader: SandboxLoader, name: String) = {
      classes.get(name) match {
        case Some(entry) =>
          entry.contents.payload
        case _ =>
          null
      }
    }
  }
}

@SerialVersionUID(34535556L)
private[core] sealed case class StandardClassRegistry(
    protected val classes: Map[String, ClassEntry]
) extends ClassRegistry