package org.scommon.script.engine.core

import scala.collection.generic.{FilterMonadic, CanBuildFrom}
import scala.collection.GenTraversableOnce
import scala.collection.immutable.HashMap
import net.datenwerke.sandbox.{SandboxLoader, SandboxLoaderEnhancer}

object ClassEntry {
  def apply(name: String, contents: Array[Byte]) =
    StandardClassEntry(name, contents.length, contents)
}

trait ClassEntry extends Serializable {
  def name: String
  def size: Long
  def contents: Array[Byte]
}

@SerialVersionUID(234242438L)
private[core] sealed case class StandardClassEntry(
    name: String
  , size: Long
  , contents: Array[Byte]
) extends ClassEntry

object ClassRegistry {
  def apply(classes: Iterable[ClassEntry]) = {
    val map = HashMap(classes.map(c => c.name -> c).toSeq:_*)
    StandardClassRegistry(map)
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
          entry.contents
        case _ =>
          null
      }
    }
  }
}

@SerialVersionUID(34535556L)
private[core] sealed case class StandardClassRegistry(
    protected val classes: HashMap[String, ClassEntry]
) extends ClassRegistry