package org.scommon.script.engine.core

import org.scommon.security._

object CompileResult {
  type SerializableDiscoveredTypeMap = Map[String, Iterable[ClassDescription]]
  type SerializableDiscoveredEntryPoints = Iterable[ClassDescription]


//  type RunInSandboxHandler[T] = SandboxData => T

  def apply(entryPoints: SerializableDiscoveredEntryPoints, filterTypes: SerializableDiscoveredTypeMap, classRegistry: ClassRegistry) =
    StandardCompileResult(entryPoints, filterTypes, classRegistry)
}

import CompileResult._

///** Packages up data for use when running in a sandbox. All contents must be serializable. */
//trait SandboxData extends Serializable {
//  def entryPoints: SerializableDiscoveredEntryPoints
//  def filterTypes: SerializableDiscoveredTypeMap
//  def classLoader: ClassLoader =
//    //Unfortunately this relies on behavior in MutableSandbox#execute().
//    Thread.currentThread().getContextClassLoader()
//}
//
//object SandboxData {
//  def unapply(sd: SandboxData): Option[(SerializableDiscoveredEntryPoints, SerializableDiscoveredTypeMap, ClassLoader)] =
//    if (sd ne null)
//      Some((sd.entryPoints, sd.filterTypes, sd.classLoader))
//    else
//      None
//}
//
//private[core] sealed case class StandardSandboxData(
//    entryPoints: SerializableDiscoveredEntryPoints
//  , filterTypes: SerializableDiscoveredTypeMap
//) extends SandboxData

trait CompileResult { //NOT serializable
  def entryPoints: SerializableDiscoveredEntryPoints
  def filterTypes: SerializableDiscoveredTypeMap
  def classRegistry: ClassRegistry

//  def withSandbox[U](fn: (MutableSandbox) => U): U =
//    fn(Sandbox(classRegistry.toClassLoader()))
//
//  def withSandbox[U](parent: ClassLoader)(fn: (MutableSandbox) => U): U =
//    fn(Sandbox(classRegistry.toClassLoader(parent)))
//
//  def unitBasicRunInSandbox(fn: => Unit): Unit =
//    unitRunInSandbox(_ => {})((_) => fn)
//
//  def basicRunInSandbox[T <: Serializable](fn: => T): T =
//    runInSandbox[T](_ => {})((_) => fn)
//
//  def unitRunInSandboxWithTypes(fn: RunInSandboxHandler[Unit]): Unit =
//    unitRunInSandbox(_ => {})(fn)
//
//  def runInSandboxWithTypes[T <: Serializable](fn: RunInSandboxHandler[T]): T =
//    runInSandbox[T](_ => {})(fn)
//
//  def unitRunInSandbox(fnMutateSandbox: MutateSandboxHandler)(fn: RunInSandboxHandler[Unit]): Unit = {
//    //Exercise caution here because we do not want to inadvertently introduce something that
//    //cannot be serialized into the sandbox. So serialize out our data here instead of in
//    //the run callback.
//    val class_loader = classRegistry.toClassLoader()
//    val sandbox = Sandbox(class_loader)
//    val data = StandardSandboxData(entryPoints, filterTypes)
//
//    //Mark all classes from the registry as available in the sandbox.
//    for(e <- classRegistry; class_name = e.description.javaClassName)
//      sandbox.context.addClassPermission(AccessType.PERMIT, Mode.NORMAL, class_name)
//
//    //TODO: Need to add add'l classes/jars in the compiler settings to the sandbox's permissions.
//
//    fnMutateSandbox(sandbox)
//
//    sandbox.run[SerializableUnit] {
//      fn(data)
//      SERIALIZABLE_UNIT
//    }
//
//    Unit
//  }
//
//  def runInSandbox[T <: Serializable](fnMutateSandbox: MutateSandboxHandler)(fn: RunInSandboxHandler[T]): T = {
//    //Exercise caution here because we do not want to inadvertently introduce something that
//    //cannot be serialized into the sandbox. So serialize out our data here instead of in
//    //the run callback.
//    val class_loader = classRegistry.toClassLoader()
//    val sandbox = Sandbox(class_loader)
//    val data = StandardSandboxData(entryPoints, filterTypes)
//
//    //Mark all classes from the registry as available in the sandbox.
//    for(e <- classRegistry; class_name = e.description.javaClassName)
//      sandbox.context.addClassPermission(AccessType.PERMIT, Mode.NORMAL, class_name)
//
//    //TODO: Need to add add'l classes/jars in the compiler settings to the sandbox's permissions.
//
//    fnMutateSandbox(sandbox)
//
//    sandbox.run[T] {
//      fn(data)
//    }
//  }
}

private[core] sealed case class StandardCompileResult(
    entryPoints  : SerializableDiscoveredEntryPoints
  , filterTypes  : SerializableDiscoveredTypeMap
  , classRegistry: ClassRegistry
) extends CompileResult