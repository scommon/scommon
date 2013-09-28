package org.scommon.script.engine.core

import org.scommon.security._

object CompileResult {
  type SerializableDiscoveredTypeMap = Map[String, Iterable[ClassDescription]]
  type SerializableDiscoveredEntryPoints = Iterable[ClassDescription]


  type RunInSandboxHandler[T] = (SerializableDiscoveredEntryPoints, SerializableDiscoveredTypeMap) => T
  type MutateSandboxHandler = MutableSandbox => Unit

  def apply(classRegistry: ClassRegistry, entryPointsDiscovered: SerializableDiscoveredEntryPoints, typesDiscovered: SerializableDiscoveredTypeMap = Map() withDefaultValue Iterable()) =
    StandardCompileResult(classRegistry, entryPointsDiscovered, typesDiscovered)
}

import CompileResult._

trait CompileResult {
  def classRegistry: ClassRegistry
  def typesDiscovered: SerializableDiscoveredTypeMap
  def entryPointsDiscovered: SerializableDiscoveredEntryPoints

  def withSandbox[U](fn: (MutableSandbox) => U): U =
    fn(Sandbox(classRegistry.toEnhancer()))

  def unitBasicRunInSandbox(fn: => Unit): Unit =
    unitRunInSandbox(_ => {})((_, _) => fn)

  def basicRunInSandbox[T <: Serializable](fn: => T): T =
    runInSandbox[T](_ => {})((_, _) => fn)

  def unitRunInSandboxWithTypes(fn: RunInSandboxHandler[Unit]): Unit =
    unitRunInSandbox(_ => {})(fn)

  def runInSandboxWithTypes[T <: Serializable](fn: RunInSandboxHandler[T]): T =
    runInSandbox[T](_ => {})(fn)

  def unitRunInSandbox(fnMutateSandbox: MutateSandboxHandler)(fn: RunInSandboxHandler[Unit]): Unit = {
    //Exercise caution here because we do not want to inadvertently introduce something that
    //cannot be serialized into the sandbox. So serialize out our data here instead of in
    //the run callback.
    val sandbox = Sandbox(classRegistry.toClassLoader())
    fnMutateSandbox(sandbox)

    sandbox.run[SerializableUnit] {
      fn(entryPointsDiscovered, typesDiscovered)
      SERIALIZABLE_UNIT
    }

    Unit
  }

  def runInSandbox[T <: Serializable](fnMutateSandbox: MutateSandboxHandler)(fn: RunInSandboxHandler[T]): T = {
    //Exercise caution here because we do not want to inadvertently introduce something that
    //cannot be serialized into the sandbox. So serialize out our data here instead of in
    //the run callback.
    val sandbox = Sandbox(classRegistry.toClassLoader())
    fnMutateSandbox(sandbox)

    sandbox.run[T] {
      fn(entryPointsDiscovered, typesDiscovered)
    }
  }
}

private[core] sealed case class StandardCompileResult(
    classRegistry        : ClassRegistry
  , entryPointsDiscovered: SerializableDiscoveredEntryPoints
  , typesDiscovered      : SerializableDiscoveredTypeMap
) extends CompileResult