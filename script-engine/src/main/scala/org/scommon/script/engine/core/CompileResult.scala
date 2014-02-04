package org.scommon.script.engine.core

import scala.collection._

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

import org.scommon.security.SecurityContext
import scala.tools.reflect.{ToolBox, ToolBoxError}

sealed case class CompileResult ( //NOT serializable
    entryPoints  : CompileResult.SerializableDiscoveredEntryPoints
  , filterTypes  : CompileResult.SerializableDiscoveredTypeMap
  , classRegistry: ClassRegistry
) {
  def toClassLoader(parent: ClassLoader = Thread.currentThread().getContextClassLoader)(implicit context: SecurityContext) =
    classRegistry.toClassLoader(parent)(context)

  def discoverMainMethods(parent: ClassLoader = Thread.currentThread().getContextClassLoader)(implicit context: SecurityContext): Iterable[MethodMirror] =
    CompileResult.discoverMainMethods(entryPoints, toClassLoader(parent)(context))

  def discovered[T : TypeTag]: Iterable[ClassDescription] =
    discovered(CompilerTypeFilter[T])

  def discovered(typeFilter: CompilerTypeFilter): Iterable[ClassDescription] =
    filterTypes.getOrElse(typeFilter.name, Set())

  def discovered(typeName: String): Iterable[ClassDescription] =
    filterTypes.getOrElse(typeName, Set())
}

object CompileResult {
  type SerializableDiscoveredTypeMap = Map[String, immutable.Set[ClassDescription]]
  type SerializableDiscoveredEntryPoints = immutable.Set[ClassDescription]

  def discoverMainMethods(entryPoints: SerializableDiscoveredEntryPoints, classLoader: ClassLoader): immutable.Set[MethodMirror] = {
      val runtime_mirror = universe.runtimeMirror(classLoader)

      for {
        entry_point <- entryPoints
        //cls = Class.forName(entry_point.javaClassName, false, cl)
        module    = runtime_mirror.staticModule(entry_point.scalaClassName)
        obj       = runtime_mirror.reflectModule(module)
        reflected = runtime_mirror.reflect(obj.instance)
        method    = obj.symbol.typeSignature.member(TermName("main")).asMethod
        main      = reflected.reflectMethod(method)
      } yield main
    }

  def instantiate[T](description: ClassDescription, classLoader: ClassLoader)(args: Any*): Option[T] = {
    import scala.language.experimental.macros
    //import c.universe

    require(args.hasDefiniteSize, s"args must have a definite size")

    try {
      val runtime_mirror = universe.runtimeMirror(classLoader)
      val is_module = description.isTermName

      if (is_module)
        return Some {
          runtime_mirror.reflectModule(
            runtime_mirror.staticModule(description.scalaClassName)
          )
          .instance
          .asInstanceOf[T]
        }

      val args_size = args.size
      val args_type = args map (arg => runtime_mirror.reflect(arg).symbol.typeSignature)
      //println(args_type)

      val cls = runtime_mirror.staticClass(description.javaClassName)
      if (cls.isAbstractClass)
        return None

      //val arg_symbols = args map (arg => runtime_mirror.classSymbol(arg.getClass).asClass)
      val tb = runtime_mirror.mkToolBox()
      val dummyArgs = args.map(arg => q"null : ${runtime_mirror.classSymbol(arg.getClass).asClass.name}" /* IDE hint */.asInstanceOf[tb.u.Tree])
      val z = q"new $cls(..$dummyArgs)" /* IDE hint */.asInstanceOf[tb.u.Tree]
      val t = tb.typecheck(z, withImplicitViewsDisabled = false, withMacrosDisabled = true)
      if (t.isEmpty)
        return None

      t match {
        case q"new ${_}(..$foo)" =>
          val bar: List[Tree] = foo
          for (b <- bar)
            println(b)
          println(foo)
        case _ =>
      }

      for(x <- t)
        println(x)

//      t match {
//        case TypeTree(foo) =>
//      }

      val o = t.symbol

      println(t)

      println(cls)
      println("isAbstractType: " + cls.isAbstractType)
      println("isAbstractClass: " + cls.isAbstractClass)
      println("isTrait: " + cls.isTrait)
      println("isClass: " + cls.isClass)
      println("isCaseClass: " + cls.isCaseClass)
      println("isModule: " + cls.isModule)
      println("isModuleClass: " + cls.isModuleClass)
      val cls_type = cls.typeSignature
      println(cls_type)
      val cls_mirror = runtime_mirror.reflectClass(cls)
      println(cls_mirror)
      val constructors = cls_type.members.filter(m => m.isMethod && m.asMethod.isConstructor)
      println(constructors)
      val matching_constructors: Iterable[universe.type#MethodSymbol] =
        for {
          c: universe.type#Symbol <- constructors
          m = c.asMethod
          f = m.paramss.head
          _ = println(f)
          if f.hasDefiniteSize && f.size == args_size
          parameters_match = f.zip(args_type).forall {
            case (x, y) => println(s"${x.typeSignature} vs $y"); x.typeSignature <:< y
          }
          if parameters_match
        } yield m

      for {
        constructor <- matching_constructors.headOption
        constructor_mirror = cls_mirror.reflectConstructor(constructor)
      } yield constructor_mirror(args).asInstanceOf[T]

    } catch {
      case t:ToolBoxError =>
        //Should be thrown when unable to find a suitable constructor.
        //Possibly b/c there are too many arguments, not enough, or not the right types.
        println(t)
        None
      case t: Throwable =>
        println(t)
        None
    }
  }
}
