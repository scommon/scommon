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
    instantiateWithIterableArgumentTypes(description, classLoader,
      for(arg <- args)
        yield if (arg != null) arg.getClass else classOf[Object],
      args
    )
  }

  def instantiateWithArgumentTypes[T](description: ClassDescription, classLoader: ClassLoader)(argTypes: Class[_]*)(args: Any*): Option[T] =
    instantiateWithIterableArgumentTypes(description, classLoader, argTypes, args)

  private[this] def instantiateWithIterableArgumentTypes[T](description: ClassDescription, classLoader: ClassLoader, argTypes: Seq[Class[_]], args: Seq[Any]): Option[T] = {
    //import scala.language.experimental.macros
    //import c.universe

    require(args.hasDefiniteSize, s"args must have a definite size")

    try {
      val runtime_mirror = universe.runtimeMirror(classLoader)
      val is_module = description.isTermName

      //If what's requested is a module (object) and arguments have been given,
      //then we should assume that we should not instantiate and return the module.
      if (is_module && !argTypes.isEmpty)
        return None

      if (is_module)
        return Some {
          runtime_mirror.reflectModule(
            runtime_mirror.staticModule(description.scalaClassName)
          )
          .instance
          .asInstanceOf[T]
        }

      val cls = runtime_mirror.staticClass(description.javaClassName)
      if (cls.isAbstractClass)
        return None

      val toolbox =
        runtime_mirror.mkToolBox()

      //Create an AST that resembles trying to cast null as an instance of each provided arg's class.
      val dummy_parameter_tree_for_finding_constructor =
        argTypes map { arg_class =>
          require(arg_class ne null, s"A class must be provided for every argument")

          q"null : ${runtime_mirror.classSymbol(arg_class).asClass.name}" /* IDE hint */.asInstanceOf[toolbox.u.Tree]
        }

      //Create an AST that represents instantiating the requested class given a list of parameters with the provided types.
      //Expression looks like:
      //  new Foo(null: TypeOfArg1, null: TypeOfArg2, ..., null: TypeOfArgN)
      val dummy_instantiation_tree_for_finding_constructor = q"new $cls(..$dummy_parameter_tree_for_finding_constructor)" /* IDE hint */.asInstanceOf[toolbox.u.Tree]

      //Use the toolbox typecheck() method to find the proper constructor given the arguments.
      //If one cannot be found, an exception will be thrown. Do not enable "silent" since we don't want an
      //error logged.
      val type_checked_instantiation_tree_for_finding_constructor =
        toolbox.typecheck(
          dummy_instantiation_tree_for_finding_constructor,
          withImplicitViewsDisabled = false,
          withMacrosDisabled = true
        )

      if (type_checked_instantiation_tree_for_finding_constructor.isEmpty)
        return None

      //After doing this, a List will be provided that contains the exact types of the parameters in the proper constructor.
      //Afterwards it's simply a matter of traversing constructors and finding an exact match.
      val list_of_exact_parameter_types_for_constructor: List[universe.Type] = type_checked_instantiation_tree_for_finding_constructor match {
        //Extract the AST representing the parameters and their types for the found matching constructor.
        case q"new ${_}(..$type_checked_parameters_tree)" =>
          //Pull out the type of each parameter.
          for (tree_for_parameter <- type_checked_parameters_tree: List[Tree]) yield tree_for_parameter match {

            //Given an expression like:
            // (null: scala.Predef.String)
            //Return:
            // scala.Predef.String
            case Typed(_ /* expression */, tree_for_type_of_constructor_parameter: TypeTree) =>
              val type_of_constructor_parameter = tree_for_type_of_constructor_parameter.tpe.normalize
              type_of_constructor_parameter

            //Given an implicit view like:
            //  scala.this.Predef.Integer2int((null: java.lang.Integer)): scala.Int
            //Return:
            // scala.Int
            case Apply(fun  @ Select(_, _), List(Typed(_, _))) =>
              val type_of_constructor_parameter = fun.symbol.asMethod.returnType.normalize
              type_of_constructor_parameter

            //Unknown tree -- get out of here.
            case _ =>
              return None
          }
        case _ =>
          return None
      }

      val args_size = argTypes.size

      val cls_type = cls.typeSignature
      val cls_mirror = runtime_mirror.reflectClass(cls)

      //TODO: Are multiple parameter lists in a constructor a problem here?

      val matching_constructors: Iterable[universe.type#MethodSymbol] =
        for {
          member <- cls_type.members
          if member.isMethod
          method = member.asMethod
          if method.isConstructor
          constructor = method

          single_parameter_list <- constructor.paramss.headOption //Get the first parameter list

          //_ = println(f)
          if single_parameter_list.hasDefiniteSize && single_parameter_list.size == args_size

          //Determine if the parameter types for this constructor matches what we're looking for.
          parameters_match = single_parameter_list.zip(list_of_exact_parameter_types_for_constructor).forall {
            case (x, y) =>
              x.typeSignature =:= y
          }
          if parameters_match
        } yield constructor

      for {
        constructor <- matching_constructors.headOption
        constructor_mirror = cls_mirror.reflectConstructor(constructor)
      } yield constructor_mirror(args:_*).asInstanceOf[T]

    } catch {
      case t:ToolBoxError =>
        //Should be thrown when unable to find a suitable constructor.
        //Possibly b/c there are too many arguments, not enough, or not the right types.
        None
      case t: Throwable =>
        None
    }
  }
}
