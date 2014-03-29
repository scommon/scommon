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

  def instantiate[T](description: ClassDescription, classLoader: ClassLoader)(args: Any*): Option[T] =
    instantiateWithParameterLists[T](description, classLoader)(Seq(args.toSeq))

  def instantiateWithParameterLists[T](description: ClassDescription, classLoader: ClassLoader)(parameterLists: Iterable[Any]*): Option[T] = {
    val parameterListsAsSeq = (parameterLists map (_.toSeq)).toSeq
    instantiateWithIterableArgumentTypes[T](description, classLoader,
      for (parameterList <- parameterListsAsSeq) yield
        for(arg <- parameterList) yield
          if (arg != null) arg.getClass else classOf[Object]
      ,
      parameterListsAsSeq
    )
  }

  def instantiateWithArgumentTypes[T](description: ClassDescription, classLoader: ClassLoader)(argTypes: Class[_]*)(args: Any*): Option[T] =
    instantiateWithIterableArgumentTypes[T](description, classLoader, Seq(argTypes.toSeq), Seq(args.toSeq))

  private[this] def instantiateWithIterableArgumentTypes[T](description: ClassDescription, classLoader: ClassLoader, argTypes: Seq[Seq[Class[_]]], args: Seq[Seq[Any]]): Option[T] = {
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
      if (cls.isAbstract)
        return None

      val toolbox =
        runtime_mirror.mkToolBox()

      val build =
        universe.internal.reificationSupport

      //Create an AST that resembles a variable of the argument type bound to the argument's value.
      //TODO: Don't use 2 Seqs, instead use a single Seq[(Class[_], Any)]
      val ast_arguments_with_bound_values =
        for ((parameter_list_types, parameter_list_values) <- argTypes.zip(args)) yield {
          for ((arg_class, arg_value) <- parameter_list_types.zip(parameter_list_values)) yield {
            require(arg_class ne null, s"A class must be provided for every argument")

            val argument =
              build.setInfo(
                  build.newFreeTerm("x", arg_value)
                , runtime_mirror.classSymbol(arg_class).toType
              )

            //Construct AST for the free term.
            //Essentially just: Ident(argument)
            q"$argument" /* IDE hint */.asInstanceOf[toolbox.u.Tree]
          }
        }

      //Create an AST that represents instantiating the requested class with the provided arguments.
      //Given Z arguments across a total of M parameter lists, it effectively looks like:
      //  val x1 = <value arg 1, parameter list 1>
      //  val x2 = <value arg 2, parameter list 1>
      //  val x3 = <value arg 3, parameter list 1>
      //           ...
      //  val xX = <value arg 1, parameter list M>
      //  val xY = <value arg 2, parameter list M>
      //  val xZ = <value arg 3, parameter list M>
      //
      //  new Foo(x1, x2, x3)...(xX, xY, xZ)
      val ast_instantiate_class =
        q"new $cls(...$ast_arguments_with_bound_values)" /* IDE hint */.asInstanceOf[toolbox.u.Tree]

      //Handles all the hard parts for us by evaluating the AST, resolving the correct (potentially overloaded)
      //constructor, resolving and applying implicit conversions, and finally instantiating the new class.
      val new_instance =
        toolbox.eval(ast_instantiate_class)

      Option(new_instance.asInstanceOf[T])

//
//      //Older approach -- it is faster than using .eval() but may not work for all implicit conversions.
//      //   If it were possible to get this to work w/ implicit conversions it would be the better choice.
//
//      //Create an AST that resembles trying to cast ??? as an instance of each provided arg's class.
//      val dummy_parameter_tree_for_finding_constructor =
//        for (parameter_list_types <- argTypes) yield {
//          for (arg_class <- parameter_list_types) yield {
//            require(arg_class ne null, s"A class must be provided for every argument")
//
//            q"??? : ${runtime_mirror.classSymbol(arg_class).name}" /* IDE hint */.asInstanceOf[toolbox.u.Tree]
//          }
//        }
//
//      //Create an AST that represents instantiating the requested class given a list of parameters with the provided types.
//      //Expression looks like:
//      //  new Foo(??? : TypeOfArg1ParameterList1, ??? : TypeOfArg2ParameterList1, ..., ??? : TypeOfArgNParameterList1)
//      //         (??? : TypeOfArg1ParameterList2, ??? : TypeOfArg2ParameterList2, ..., ??? : TypeOfArgNParameterList2)
//      //         ...
//      //         (??? : TypeOfArg1ParameterListM, ??? : TypeOfArg2ParameterListM, ..., ??? : TypeOfArgNParameterListM)
//      val dummy_instantiation_tree_for_finding_constructor =
//        q"new $cls(...$dummy_parameter_tree_for_finding_constructor)" /* IDE hint */.asInstanceOf[toolbox.u.Tree]
//
//      //Use the toolbox typecheck() method to find the proper constructor given the arguments.
//      //If one cannot be found, an exception will be thrown. Do not enable "silent" since we don't want an
//      //error logged.
//      val type_checked_instantiation_tree_for_finding_constructor =
//        toolbox.typecheck(
//          dummy_instantiation_tree_for_finding_constructor,
//          withImplicitViewsDisabled = false,
//          withMacrosDisabled = false
//        )
//
//      type_checked_instantiation_tree_for_finding_constructor match {
//        //Extract the constructor if found.
//        case Apply(Apply(constructor @ Select(_, nme.CONSTRUCTOR), _), _) =>
//          //Use extra variables here simply for use in debugging.
//          val cls_mirror = runtime_mirror.reflectClass(cls)
//          val constructor_mirror = cls_mirror.reflectConstructor(constructor.symbol.asMethod)
//
//          //Instantiate a new instance of T with the provided arguments.
//          Option(constructor_mirror(args.flatten:_*).asInstanceOf[T])
//        case _ =>
//          None
//      }
//
    } catch {
      case t: ToolBoxError =>
        //Should be thrown when unable to find a suitable constructor.
        //Possibly b/c there are too many arguments, not enough, or not the right types.
        //t.printStackTrace()
        None
      case t: Throwable =>
        //t.printStackTrace()
        None
    }
  }
}
