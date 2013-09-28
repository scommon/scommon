package org.scommon.script.engine

import _root_.scala.annotation.tailrec

import _root_.scala.collection.mutable
import _root_.scala.tools.nsc

object ScalaCompilerUtils {
  /**
   * Constructs a java-compliant class name.
   *
   * For example, suppose we had the following:
   *
   * <code>
   * package foo

   * class A {
   *   class AA {
   *     class A
   *     class B
   *   }
   *   class AB {
   *     class A
   *     class B
   *     class C
   *   }
   * }
   * </code>
   *
   * If we want to find the actual Java class file name for A.AB.C, we would need to determine that C is enclosed by AB which is
   * enclosed by A which is in the package foo.
   *
   * This method provides the equivalent name for a provided symbol. So it would start with "C", then prepend "AB$"
   * since the dollar sign denotes the AB enclosing class, then it would prepend "A$" and finally "foo" to give us, in the end,
   * the following result:
   *
   * foo.A$AB$C
   *
   * Replacing the '.' with a '/' and appending ".class" to the end would give us the path to the corresponding Java class file.
   */
  def trueJavaClassName(global: nsc.Global)(sym: global.Symbol): String = {
    val innerClassBuffer = mutable.LinkedHashSet[global.Symbol]()

    def innerClassSymbolFor(s: global.Symbol): global.Symbol =
      if (s.isClass) s else if (s.isModule) s.moduleClass else global.NoSymbol

    /**
     * Checks if given symbol corresponds to inner class/object and add it to innerClassBuffer
     *
     * Note: This method is called recursively thus making sure that we add complete chain
     * of inner class all until root class.
     */
    def collectInnerClass(s: global.Symbol): Unit = {
      /** For given symbol return a symbol corresponding to a class that should be declared as inner class.
        *
        *  For example:
        *  class A {
        *    class B
        *    object C
        *  }
        *
        *  then method will return:
        *    NoSymbol for A,
        *    the same symbol for A.B (corresponding to A$B class), and
        *    A$C$ symbol for A.C.
        */
      val x =
        innerClassSymbolFor(s)

      if(x ne global.NoSymbol) {
        assert(x.isClass, "not an inner-class symbol")
        val isInner = !x.rawowner.isPackageClass
        if (isInner) {
          innerClassBuffer += x
          collectInnerClass(x.rawowner)
        }
      }
    }

    collectInnerClass(sym)

    //for (s <- List(sym, sym.linkedClassOfClass); m <- s.info.decls.map(x => innerClassSymbolFor(x)) if m.isClass)
    //  innerClassBuffer += m

    /**
     * Constructs the java-compliant class name. This is where the real work for this method is done after we've examined our hierarchy
     * and populated a list (innerClassBuffer) with the provided symbol's enclosing classes.
     *
     * For example, suppose we had the following:
     *
     * <code>
     * package foo

     * class A {
     *   class AA {
     *     class A
     *     class B
     *   }
     *   class AB {
     *     class A
     *     class B
     *     class C
     *   }
     * }
     * </code>
     *
     * If we want to find the actual Java class file name for A.AB.C, we would need to determine that C is enclosed by AB which is
     * enclosed by A which is in the package foo.
     *
     * This method will traverse that hierarchy, accumulating the name as it moves. So it would start with "C", then prepend "AB$"
     * since the dollar sign denotes the AB enclosing class, then it would prepend "A$" and finally "foo" to give us, in the end,
     * the following result:
     *
     * foo.A$AB$C
     *
     * Replacing the '.' with a '/' and appending ".class" to the end would give us the path to the corresponding Java class file.
     */
    @tailrec
    def fullNameInternal(s: global.Symbol, separator: Char, accum: global.Name): global.Name = {
      if (s.isRoot || s.isRootPackage || s == global.NoSymbol) accum prepend s.name
      else if (s.owner.isEffectiveRoot) accum prepend s.name
      else {
        val enclosing_class = s.effectiveOwner.enclClass
        val inner = innerClassSymbolFor(s)
        val name =
          if (innerClassBuffer.contains(inner))
            s.name prepend (if (enclosing_class.needsModuleSuffix) enclosing_class.moduleSuffix else "")
          else
            s.name prepend separator

        fullNameInternal(enclosing_class, separator, accum prepend name)
      }
    }

    /** Adds a $ (the module suffix) if needed. */
    def addModuleSuffix(s: global.Symbol, n: global.Name): global.Name =
      if (s.needsModuleSuffix && !s.isModuleClass)
        n append s.moduleSuffix
      else
        n

    /** Drops the specified n number of characters from the provided name. */
    def dropRight(name: global.Name, n: Int): global.Name =
      name.subName(0, name.length - n)

    /** Removes any single space on the end of the name if present. */
    def dropLocalSuffix(name: global.Name): global.Name  =
      if (name endsWith ' ') dropRight(name, 1) else name

    /** Check if the symbol potentially has a full path to the name or if we can get by using a simple name. */
    def hasInternalName(s: global.Symbol) =
      s.isClass || (s.isModule && !s.isMethod)

    def binaryName(s: global.Symbol) =
      addModuleSuffix(s, fullNameInternal(s, '.', global.newTermName("")))

    def simpleName(s: global.Symbol) =
      addModuleSuffix(s, dropLocalSuffix(s.simpleName))

    val java_class_name =
      if (hasInternalName(sym))
        binaryName(sym)
      else
        simpleName(sym)

    java_class_name.toString.replaceAllLiterally("/", ".")
  }
}
