package org.scommon.script.engine.core

import org.scommon.core.Enum

object CompilerPhase extends Enum {
  sealed case class EnumVal private[CompilerPhase](order: Int, phaseName: String, title: String) extends Value with Ordered[EnumVal] {
    def compare(that: EnumVal) = CompilerPhase.compare(this, that)
  }

  val Custom                  = EnumVal(-2, "custom",                  "Custom")
  val Unknown                 = EnumVal(-1, "",                        "Unknown")

  //https://wiki.scala-lang.org/display/SIW/Overview+of+Compiler+Phases

  val Parser                  = EnumVal( 1, "parser",                  "Parser")
  val Namer                   = EnumVal( 2, "namer",                   "Namer")
  val PackageObjects          = EnumVal( 3, "packageobjects",          "PackageObjects")
  val Typer                   = EnumVal( 4, "typer",                   "Typer")
  val PatternMatching         = EnumVal( 5, "patmat",                  "Pattern Matching")
  val SuperAccessors          = EnumVal( 6, "superaccessors",          "Super Accessors")
  val ExtMethods              = EnumVal( 7, "extmethods",              "Ext Methods")
  val Pickler                 = EnumVal( 8, "pickler",                 "Pickler")
  val RefChecks               = EnumVal( 9, "refchecks",               "Ref Checks")
  val LiftCode                = EnumVal(10, "liftcode",                "Lift Code")
  val UnCurry                 = EnumVal(11, "uncurry",                 "Un Curry")
  val TailCalls               = EnumVal(12, "tailcalls",               "Tail Calls")
  val Specialize              = EnumVal(13, "specialize",              "Specialize")
  val ExplicitOuter           = EnumVal(14, "explicitouter",           "Explicit Outer")
  val Erasure                 = EnumVal(15, "erasure",                 "Erasure")
  val PostErasure             = EnumVal(16, "posterasure",             "Post Erasure")
  val LazyVals                = EnumVal(17, "lazyvals",                "Lazy Vals")
  val LambdaLift              = EnumVal(18, "lambdalift",              "Lambda Lift")
  val Constructors            = EnumVal(19, "constructors",            "Constructors")
  val Flatten                 = EnumVal(20, "flatten",                 "Flatten")
  val Mixin                   = EnumVal(21, "mixin",                   "Mixin")
  val Cleanup                 = EnumVal(22, "cleanup",                 "Clean Up")
  val ICode                   = EnumVal(23, "icode",                   "ICode")
  val Inliner                 = EnumVal(24, "inliner",                 "Inliner")
  val InlineExceptionHandlers = EnumVal(25, "inlineExceptionHandlers", "Inline Exception Handlers")
  val ClosureElimination      = EnumVal(26, "closelim",                "Closure Elimination")
  val DeadCode                = EnumVal(27, "dce",                     "Dead Code")
  val GenJVM                  = EnumVal(28, "genJVM",                  "GenJVM")

  val last: EnumVal = GenJVM

  def apply(phaseName: String): EnumVal =
    values.find(_.phaseName == phaseName).getOrElse(Unknown)

  def isBefore(phaseX: EnumVal, phaseY: EnumVal): Boolean =
    isKnown(phaseX) && isKnown(phaseY) && phaseX.order < phaseY.order

  def isAfter(phaseX: EnumVal, phaseY: EnumVal): Boolean =
    isKnown(phaseX) && isKnown(phaseY) && phaseX.order > phaseY.order

  def isKnown(phase: EnumVal): Boolean =
    phase != Unknown && phase != Custom

  def isKnown(phaseName: String): Boolean =
    isKnown(apply(phaseName))

  def compare(phaseX: EnumVal, phaseY: EnumVal): Int =
    if (!isKnown(phaseX) && !isKnown(phaseY)) 0
    else if (!isKnown(phaseX)) -1
    else if (!isKnown(phaseY)) 1
    else if (isBefore(phaseX, phaseY)) -1
    else if (isAfter(phaseX, phaseY)) 1
    else  0

  implicit val ordering = new Ordering[EnumVal] {
    def compare(x: EnumVal, y: EnumVal) = CompilerPhase.compare(x, y)
  }
}