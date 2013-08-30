import sbt._
import Keys._

object ProjectTemplates {
  object Root {
    def apply(aggregate:ProjectReference*):Project =
      apply("", aggregate)

    def apply(name:String, aggregate:Seq[ProjectReference]):Project = Project(
      id        = ThisProject.root(name),
      base      = ThisProject.root.base(),
      settings  = ThisProject.root.settings,

      aggregate = aggregate
    )
  }

  object Module {
    def apply(name:String):Project =
      apply("", name)

    def apply(prefix:String, name:String):Project = Project(
      id        = ThisProject.module(prefix, name),
      base      = ThisProject.module.base(name),
      settings  = ThisProject.module.settings
    )
  }
}

