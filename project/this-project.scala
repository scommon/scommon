import sbt._
import Keys._

object ThisProject {
  object root {
    def apply:String =
      apply("")

    def apply(name:String):String =
      if ("" == name)
        Settings.project.toLowerCase()
      else
        name.toLowerCase()

    def base(path:String = ".") = file(path)
    def version                 = Keys.version in ThisBuild
    def settings                = ReleaseSettings.defaults ++ BuildSettings.defaults ++ Defaults.defaultSettings
  }

  object module {
    def apply(name:String):String =
      apply("", name)

    def apply(prefix:String, name:String):String =
      if ("" == prefix)
        "%s-%s".format(Settings.project.toLowerCase(), name)
      else
        "%s-%s".format(prefix.toLowerCase(), name)

    def base(path:String)    = root.base(path)
    def version              = root.version
    def settings             = root.settings
  }
}
