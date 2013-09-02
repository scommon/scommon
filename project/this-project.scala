import sbt._
import Keys._

object ThisProject {
  object root {
    def apply:String =
      apply("root")

    def apply(name:String):String =
      if ("" == name)
        "root"
      else
        name.toLowerCase()

    def base(path:String = ".") = file(path)
    def version                 = Keys.version in ThisBuild
    def settings                = /* Do not publish */ Seq(publishArtifact := false) ++ PublishSettings.defaults ++ ReleaseSettings.defaults ++ BuildSettings.defaults ++ MavenSettings.defaults ++ Defaults.defaultSettings
  }

  object module {
    def apply(name:String):String =
      apply("", name)

    def apply(prefix:String, name:String):String =
      if ("" == prefix)
        "%s".format(name)
      else
        "%s-%s".format(prefix.toLowerCase(), name)

    def base(path:String)    = root.base(path)
    def version              = root.version
    def settings             = PublishSettings.defaults ++ ReleaseSettings.defaults ++ BuildSettings.defaults ++ MavenSettings.defaults ++ Defaults.defaultSettings
  }
}
