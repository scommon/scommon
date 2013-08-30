import sbt._
import Keys._

object BuildSettings {

  val defaults = Seq(

      shellPrompt   :=  Settings.prompt
    , organization  :=  Settings.organization
    , scalaVersion  :=  Settings.scalaVersion
    , scalacOptions :=  Settings.scalacOptions
    , javacOptions  :=  Settings.javacOptions

  )

}

