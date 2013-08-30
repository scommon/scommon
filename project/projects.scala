import sbt._
import Settings._
import ProjectTemplates._

object Projects extends Build {
  lazy val aaa_default_project = root
  
  lazy val root = Root(
      core
    , io
	, script_engine
  )
  
  lazy val core          = Module(project, "core")

  lazy val io            = Module(project, "io")
    .dependsOn(core % "compile")

  lazy val script_engine = Module(project, "script-engine")
    .dependsOn(core % "compile")
    .dependsOn(io % "compile")

//  lazy val logging     = Module(project, "logging")
//    .dependsOn(core % "compile")
//
//  lazy val platform    = Module(project, "platform")
//    .dependsOn(core % "compile")

}

