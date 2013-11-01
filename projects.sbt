import ProjectTemplates._


lazy val aaa_default_project = root

lazy val root = Root(
    core
  , io
  , logging
  , reflect
  , reactive
  , security
  , platform
  , script_engine
)

lazy val core          = Module("core")

lazy val io            = Module("io")
  .dependsOn(core % "compile")

lazy val logging       = Module("logging")
  .dependsOn(core % "compile")

lazy val reflect       = Module("reflect")
  .dependsOn(core % "compile")

lazy val reactive      = Module("reactive")
  .dependsOn(core % "compile")

lazy val security      = Module("security")
  .dependsOn(core % "compile")
  .dependsOn(io % "compile")
  .dependsOn(logging % "compile")

lazy val platform      = Module("platform")
  .dependsOn(core % "compile")

lazy val script_engine = Module("script-engine")
  .dependsOn(core % "compile")
  .dependsOn(io % "compile")
  .dependsOn(logging % "compile")
  .dependsOn(reflect % "compile")
  .dependsOn(reactive % "compile")
  .dependsOn(security % "compile")
