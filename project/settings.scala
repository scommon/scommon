import sbt._

object Settings {
  val project            = "scommon"

  val company            = "scommon"

  val organization       = "org.scommon"

  val homepage           = "https://github.com/scommon/"

  val vcsSpecification   = "git@github.com:scommon/scommon.git"

  val licenses           = Seq(
    License(
      name  = "The Apache Software License, Version 2.0",
      url   = "http://www.apache.org/licenses/LICENSE-2.0.txt"
    )
  )

  val developers         = Seq(
    Developer(
        id              = "David Hoyt"
      , name            = "David Hoyt"
      , email           = "dhoyt@hoytsoft.org"
      , url             = "http://www.hoytsoft.org/"
      , organization    = "HoytSoft"
      , organizationUri = "http://www.hoytsoft.org/"
      , roles           = Seq("architect", "developer")
    )
  )

  val scalaVersion       = "2.11.0-RC3"

  val scalacOptions      = Seq("-deprecation", "-unchecked", "-feature", "-Xelide-below", "900")
  val javacOptions       = Seq("-Xlint:unchecked")

  val prompt             = GitPrompt.build
}

