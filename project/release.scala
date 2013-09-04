import sbt._
import Keys._

import sbtrelease._
import ReleaseStateTransformations._
import ReleasePlugin._
import ReleaseKeys._

object ReleaseSettings {
  val defaults = releaseSettings ++ Seq(
    //Customize the next version string to bump the revision number.
    nextVersion := { ver => Version(ver).map(determineNextVersion(_)).getOrElse(versionFormatError) }
  )

  def determineNextVersion(version: Version): String = {
    version
      .bumpBugfix
      .asSnapshot
      .string
  }
}
