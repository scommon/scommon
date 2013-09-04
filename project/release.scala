import sbt._

import Keys._

import sbtrelease._
import ReleaseStateTransformations._
import ReleasePlugin._
import ReleaseKeys._
import Utilities._

import com.typesafe.sbt.SbtPgp.PgpKeys._

object ReleaseSettings {
  val defaults = releaseSettings ++ Seq(
    //Customize the steps of the release process.
    releaseProcess := Seq[ReleaseStep](
        checkSnapshotDependencies              //
      , runTest                                //
      , inquireVersions                        //
      , setReleaseVersion                      //
      , commitReleaseVersion                   //performs the initial git checks
      , tagRelease                             //
      , publishArtifacts.copy(action = publishSignedAction)
                                               //checks whether `publishTo` is properly set up
                                               //uses publish-signed instead of publish.
      , setNextVersion                         //
      , commitNextVersion                      //
      , pushChanges                            //also checks that an upstream branch is properly configured
    ),

    //Customize the next version string to bump the revision number.
    nextVersion := { ver => Version(ver).map(determineNextVersion(_)).getOrElse(versionFormatError) }
  )

  lazy val publishSignedAction = { st: State =>
    val extracted = st.extract
    val ref = extracted.get(thisProjectRef)
    extracted.runAggregated(publishSigned in Global in ref, st)
  }

  def determineNextVersion(version: Version): String = {
    version
      .bumpBugfix
      .asSnapshot
      .string
  }
}
