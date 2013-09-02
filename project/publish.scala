import sbt._
import Keys._

object PublishSettings {
  val defaults = Seq(
    publishArtifact in Test := false,
    publishTo <<= version { version => Some(determinePublishTo(version)) }
  )

  def determinePublishTo(version: String): Resolver = {
    //http://www.scala-sbt.org/release/docs/Detailed-Topics/Publishing
    val nexus = "https://oss.sonatype.org/"
    if (version.trim.endsWith("SNAPSHOT"))
      "snapshots" at nexus + "content/repositories/snapshots"
    else
      "releases"  at nexus + "service/local/staging/deploy/maven2"
  }
}

