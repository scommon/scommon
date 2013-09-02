import sbt._
import Keys._

object PublishSettings {
  val defaults = Seq(
    publishTo <<= version { version => Some(determinePublishTo(version)) }
  )

  def determinePublishTo(version: String): Resolver = {
    //http://www.scala-sbt.org/release/docs/Detailed-Topics/Publishing
    //val nexus = "https://oss.sonatype.org/"
    //if (version.trim.endsWith("SNAPSHOT"))
    //  "snapshots" at nexus + "content/repositories/snapshots"
    //else
    //  "releases"  at nexus + "service/local/staging/deploy/maven2"
    //
    Resolver.file("file", new File(Path.userHome.getAbsolutePath + "/.m2/repository"))
  }
}

