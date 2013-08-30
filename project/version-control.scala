import sbt._
import Keys._

object VersionControl {
  def currentCommit() =
    Process("git rev-parse HEAD").lines.head

  def shortenedCurrentCommit() =
    Process("git rev-parse --short HEAD").lines.head

  def branchName() =
    Process("git rev-parse --abbrev-ref HEAD").lines.head
}

