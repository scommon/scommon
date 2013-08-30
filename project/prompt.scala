import sbt._
import Keys._

object GitPrompt {
  def currBranch = {
    val branch = VersionControl.branchName()
    if ("" == branch)
      ":master"
    else
      ":" + branch
  }

  def currCommit = {
    val commit = VersionControl.shortenedCurrentCommit()
    if ("" == commit)
      "@unknown"
    else
      "@" + commit
  }

  val build = {
    (state: State) => {
      val currProject = Project.extract(state).currentRef.project
      "%s%s%s> ".format(
        currProject, currBranch, currCommit
      )
    }
  }
}

