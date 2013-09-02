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
      val project_name = Project.extract(state).currentRef.project
      val project_title = Settings.project + "-" + project_name
      "%s%s%s> ".format(
        project_title, currBranch, currCommit
      )
    }
  }
}

