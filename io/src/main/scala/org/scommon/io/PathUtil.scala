package org.scommon.io

import java.io.File
import java.nio.file.Paths
import scala.collection._

import scala.language.implicitConversions

object PathUtil {
  //Holds cached versions of the queries. It's better to call the queryXXX versions if you
  //anticipate the values changing over the life time of your running application.
  val tempDirectory = querySystemTempDirectory
  val userTempDirectory = querySystemUserTempDirectory
  val userHomeDirectory = queryUserHomeDirectory
  val fileSeparator = queryFileSeparator
  val pathSeparator = queryPathSeparator
  val applicationDirectory = queryApplicationDirectory

  @inline def queryFileSeparator: String = File.separator
  @inline def queryPathSeparator: String = File.pathSeparator
  @inline def queryUserHomeDirectory: String = System.getProperty("user.home")

  @inline def queryApplicationDirectory: String = System.getProperty("user.dir")
  @inline def queryWorkingDirectory: String = Paths.get("").toAbsolutePath.toString

  /** Requests the system's default temporary directory. **/
  @inline def querySystemTempDirectory: String = System.getProperty("java.io.tmpdir")
  @inline def querySystemUserTempDirectory: String = System.getProperty("java.io.tmpdir")

  @inline def querySystemPath: String = System.getenv("PATH")

  @inline def toTemp(f: File): File = new File(tempDirectory, f.getPath)
  @inline def toUserTemp(f: File): File = new File(userTempDirectory, f.getPath)

  def deleteAll(parent: File): Boolean = {
    val s = mutable.Stack[File]()
    val q = mutable.Queue[File](parent)

    while(!q.isEmpty) {
      val d = q.dequeue()
      val children = d.listFiles()
      s.push(d)

      if (children != null && !children.isEmpty) {
        for (f <- children) {
          if (f.isDirectory)
            q.enqueue(f)
          else
            if (!f.delete())
              return false
        }
      }
    }

    for(f <- s)
      if (!f.delete())
        return false

    true
  }
}
