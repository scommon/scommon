package org.scommon.io

import java.io.File
import scala.collection._

import scala.language.implicitConversions

object PathUtil {
  val tempDirectory = querySystemTempDirectory
  val userTempDirectory = querySystemUserTempDirectory

  /** Requests the system's default temporary directory. **/
  @inline def querySystemTempDirectory: String = System.getProperty("java.io.tmpdir")
  @inline def querySystemUserTempDirectory: String = System.getProperty("java.io.tmpdir")

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
