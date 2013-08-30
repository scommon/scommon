package org.scommon.io

import java.io.{File, PrintWriter, InputStream, OutputStream, FileInputStream, FileOutputStream}

import org.scommon.core.{Closeable}

object FileUtil {
  @inline def touch(f:File, time:Long = System.currentTimeMillis()): Boolean = {
    if (f.isDirectory) {
      if (!f.exists())
        f.mkdirs()
      f.setLastModified(time)
    } else {
      if (!f.exists()) {
        f.getParentFile.mkdirs()
        new PrintWriter(f).close()
      }
      f.setLastModified(time)
    }
  }

  @inline def openForRead(f:File):InputStream with Closeable =
    InputStreamUtil.toCloseable(new FileInputStream(f))

  @inline def openForWrite(f:File, append:Boolean = false, createIfDoesntExist:Boolean = true):OutputStream with Closeable = {
    if (createIfDoesntExist) {
      val parent_directory = f.getParentFile
      if (!parent_directory.exists())
        parent_directory.mkdirs()
    }
    OutputStreamUtil.toCloseable(new FileOutputStream(f, append))
  }

  object File {
    def apply(parent:File, path:String):File =
      new File(parent, path)
  }
}
