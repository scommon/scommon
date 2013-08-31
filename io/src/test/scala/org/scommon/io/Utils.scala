package org.scommon.io

import java.io.File

object Utils {
  object Path {
    def apply(path:String):File =
      new File(path)

    def apply(parent:String, path:String):File =
      new File(parent, path)

    def apply(parent:File, path:String):File =
      new File(parent, path)
  }
}
