package org.scommon.script.engine

import java.io.File
import scala.{io => si}
import java.util.Locale

object Utils {
  object Path {
    def apply(path:String):File =
      new File(path)

    def apply(parent:String, path:String):File =
      new File(parent, path)

    def apply(parent:File, path:String):File =
      new File(parent, path)
  }

  object EmbeddedResource {
    def apply(resource: String): String =
      withResource(resource)(x => x)

    def withResource[A](resource: String)(res: String => A) = {
      var source: si.BufferedSource = null
      try {
        source = si.Source.fromURL(getClass.getResource(resource))
        res(source.getLines() mkString "\n")
      } finally {
        if (source ne null)
          source.close()
      }
    }
  }

  object EmbeddedResources {
    def apply(resources: String*): Iterable[String] =
      for(res <- resources)
        yield EmbeddedResource(res)

    def apply(range: Range)(fmt: String): Iterable[String] =
      for(r <- range)
        yield {
        println(String.format(fmt, r: java.lang.Integer))
        EmbeddedResource(String.format(fmt, r: java.lang.Integer))
      }
  }
}
