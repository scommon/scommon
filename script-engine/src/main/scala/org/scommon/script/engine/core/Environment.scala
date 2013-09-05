package org.scommon.script.engine.core

import scala.collection._
import java.net.URLClassLoader
import java.io.File

/**
 * Supplies valuable utilities for the runtime environment.
 */
object Environment {
  def determineFullUserClassPath(current_loader: ClassLoader = Thread.currentThread().getContextClassLoader): Iterable[String] = {
    var loader = current_loader
    val cp = mutable.ArrayBuffer[String]()

    while(Option(loader).isDefined) {
      loader match {
        case u: URLClassLoader =>
          for {
            url <- u.getURLs
            if url.getProtocol.equalsIgnoreCase("file")
            class_path_element = new File(url.toURI).getAbsolutePath
            if !cp.contains(class_path_element)
          }
            cp += class_path_element
        case _ =>
      }
      loader = loader.getParent
    }

    for {
      not_split_class_path       <- Option(System.getProperty("java.class.path"))
      class_path_element         <- not_split_class_path.split(File.pathSeparator)
      class_path_element_as_file  = new File(class_path_element)
      class_path_as_string        = class_path_element_as_file.getAbsolutePath
      if !cp.contains(class_path_as_string)
    }
      cp += class_path_as_string

    cp
  }
}
