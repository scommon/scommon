package org.scommon.script.engine

import java.io.{FileInputStream, File}

/**
 * finds all class names for a list of directories
 *
 * @author kostas.kougios
 *          Date: 21/08/13
 */
class ClassRegistry(dirs: Set[File])
{
	val allClasses = {
		val classFiles = find(dirs.toList)
		val classLoader = new ClassLoader()
		{
			def scan = classFiles.map {
				f =>
					val is = new FileInputStream(f)
					try {
						val cnt = is.available
						val bytes = Array.ofDim[Byte](cnt)
						is.read(bytes)
						defineClass(null, bytes, 0, bytes.length)
					} finally {
						is.close()
					}
			}
		}
		classLoader.scan
	}.map(_.getName)

	// find all class files
	private def find(dirs: List[File]): List[File] = dirs.map {
		dir =>
			val files = dir.listFiles.toList
			val subDirs = find(files.filter(_.isDirectory))
			files.filter(_.getName.endsWith(".class")) ::: subDirs
	}.flatten
}
