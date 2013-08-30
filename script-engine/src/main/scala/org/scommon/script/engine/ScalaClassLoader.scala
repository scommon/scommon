package org.scommon.script.engine

import java.io.File
import java.net.URLClassLoader
import java.security.AccessControlException
import scala.reflect.ClassTag

/**
 * a throwaway classloader that keeps one version of the source code. For every code change/refresh,
 * a new instance of this classloader is used.
 *
 * @author kostantinos.kougios
 *
 *         22 Dec 2011
 */
class ScalaClassLoader(
	sourceDirs: Set[File],
	classPath: Set[File],
	parentClassLoader: ClassLoader,
	config: ClassLoaderConfig) extends URLClassLoader(
	(classPath ++ sourceDirs).toArray.map(_.toURI.toURL),
	parentClassLoader)
{
	private val allClasses = if (config.enableClassRegistry) {
		new ClassRegistry(sourceDirs).allClasses.map(loadClass _)
	} else Nil

	/**
	 * @return      all classes, if ClassLoaderConfig.enableClassRegistry=true , throws an exception otherwise
	 */
	def all = if (config.enableClassRegistry) allClasses else throw new IllegalStateException("ClassLoaderConfig.enableClassRegistry not true, class registry not enabled")

	def withTypeOf[T](implicit ct: ClassTag[T]) = all.filter(ct.runtimeClass.isAssignableFrom _)

	def get[T](className: String): Class[T] = loadClass(className).asInstanceOf[Class[T]]

	def newInstance[T](className: String): T = get[T](className).newInstance

	override def loadClass(name: String) = {

		def accessForbidden() = throw new AccessControlException("access to class " + name + " not allowed")

		if (!config.protectPackages.isEmpty) {
			config.protectPackagesSuffixed.find(name.startsWith(_)).foreach {
				_ =>
					accessForbidden()
			}
		}
		if (!config.protectClasses.isEmpty) {
			config.protectClasses.find(_ == name).foreach {
				_ =>
					accessForbidden()
			}
		}
		val pckg = name.lastIndexOf('.') match {
			case -1 => ""
			case n => name.substring(0, n)
		}

		if (!config.allowed(pckg, name))
			accessForbidden()

		val clz = super.loadClass(name)
		config.classLoadingListeners.foreach {
			cll =>
				cll(name, clz)
		}
		clz
	}
}
