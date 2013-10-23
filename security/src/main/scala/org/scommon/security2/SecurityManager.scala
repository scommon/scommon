package org.scommon.security2

import java.lang.{SecurityManager => JSecurityManager}
import java.security.{Permission => JPermission, AccessControlException}
import java.util.concurrent.locks.ReentrantLock
import java.util.UUID

object SecurityManager2 extends JSecurityManager {
  type UnrestrictedAccess = SecurityManager.type => Unit
  private[this] type RestrictedAccess = Context => Unit
  private[this] type Delegated = JSecurityManager => Unit

  private[this] val PERMANENT_WHITELIST = Seq(
    "java.lang"
  )

  private[this] val install_lock = new ReentrantLock()

  private class InheritableContext(val context: Context = null, val parent: Option[InheritableContext] = None) extends InheritableThreadLocal[InheritableContext] {
    override def initialValue() = new InheritableContext()

    override protected def childValue(parentValue: InheritableContext) =
      new InheritableContext(parentValue.context, Some(parentValue))
    def hasParent =
      parent.isDefined
    override def toString =
      "InheritableContext(" + context + ", " + parent + ")"
  }

  //Explicitly NOT inheritable -- other threads viewing this security manager should have
  //restricted access by default.
  private[this] val thread_local_unrestricted = new ThreadLocal[Boolean]()

  private[this] val thread_local_checking = new ThreadLocal[Boolean]()

  //Inherit certain values for new threads.
  private[this] val thread_local_lock = new InheritableThreadLocal[ReentrantLock]()
  private[this] val thread_local_password = new InheritableThreadLocal[Array[Byte]]()
  private[this] val thread_local_context = new InheritableContext()
  private[this] val thread_local_delegate_to = new InheritableThreadLocal[Option[JSecurityManager]]()

  private[this] val thread_local_disabled = new InheritableThreadLocal[Boolean] {
    override def childValue(parentValue: Boolean) =
      super.childValue(parentValue)
  }

  //Create a thread group that we can use to catch uncaught exceptions.
  private[this] val thread_group = new ThreadGroup("sandbox") {
    override def uncaughtException(t: Thread, e: Throwable) = {
      val local = thread_local_context.get()
      if (local ne null)
        local.context.handleUncaughtException(t, e)
    }
  }

  thread_local_lock.set(new ReentrantLock())
  thread_local_password.set(UUID.randomUUID.toString.getBytes)
  thread_local_checking.set(false)
  thread_local_disabled.set(false)
  thread_local_unrestricted.set(false)

  def install(): Unit = {
    install_lock.lock()
    try {
      System.getSecurityManager() match {
        case sm: SecurityManager =>
          //Do nothing if we're installing the same instance.
        case sm: JSecurityManager if sm ne this =>
          //Error if another security manager is installed.
          throw new AccessControlException(s"Unable to install security manager. Another security manager of type ${sm.getClass.getName} is already installed.")
        case _ =>
          //Go ahead and install it.
          try {
            //Ensure security-related classes are loaded.
            checkPermission(new RuntimePermission("createSecurityManager"))
            System.setSecurityManager(this)
          } catch {
            case t: Throwable =>
              throw new RuntimeException("Unable to install security manager.", t)
          }
      }
    } finally {
      install_lock.unlock()
    }
  }

  def isChecking: Boolean =
    thread_local_checking.get()

  def isEnabled: Boolean =
    !isDisabled

  def isDisabled: Boolean =
    thread_local_disabled.get()

  def isRestricted: Boolean =
    !isUnrestricted

  def isUnrestricted: Boolean =
    thread_local_unrestricted.get()

  def unrestricted(password: String)(callback: UnrestrictedAccess): Unit =
    unrestricted(password.getBytes)(callback)

  def unrestricted(password: Array[Byte])(callback: UnrestrictedAccess): Unit = {
    if (java.util.Arrays.equals(password, thread_local_password.get())) {
      val lock = thread_local_lock.get()

      //Intentionally serialize unrestricted  operations.
      lock.lock()
      thread_local_unrestricted.set(true)
      try {
        callback(this)
      } finally {
        thread_local_unrestricted.set(false)
        lock.unlock()
      }
    } else {
      throw new AccessControlException(s"Invalid password when attempting to perform an unrestricted operation on an instance of ${classOf[SecurityManager].getName}")
    }
  }

  private[this] def restricted(precheck: => Boolean)(callback: RestrictedAccess) = {
    if (isRestricted && isEnabled && !isChecking) {
      if (precheck) {
        check()
        try {
          val local = thread_local_context.get()
          if ((local ne null) && (local.context ne null))
            callback(local.context)
        }
        finally uncheck()
      }
    }
  }

  private[this] def delegate(callback: Delegated) = {
    val delegate = thread_local_delegate_to.get()
    if (delegate != null)
      delegate.map(callback)
  }

  def check(): Unit =
    thread_local_checking.set(true)

  def uncheck(): Unit =
    thread_local_checking.set(false)

  def disable(): Unit =
    thread_local_disabled.set(true)

  def enable(): Unit =
    thread_local_disabled.set(false)

  def hasContext: Boolean =
    thread_local_context.get() ne null

  def disabled(callback: => Unit): Unit = {
    val foo = SecurityManagerData.FOO.get()
    println(s"${Thread.currentThread().getName} ${thread_local_context.get()}")
    val old_context = thread_local_context.get()
    if ((old_context ne null) && old_context.hasParent)
      throw new AccessControlException("Cannot modify the context if one is already set. Are you attempting to run a disabled or insecure run within a secure run?")
    val was_disabled = thread_local_disabled.get()
    try {
      thread_local_disabled.set(true)
      callback
    } finally {
      thread_local_disabled.set(was_disabled)
    }
  }

  def enabled(callback: => Unit)(implicit context: Context): Unit = {
    SecurityManagerData.FOO.set(true)

    val old_context = thread_local_context.get()
    if ((old_context ne null) && old_context.hasParent)
      throw new AccessControlException("Cannot modify the context if one is already set.")
    val was_disabled = thread_local_disabled.get()
    try {
      thread_local_context.set(new InheritableContext(context))
      thread_local_disabled.set(false)
      println(s"${Thread.currentThread().getName} ${thread_local_context.get()}")
      callback
    } finally {
      thread_local_context.remove()
      thread_local_disabled.set(was_disabled)
    }
  }

  override def checkPermission(perm: JPermission, context: AnyRef): Unit = {
    if (isRestricted && isEnabled)
      throw new AccessControlException(s"Unable to comply")
    delegate { other =>
      other.checkPermission(perm, context)
    }
  }

  override def checkPackageDefinition(pkg: String): Unit = {
    if (isRestricted && isEnabled)
      throw new AccessControlException(s"Package definition denied: $pkg")
    delegate { other =>
      other.checkPackageDefinition(pkg)
    }
  }

  override def checkPermission(perm: JPermission): Unit = {
    restricted(true) { context =>
      if (!context.checkPermission(perm))
        throw new AccessControlException(s"Permission denied: $perm", perm)
    }
    delegate { other =>
      other.checkPermission(perm)
    }
  }

  override def checkPackageAccess(pkg: String): Unit = {
    restricted(!PERMANENT_WHITELIST.exists(_ == pkg)) { context =>
      if (!context.checkPackageAccess(pkg))
        throw new AccessControlException(s"Package access denied: $pkg")
    }
    delegate { other =>
      other.checkPackageAccess(pkg)
    }
  }
}

sealed class SecurityManager(parentSecurityManager: Option[JSecurityManager] = None) extends JSecurityManager {
  import SecurityManager._

}
