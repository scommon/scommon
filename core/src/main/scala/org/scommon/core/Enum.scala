package org.scommon.core

/**
 * Courtesy [[https://github.com/dadrox/scala.enum/blob/master/src/main/scala/com/dadrox/ScalaEnum.scala]]
 * Information about this can be found at [[https://github.com/dadrox/scala.enum]]
 */
trait Enum {
  import java.util.concurrent.atomic.AtomicReference

  type EnumVal <: Value //This is a type that needs to be found in the implementing class

  def withName(name: String): Option[EnumVal] = values.find(_.name == name)
  def withNameIgnoringCase(name: String): Option[EnumVal] = values.find(_.name.equalsIgnoreCase(name))

  private def lazyName(requestingInstance: EnumVal): String = {
    getClass().getDeclaredFields().filter(field => classOf[Value].isAssignableFrom(field.getType())).find { f =>
      f.setAccessible(true)
      val value = f.get(this).asInstanceOf[EnumVal]
      value.ordinal == requestingInstance.ordinal
    } match {
      case Some(method) => method.getName
      case None         => throw new Error("Unknown enum value")
    }
  }

  private val _values = new AtomicReference(Vector[EnumVal]()) //Stores our enum values

  //Adds an EnumVal to our storage, uses CCAS to make sure it's thread safe, returns the ordinal
  private final def addEnumVal(newVal: EnumVal): Int = {
    import _values.{ get, compareAndSet => CAS }
    val oldVec = get
    val newVec = oldVec :+ newVal

    if ((get eq oldVec) && CAS(oldVec, newVec)) {
      newVec.indexWhere(_ eq newVal)
    } else {
      addEnumVal(newVal)
    }
  }

  //Here you can get all the enums that exist for this type
  def values: Vector[EnumVal] = _values.get

  //This is the class that we need to extend our EnumVal type with, it does the book-keeping for us
  protected trait Value extends java.io.Serializable {
    self: EnumVal => // Enforce that no one mixes in Value in a non-EnumVal type

    final val ordinal = addEnumVal(this) //Adds the EnumVal and returns the ordinal

    lazy val name: String = lazyName(this)

    override def toString: String = name

    override def equals(other: Any): Boolean = other match {
      case any: Value => this.name == any.name
      case _          => false
    }

    override def hashCode: Int = 31337 * (this.getClass.## + name.## + ordinal)
  }
}
