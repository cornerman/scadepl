package scadepl

import reflect.runtime.universe._

trait NamedValue {
  val name: String
  val value: Any
  def typeName: String

  def strippedName = {
    if (name == "this") "_this"
    else name.replaceAll("^[0-9]|[^0-9a-zA-Z_$]", "_")
  }

  override def toString = s"$name: $typeName = $value"
}

object NamedValue {
  def apply[T : WeakTypeTag](name: String, value: T) = TypeNamedValue(name, value)
  def apply(name: String, value: Any, typeName: String) = LiteralNamedValue(name, value, typeName)
}

case class TypeNamedValue[T : WeakTypeTag](name: String, value: T) extends NamedValue {
  val typeTag = weakTypeTag[T]

  def isGenericType = typeTag.tpe.typeArgs.nonEmpty //typeTag.tpe.takesTypeArgs
  def isFreeType = !typeTag.tpe.typeSymbol.isAbstract && !typeTag.tpe.typeSymbol.isStatic

  def typeName = typeTag match {
    case t:TypeTag[_] => t.tpe.toString
    case t =>
      if (isFreeType) {
        if (value == null) typeOf[Null].toString else value.getClass.getName
      } else if (value != null && isGenericType) {
        value.getClass.getName
      } else {
        typeTag.tpe.erasure.toString
      }
  }
}

case class LiteralNamedValue(name: String, value: Any, typeName: String) extends NamedValue
