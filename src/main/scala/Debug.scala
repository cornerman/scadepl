package scadepl

import scala.reflect.runtime.universe.WeakTypeTag

case class NamedValue[T](name: String, value: T)(implicit val typeTag: WeakTypeTag[T])

object Debug {
  implicit def tupleToNamedValue[T](tuple: (String, T))(implicit typeTag: WeakTypeTag[T]): NamedValue[T] = NamedValue(tuple._1, tuple._2)

  def break(imports: Seq[String], namedValues: NamedValue[_]*) {
    val repl = new ReplILoop(imports, namedValues)
    repl process ReplConfig.settings
  }

  def break(namedValues: NamedValue[_]*) {
    break(Seq.empty, namedValues: _*)
  }

  def log(namedValues: NamedValue[_]*) {
    val statements = namedValues.map { v =>
      s"${v.name}: ${v.typeTag.tpe} = ${v.value}"
    }
    println(statements.mkString("\n"))
  }
}
