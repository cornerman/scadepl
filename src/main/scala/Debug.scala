package scadepl

import reflect.runtime.universe.WeakTypeTag

case class NamedValue[T](name: String, value: T)(implicit val typeTag: WeakTypeTag[T]) {
  def strippedName = {
    //TODO what is allowed...
    if (name == "this") "_this" else name.replace(".", "_")
  }
}

object Debug {
  implicit def tupleToNamedValue[T](tuple: (String, T))(implicit typeTag: WeakTypeTag[T]): NamedValue[T] = NamedValue(tuple._1, tuple._2)

  def break[T](imports: Seq[String], namedValues: NamedValue[_]*): Option[T] = {
    val repl = new ReplILoop(imports, namedValues)
    repl.process(ReplConfig.settings)
    repl.lastResult.map(_.asInstanceOf[T])
  }

  def break[T](namedValues: NamedValue[_]*): Option[T] = {
    break(Seq.empty, namedValues: _*)
  }

  def log(namedValues: NamedValue[_]*) {
    val statements = namedValues.map { v =>
      s"${v.name}: ${v.typeTag.tpe} = ${v.value}"
    }

    println(statements.mkString("\n"))
  }
}
