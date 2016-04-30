package scadepl

import scala.tools.nsc.Settings
import scala.reflect.runtime.universe.TypeTag

case class NamedValue[T](name: String, value: T)(implicit val typeTag: TypeTag[T])

object ReplConfig {
  lazy val settings = {
    val settings = new Settings
    if (startedFromSbt)
      settings.embeddedDefaults[DebugRepl.type]
    else
      settings.usejavacp.value = true

    settings
  }

  def startedFromSbt = {
    new Exception().getStackTrace.exists(_.toString.startsWith("sbt"))
  }
}

object DebugRepl {
  implicit def tupleToNamedValue[T](tuple: (String, T))(implicit typeTag: TypeTag[T]) = NamedValue(tuple._1, tuple._2)

  def breakIf(assertion: => Boolean, namedValues: NamedValue[_]*) {
    if (assertion) break(namedValues: _*)
  }

  def break(namedValues: NamedValue[_]*) {
    val repl = new DebugILoop(namedValues)
    repl process ReplConfig.settings
  }

  object Smart {
    def breakIf(assertion: => Boolean)(implicit line: sourcecode.Line, file: sourcecode.File) {
      if (assertion) break()
    }

    def break()(implicit line: sourcecode.Line, file: sourcecode.File) {
      println(s"BREAK AT: $file:$line")
    }
  }
}
