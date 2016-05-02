package scadepl

import scala.tools.nsc.interpreter.ILoop
import scala.reflect.runtime.universe.TypeTag

class ReplILoop(imports: Seq[String] = Seq.empty, namedValues: Seq[NamedValue[_]] = Seq.empty) extends ILoop {

  private def init() {
    echo("Binding scope:")
    imports.foreach(i => intp.interpret(s"import $i"))
    namedValues.foreach { v =>
      val tpe = v.typeTag match {
        case t: TypeTag[_] => t.tpe.toString
        case _             => v.value.getClass.getName // TODO: this is only a workaround for generics
      }

      intp.bind(v.name, tpe, v.value)
    }
    // intp.beQuietDuring {
    // }
  }

  override def printWelcome() = {
    super.printWelcome()
    echo("Debug repl started. Welcome!")

    processLine("")
    init()
  }

  override def commands = super.commands ++ debugCommands

  import LoopCommand.{cmd, nullary}

  lazy val debugCommands = List(
    nullary("ls", "show all defined parameters", DebugCommands.ls)
  )

  object DebugCommands {
    def ls() {
      namedValues.foreach { param =>
        val line = s"${param.name}: ${param.typeTag.tpe} = ${param.value}"
        println(line)
      }
    }
  }
}
