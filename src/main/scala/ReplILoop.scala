package scadepl

import scala.tools.nsc.interpreter.ILoop

class ReplILoop(namedValues: Seq[NamedValue[_]] = Seq.empty, imports: Seq[String] = Seq.empty) extends ILoop {

  private def init() {
    echo("Binding scope:")
    namedValues.foreach(v => intp.bind(v.name, v.typeTag.tpe.toString, v.value))
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
