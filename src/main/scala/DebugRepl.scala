package debugrepl

import scala.tools.nsc.interpreter.{ILoop, NamedParam}
import scala.tools.nsc.Settings

object DebugRepl {
  class DebugILoop(params: Seq[NamedParam]) extends ILoop {

    override def printWelcome() = {
      super.printWelcome()
      echo("Debug repl started. Welcome!")

      processLine("")
      echo("Binding scope:")
      params.foreach(intp.bind)

      // intp.beQuietDuring {
      // }
    }

    override def commands = super.commands ++ debugCommands

    import LoopCommand.{ cmd, nullary }

    lazy val debugCommands = List(
        nullary("ls", "show all defined parameters", DebugCommands.ls)
      )

    object DebugCommands {
      def ls() {
        params.foreach { param =>
          val line = s"${param.name}: ${param.tpe} = ${param.value}"
          println(line)
        }
      }
    }
  }

  def breakIf(assertion: => Boolean, params: NamedParam*) {
    if (assertion) break(params: _*)
  }

  def break(params: NamedParam*) {
    val repl = new DebugILoop(params)
    repl process settings
  }

  def settings = {
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
