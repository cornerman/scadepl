package scadepl

import tools.nsc.Settings
import java.io.PrintStream

object Config {
  val replSettings = {
    val settings = new Settings
    if (startedFromSbt)
      settings.embeddedDefaults[Config.type]
    else
      settings.usejavacp.value = true

    settings
  }

  var logStream: PrintStream = System.out

  def startedFromSbt = {
    new Exception().getStackTrace.exists(_.toString.startsWith("sbt"))
  }
}
