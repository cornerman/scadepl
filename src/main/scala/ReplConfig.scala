package scadepl

import scala.tools.nsc.Settings

object ReplConfig {
  lazy val settings = {
    val settings = new Settings
    if (startedFromSbt)
      settings.embeddedDefaults[ReplConfig.type]
    else
      settings.usejavacp.value = true

    settings
  }

  def startedFromSbt = {
    new Exception().getStackTrace.exists(_.toString.startsWith("sbt"))
  }
}
