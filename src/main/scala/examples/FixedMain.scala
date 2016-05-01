import scadepl.{Debugger, DebugSettings}

object FixedDebug extends App {
  Debugger.launch(FixedMain.getClass,
    DebugSettings(breakpoints = Seq("FixedMain.scala" -> 13)))
}

object FixedMain extends App {
  println("started")

  val foo = 2

  println(s"finished: $foo")
}
