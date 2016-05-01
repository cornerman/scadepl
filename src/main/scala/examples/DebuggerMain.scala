package scadepel.examples

import scadepl.{Debugger, Debuggee}

object DebuggerMain extends App {
  Debugger.launch(DebuggeeMain.getClass)
}

object DebuggeeMain extends App {
  import Debuggee._

  trait IBar {
    val inherited = "top"
  }

  class Bar extends IBar {
    val integers = List(1,2)
    def func = true

    def test(me: Option[Int]) = {
      val number = 0
      if (me.filter(_ > number).isEmpty) break()
    }
  }

  println("started")

  val bar = new Bar
  bar.test(Some(1))
  bar.test(None)

  println("finished")
}
