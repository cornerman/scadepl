package scadepel.examples

import scadepl.macros.Debug._

object MacroMain extends App {
  println("started")

  val integers = List(1,2)
  val name = "a name"

  trait IBar {
    val inherited = "yep"
  }

  object Bar extends IBar {
    val field =  List(1)
    def func(arg: Int) {
      val local = 0
      if (arg > local) break()
    }
  }

  Bar.func(2)

  println("finished")
}
