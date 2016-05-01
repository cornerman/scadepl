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
    def func(arg: Int): String = {
      log(arg)
      val local = 1

      // if (arg < local) {
      //   val unseeBlock = "one"
      //   unseeBlock
      // } else {
        val seeBlock = "two"
        break(arg, seeBlock, field, inherited)
        seeBlock
      // }
    }
  }

  println(Bar.func(2))

  println("finished")
}
