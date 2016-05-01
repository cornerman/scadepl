package scadepel.examples

import scadepl.Debug._

object Main extends App {
  println("started")

  val integers = List(1,2)
  val name = "a name"
  val num = 3

  log("num" -> num)
  break("integers" -> integers, "name" -> name)

  println("finished")
}
