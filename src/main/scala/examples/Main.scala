import scadepl.DebugRepl._

object Main extends App {
  println("started")

  val integers = List(1,2)
  val name = "a name"
  break("integers" -> integers, "name" -> name)

  println("finished")
}
