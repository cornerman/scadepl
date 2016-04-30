import scadepl.{DebugRepl, Debugger}

object Main extends App {
  import DebugRepl._

  println("started")

  val integers = List(1,2)
  val name = "a name"
  break("integers" -> integers, "name" -> name)

  println("finished")
}

object SmartDebug extends App {
  Debugger.launch(SmartMain.getClass)
}

object SmartMain extends App {
  import DebugRepl.Smart._

  trait IBar {
    val inherited = "top"
  }

  class Bar extends IBar {
    val integers = List(1,2)
    def func = true

    def test(me: Option[Int]) = {
      val number = 0
      breakIf(me.filter(_ > number).isEmpty)
    }
  }

  println("started")

  val bar = new Bar
  bar.test(Some(1))
  Thread.sleep(1000)
  bar.test(None)

  println("finished")
}
