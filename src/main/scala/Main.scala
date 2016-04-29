package debugrepl

import scala.tools.nsc.interpreter.NamedParam

object Main extends App {
  def test(foo: Int) = {
    DebugRepl.breakIf(foo > 1, NamedParam("foo", foo))
  }

  test(1)
  test(2)
  println("finished")
}
