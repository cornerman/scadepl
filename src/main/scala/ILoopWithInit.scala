package scadepl

import scala.tools.nsc.interpreter.{ILoop, IMain}

class ILoopWithInit extends ILoop {
  //TODO: why do i need this? otherwise intp is null after process
  private var _intp: IMain = _
  def intpOpt = Option(_intp)

  protected def init() {}

  override def printWelcome() = {
    super.printWelcome()

    processLine("") // wait for repl to be ready
    _intp = intp
    init()
  }
}
