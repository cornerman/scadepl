package scadepl

import reflect.runtime.universe.WeakTypeTag
import macros.DebugImpl
import repl.ReplILoop

object Debug {
  implicit def tupleToNamedValue[T : WeakTypeTag](tuple: (String, T)): TypeNamedValue[T] = NamedValue(tuple._1, tuple._2)
  implicit def tupleTupleToNamedValue[T : WeakTypeTag](tuple: ((String, Any), String)): LiteralNamedValue = NamedValue(tuple._1._1, tuple._1._2, tuple._2)

  def repl[T](imports: Seq[String], namedValues: NamedValue*): Option[T] = {
    val repl = new ReplILoop(imports, namedValues)
    repl.process(Config.replSettings)
    repl.lastResult.map(_.asInstanceOf[T])
  }

  def repl[T](namedValues: NamedValue*): Option[T] = {
    repl(Seq.empty, namedValues: _*)
  }

  def log(namedValues: NamedValue*) {
    Config.logStream.println(namedValues.mkString("\n"))
  }

  def _imports: List[String] = macro DebugImpl.imports
  def _locals: List[NamedValue] = macro DebugImpl.locals
  def _args: List[NamedValue] = macro DebugImpl.args
  def _members: List[NamedValue] = macro DebugImpl.members
  def _thises: List[NamedValue] = macro DebugImpl.thises
  def _all: List[NamedValue] = macro DebugImpl.all
  def _idents(vals: Any*): List[NamedValue] = macro DebugImpl.idents
}
