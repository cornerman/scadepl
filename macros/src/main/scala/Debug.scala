package scadepl.macros

import language.experimental.macros
import reflect.macros.blackbox.Context

object Debug {
  def break(vals: Any*): Unit = macro DebugImpl.break
  def log(vals: Any*): Unit = macro DebugImpl.log
}

object DebugImpl {
  def namedValuesFromTrees(c: Context)(vals: Seq[c.Tree]): List[c.Tree] = {
    import c.universe._
    vals.collect {
      case v@Select(_, TermName(name)) => q"scadepl.NamedValue($name, $v)"
      case v@Ident(TermName(name)) => q"scadepl.NamedValue($name, $v)"
      case v@Literal(Constant(_)) => q"scadepl.NamedValue(${c.freshName()}, $v)"
    }.toList
  }

  def break(c: Context)(vals: c.Tree*): c.Tree = {
    import c.universe._
    val namedValues = namedValuesFromTrees(c)(vals)
    q"scadepl.Debug.break(..$namedValues)"
  }

  def log(c: Context)(vals: c.Tree*): c.Tree = {
    import c.universe._
    val namedValues = namedValuesFromTrees(c)(vals)
    q"scadepl.Debug.log(..$namedValues)"
  }
}
