package scadepl.macros

import reflect.macros.blackbox.Context
import scadepl.NamedValue

object DebugImpl {
  def imports(c: Context): c.Expr[List[String]] = {
    val tools = ContextTools(c)
    import tools._
    mkList(importsInContext)
  }

  def locals(c: Context): c.Expr[List[NamedValue]] = {
    val tools = ContextTools(c)
    import tools._
    val values = localsInContext.map(termToNamedValue)
    mkList(values)
  }

  def args(c: Context): c.Expr[List[NamedValue]] = {
    val tools = ContextTools(c)
    import tools._
    val values = argsInContext.map(termToNamedValue)
    mkList(values)
  }

  def members(c: Context): c.Expr[List[NamedValue]] = {
    val tools = ContextTools(c)
    import tools._
    val values = membersInContext.map(termToNamedValue)
    mkList(values)
  }

  def thises(c: Context): c.Expr[List[NamedValue]] = {
    val tools = ContextTools(c)
    import tools._
    val values = thisesInContext.map(treeToNamedValue)
    mkList(values)
  }

  def all(c: Context): c.Expr[List[NamedValue]] = {
    val tools = ContextTools(c)
    import tools._
    val terms = localsInContext ++ argsInContext ++ membersInContext
    val trees = thisesInContext
    val values = terms.map(termToNamedValue) ++ trees.map(treeToNamedValue)
    mkList(values)
  }

  def idents(c: Context)(vals: c.Tree*): c.Expr[List[NamedValue]] = {
    val tools = ContextTools(c)
    import tools._
    val identVars = vals.map(treeToNamedValue).toList
    mkList(identVars)
  }
}
