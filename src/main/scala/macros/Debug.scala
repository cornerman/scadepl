package scadepl.macros

import language.experimental.macros
import reflect.macros.blackbox.Context
import scadepl.NamedValue

object Debug {
  def _imports: List[String] = macro DebugImpl.imports
  def _locals: List[NamedValue[_]] = macro DebugImpl.locals
  def _args: List[NamedValue[_]] = macro DebugImpl.args
  def _members: List[NamedValue[_]] = macro DebugImpl.members
  def _thises: List[NamedValue[_]] = macro DebugImpl.thises
  def _all: List[NamedValue[_]] = macro DebugImpl.all
  def _idents(vals: Any*): List[NamedValue[_]] = macro DebugImpl.idents
}

// https://github.com/lihaoyi/sourcecode/blob/master/sourcecode/shared/src/main/scala/sourcecode/SourceContext.scala#L117
// val lineNum = pos.line
// val fileName = pos.source.path // This needs to be trimmed down
// val fullName = owner.fullName.trim
// val fileContent = new String(tree.pos.source.content)
// val start = tree.collect{case tree => tree.pos.startOrPoint}.min
// val g = c.asInstanceOf[reflect.macros.runtime.Context].global
// val parser = g.newUnitParser(fileContent.drop(start))
// parser.expr()
// val end = parser.in.lastOffset
// val txt = fileContent.slice(start, start + end)

// val newTree = c.parse(fileContent)

object DebugImpl {
  def symbolOwnerChain(c: Context)(sym: c.Symbol): List[c.Symbol] = {
    import c.universe._

    sym.owner match {
      case NoSymbol => sym :: Nil
      case owner => sym :: symbolOwnerChain(c)(owner)
    }
  }

  def expandEnclosingTree(c: Context)(tree: c.Tree): List[c.Tree] = {
    import c.universe._

    if (tree.exists(_.pos == c.enclosingPosition))
      tree :: tree.children.flatMap(t => expandEnclosingTree(c)(t))
    else
      List.empty
  }

  def localsInContext(c: Context): List[c.TermName] = {
    import c.universe._

    val methodTreeOpt = Option(c.enclosingMethod)
    methodTreeOpt.map { methodTree =>
      val contextTrees = expandEnclosingTree(c)(methodTree)
      val pos = c.enclosingPosition
      contextTrees.flatMap { tree =>
        tree.children.collect{
          case v@ValDef(_, name, _, rhs) if (v.pos precedes pos) && !v.symbol.isSynthetic && !rhs.exists(_.pos == pos) => name.toTermName
        }
      }
    }.getOrElse(List.empty)
  }

  def argsInContext(c: Context): List[c.TermName] = {
    import c.universe._

    val owner = c.internal.enclosingOwner
    val ownerChain = symbolOwnerChain(c)(owner)
    ownerChain.collect {
      case s: MethodSymbol => s.paramLists.flatMap(_.map(_.name.toTermName))
    }.flatten
  }

  def membersInContext(c: Context): List[c.TermName] = {
    import c.universe._

    val owner = c.internal.enclosingOwner
    val ownerChain = symbolOwnerChain(c)(owner)
    ownerChain.collect {
      case s: ClassSymbol => s.toType.decls.filter(m => m.isTerm && (m.asTerm.isVal || m.asTerm.isVar)).map(_.name.toTermName)
    }.flatten
  }

  def thisesInContext(c: Context): List[c.Tree] = {
    import c.universe._

    val owner = c.internal.enclosingOwner
    val ownerChain = symbolOwnerChain(c)(owner)
    ownerChain.collect {
      case s: ClassSymbol if !s.isPackage => This(s.toType.typeSymbol)
    }
  }

  def importsInContext(c: Context): List[String] = {
    import c.universe._

    val owner = c.internal.enclosingOwner
    val ownerChain = symbolOwnerChain(c)(owner)
    val pkgOpt = ownerChain.find(_.isPackage).filter(_.name != "<empty>").map(p => s"${p.fullName}._") //TODO sane check

    val pkgTree = c.enclosingPackage
    val contextTrees = expandEnclosingTree(c)(pkgTree)
    val pos = c.enclosingPosition
    val allImports = pkgOpt ++ contextTrees.flatMap { tree =>
      tree.children.collect{
        case i:Import if i.pos precedes pos => i.toString.replaceFirst("import ", "") //TODO: sane conversion
      }
    }

    allImports.toList
  }

  def namedValue(c: Context)(name: String, value: c.Tree): c.Tree = {
    import c.universe._
    q"scadepl.NamedValue($name, $value)"
  }

  def termToNamedValue(c: Context)(term: c.TermName): c.Tree = {
    import c.universe._
    val name = term.decodedName.toString
    namedValue(c)(name, q"$term")
  }

  def treeToNamedValue(c: Context)(tree: c.Tree): Option[c.Tree] = {
    import c.universe._

    tree match {
      case v@Select(_, term: TermName) => Some(termToNamedValue(c)(term))
      case v@Ident(term: TermName) => Some(termToNamedValue(c)(term))
      case v@Literal(Constant(_)) => Some(namedValue(c)(c.freshName(), v))
      case v@This(TypeName(_)) => Some(namedValue(c)(v.toString, v))
      case _ => None
    }
  }

  def imports(c: Context): c.Expr[List[String]] = {
    import c.universe._
    val imports = importsInContext(c).map(i => q"$i")
    c.Expr(q"List(..$imports)")
  }

  def locals(c: Context): c.Expr[List[NamedValue[_]]] = {
    import c.universe._
    val terms = localsInContext(c)
    val values = terms.map(t => termToNamedValue(c)(t))
    c.Expr(q"List(..$values)")
  }

  def args(c: Context): c.Expr[List[NamedValue[_]]] = {
    import c.universe._
    val terms = argsInContext(c)
    val values = terms.map(t => termToNamedValue(c)(t))
    c.Expr(q"List(..$values)")
  }

  def members(c: Context): c.Expr[List[NamedValue[_]]] = {
    import c.universe._
    val terms = membersInContext(c)
    val values = terms.map(t => termToNamedValue(c)(t))
    c.Expr(q"List(..$values)")
  }

  def thises(c: Context): c.Expr[List[NamedValue[_]]] = {
    import c.universe._
    val trees = thisesInContext(c)
    val values = trees.flatMap(t => treeToNamedValue(c)(t))
    c.Expr(q"List(..$values)")
  }

  def all(c: Context): c.Expr[List[NamedValue[_]]] = {
    import c.universe._
    val terms = localsInContext(c) ++ argsInContext(c) ++ membersInContext(c)
    val trees = thisesInContext(c)
    val values = terms.map(t => termToNamedValue(c)(t)) ++ trees.flatMap(t => treeToNamedValue(c)(t))
    c.Expr(q"List(..$values)")
  }

  def idents(c: Context)(vals: c.Tree*): c.Expr[List[NamedValue[_]]] = {
    import c.universe._
    val identVars = vals.flatMap(t => treeToNamedValue(c)(t))
    c.Expr(q"List(..$identVars)")
  }
}
