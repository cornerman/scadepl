package scadepl.macros

import reflect.macros.blackbox.Context
import scadepl.NamedValue

class ContextTools[C <: Context](val context: C) {
  import context.universe._

  def symbolOwnerChain(sym: context.Symbol): List[context.Symbol] = {
    sym.owner match {
      case NoSymbol => sym :: Nil
      case owner => sym :: symbolOwnerChain(owner)
    }
  }

  def expandEnclosingTree(tree: context.Tree): List[context.Tree] = {
    if (tree.exists(_.pos == context.enclosingPosition))
      tree :: tree.children.flatMap(expandEnclosingTree)
    else
      Nil
  }

  def localsInContext: List[context.TermName] = {
    val methodTreeOpt = Option(context.enclosingMethod)
    methodTreeOpt.map { methodTree =>
      val contextTrees = expandEnclosingTree(methodTree)
      val pos = context.enclosingPosition
      contextTrees.flatMap(_.children.collect{
        case v@ValDef(_, name, _, rhs) if (v.pos precedes pos) && !v.symbol.isSynthetic && !rhs.exists(_.pos == pos) => name.toTermName
      })
    }.getOrElse(List.empty)
  }

  def argsInContext: List[context.TermName] = {
    val owner = context.internal.enclosingOwner
    val ownerChain = symbolOwnerChain(owner)
    ownerChain.collect {
      case s: MethodSymbol => s.paramLists.flatMap(_.map(_.name.toTermName))
    }.flatten
  }

  def membersInContext: List[context.TermName] = {
    val owner = context.internal.enclosingOwner
    val ownerChain = symbolOwnerChain(owner)
    ownerChain.collect {
      case s: ClassSymbol => s.toType.decls.filter(m => m.isTerm && (m.asTerm.isVal || m.asTerm.isVar)).map(_.name.toTermName)
    }.flatten
  }

  def thisesInContext: List[context.Tree] = {
    import context.universe._

    val owner = context.internal.enclosingOwner
    val ownerChain = symbolOwnerChain(owner)
    ownerChain.collect {
      case s: ClassSymbol if !s.isPackage => This(s.toType.typeSymbol)
    }
  }

  def importsInContext: List[context.Tree] = {
    val owner = context.internal.enclosingOwner
    val ownerChain = symbolOwnerChain(owner)
    val pkgOpt = ownerChain.find(_.isPackage).filterNot(_.name.toString.startsWith("<empty>")).map(p => s"${p.fullName}._") //TODO sane check

    val pkgTree = context.enclosingPackage
    val contextTrees = expandEnclosingTree(pkgTree)
    val pos = context.enclosingPosition
    val allImports = pkgOpt ++ contextTrees.flatMap { tree =>
      tree.children.collect{
        case i:Import if i.pos precedes pos => i.toString.replaceFirst("import ", "") //TODO: sane conversion
      }
    }

    allImports.map(i => q"$i").toList
  }

  def mkList[I,R](values: List[I])(implicit lift: Liftable[I]): context.Expr[List[R]] = context.Expr(q"List(..$values)")

  def namedValue(name: String, value: context.Tree): context.Tree = {
    q"scadepl.NamedValue($name, $value)"
  }

  def termToNamedValue(term: context.TermName): context.Tree = {
    val name = term.decodedName.toString
    namedValue(name, q"$term")
  }

  def treeToNamedValue(tree: context.Tree): context.Tree = {
    tree match {
      case v@Select(_, term: TermName) => termToNamedValue(term)
      case v@Ident(term: TermName) => termToNamedValue(term)
      case v@This(TypeName(_)) => namedValue(v.toString, v)
      case v =>
        val estimateName = v.toString.split(" ", 2).head //TODO: better
        namedValue(context.freshName(estimateName), v)
    }
  }
}

object ContextTools {
  def apply(c: Context): ContextTools[c.type] = new ContextTools(c)
}
