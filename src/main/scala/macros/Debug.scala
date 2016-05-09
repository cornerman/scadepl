package scadepl.macros

import language.experimental.macros
import reflect.macros.blackbox.Context

object Debug {
  def break(vals: Any*): Unit = macro DebugImpl.break //TODO: version with imports
  def log(vals: Any*): Unit = macro DebugImpl.log

  object Smart {
    def break(): Unit = macro DebugImpl.autoBreak
    def log(): Unit = macro DebugImpl.autoLog
  }
}

object DebugImpl {
  def namedValuesFromTrees(c: Context)(vals: Seq[c.Tree]): List[c.Tree] = {
    import c.universe._
    println(vals)
    println(vals.map(_.getClass))
    vals.collect {
      case v@Select(_, TermName(name)) => q"scadepl.NamedValue($name, $v)"
      case v@Ident(TermName(name)) => q"scadepl.NamedValue($name, $v)"
      case v@Literal(Constant(_)) => q"scadepl.NamedValue(${c.freshName()}, $v)"
      case v@This(TypeName(thisType)) =>
        q"scadepl.NamedValue(${s"${thisType}_this"}, $v)"
    }.toList
  }

  def symbolOwnerChain(c: Context)(sym: c.universe.Symbol): List[c.universe.Symbol] = {
    import c.universe._

    sym.owner match {
      case NoSymbol => sym :: Nil
      case owner => sym :: symbolOwnerChain(c)(owner)
    }
  }

  def expandEnclosingTree(c: Context)(tree: c.universe.Tree): List[c.universe.Tree] = {
    import c.universe._

    if (tree.exists(t => t.pos == c.enclosingPosition))
      tree :: tree.children.flatMap(t => expandEnclosingTree(c)(t))
    else
      List.empty
  }

  def namedValuesInContext(c: Context): List[c.Tree] = {
    import c.universe._

    val owner = c.internal.enclosingOwner
    val tree = c.macroApplication
    val prefix = c.prefix.tree
    val ownerChain = symbolOwnerChain(c)(owner)

    val members: List[TermName] = ownerChain.collect {
      // case s: MethodSymbol => s.paramLists.flatMap(_.map(_.name.toTermName))
      case s: ClassSymbol => s.toType.decls.filter(m => m.isTerm && (m.asTerm.isVal || m.asTerm.isVar)).map(_.name.toTermName)
    }.flatten

    val methodTreeOpt = Option(c.enclosingMethod)
    val localVars: List[TermName] = methodTreeOpt.map { methodTree =>
      val contextTrees = expandEnclosingTree(c)(methodTree)
      val pos = c.enclosingPosition
      contextTrees.flatMap { tree =>
        tree.children.collect{
          case v@ValDef(_, name, _, _) if v.pos precedes pos => name.toTermName
        }
      }
    }.getOrElse(List.empty)

    val definitions = localVars ++ members
    println("DEFINITIONS: " + definitions)
    definitions.map { case term@TermName(name) =>
      q"scadepl.NamedValue($name, $term)"
    }

    // see http://stackoverflow.com/questions/21445108/collecting-all-identifers-in-scope
    // val callingPos = c.enclosingPosition
    // val encMet = Option(c.enclosingMethod)
    // val classElements = c.enclosingClass.asInstanceOf[ImplDefApi].impl.children.collect {
    //     case v: Block => v.asInstanceOf[Tree]
    //     case v: DefDef => v.asInstanceOf[Tree]
    //     case v: ValDef => v.asInstanceOf[Tree]
    //     case v: ModuleDef => v.asInstanceOf[Tree]
    //   }

    // val classFields = classElements.collect {case v@ValDef(_, name, _, _) if !v.isEmpty => name}
    // val blockFields = List()//classElements.collectFirst{
    // // case Block(body, ret) if positionIsInList(c)(body) => body.collect {
    // //   case v@ValDef(_, name, _, _) if v.pos precedes callingPos => Ident(name)
    // // }
    // // }.getOrElse(List())
    // val methodFields = encMet.map(_.collect {case v@ValDef(_, name, _, _) if v.pos precedes callingPos => name}).getOrElse(List())

    // val ignored = Set(TermName("_"))
    // val values = (classFields ++ blockFields ++ methodFields).filterNot(ignored)
  }

  def importsInContext(c: Context): List[c.Tree] = {
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

    val imports = allImports.toList.distinct
    println("IMPORTS: " + imports)
    imports .map(i => q"$i")

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

  def autoBreak(c: Context)(): c.Tree = {
    import c.universe._
    val namedValues = namedValuesInContext(c)
    val imports = importsInContext(c)
    q"scadepl.Debug.break(Seq(..$imports), ..$namedValues)"
  }

  def autoLog(c: Context)(): c.Tree = {
    import c.universe._
    val namedValues = namedValuesInContext(c)
    q"scadepl.Debug.log(..$namedValues)"
  }
}
