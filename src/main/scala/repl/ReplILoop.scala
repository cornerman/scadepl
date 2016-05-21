package scadepl.repl

import scadepl.NamedValue

class ReplILoop(imports: Seq[String] = Seq.empty, namedValues: Seq[NamedValue] = Seq.empty) extends ILoopWithInit {

  override protected def init() {
    echo("Debug repl started. Welcome!")

    echo("Importing: ")
    val importCode = imports.distinct.map(i => s"import $i").mkString(";")
    intp.interpret(importCode)

    echo("Binding values:")
    namedValues.toList.distinct.foreach { v =>
      intp.bind(v.strippedName, v.typeName, v.value)
    }
    // intp.beQuietDuring {
    // }
  }

  def freshReplScope = {
    intpOpt.map { intp =>
      val valueNames = namedValues.map(_.name).toSet
      intp.replScope.dropWhile { sym =>
        val symName = sym.name.decoded
        symName.startsWith("$") || valueNames.contains(symName)
      }
    }.getOrElse(intp.replScope)
  }

  def lastResult: Option[Any] = {
    intpOpt.flatMap { intp =>
      val symOpt = freshReplScope.filter(_.name.decoded.matches("^res\\d+")).lastOption
      symOpt.flatMap(sym => intp.valueOfTerm(sym.name.decoded))
    }
  }

  override def prompt = super.prompt

  override def commands = super.commands ++ DebugCommands.cmds

  object DebugCommands {
    import LoopCommand.{cmd, nullary}

    lazy val cmds = List(
      nullary("ls", "show all defined parameters", ls)
    )

    def ls() {
      namedValues.foreach(println)
    }
  }
}
