package scadepl

import reflect.runtime.universe.TypeTag

class ReplILoop(imports: Seq[String] = Seq.empty, namedValues: Seq[NamedValue[_]] = Seq.empty) extends ILoopWithInit {

  override protected def init() {
    echo("Binding scope:")
    imports.foreach(i => intp.interpret(s"import $i"))
    namedValues.foreach { v =>
      val tpe = v.typeTag match {
        case t: TypeTag[_] => t.tpe.toString
        case _             => v.value.getClass.getName // TODO: this is only a workaround for generics
      }

      intp.bind(v.name, tpe, v.value)
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
      nullary("ls", "show all defined parameters", DebugCommands.ls)
    )

    def ls() {
      namedValues.foreach { param =>
        val line = s"${param.name}: ${param.typeTag.tpe} = ${param.value}"
        println(line)
      }
    }
  }
}
