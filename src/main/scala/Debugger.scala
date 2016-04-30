package scadepl

import org.scaladebugger.api.debuggers.LaunchingDebugger
import org.scaladebugger.api.utils.JDITools
import com.sun.jdi._
import com.sun.jdi.event.BreakpointEvent
import collection.JavaConverters._

case class BreakMeta(frame: StackFrame, event: BreakpointEvent)

object Debugger {
  import Utils._

  def namedValuesToParams(vars: Map[String, Value]): List[NamedValue[_]] = {
    vars.toList.map { case (name, value) =>
      value match {
        case v: BooleanValue => NamedValue(name, v.value)
        case v: ByteValue => NamedValue(name, v.value)
        case v: CharValue => NamedValue(name, v.value)
        case v: DoubleValue => NamedValue(name, v.value)
        case v: FloatValue => NamedValue(name, v.value)
        case v: IntegerValue => NamedValue(name, v.value)
        case v: LongValue => NamedValue(name, v.value)
        case v: StringReference => NamedValue(name, v.value)
        case v: ShortValue => NamedValue(name, v.value)
        // case v: VoidValue => NamedValue(name, v.value)
        // case v: ArrayReference => NamedValue(name, v.value)
        // case v: ClassLoaderReference => NamedValue(name, v.value)
        // case v: ClassObjectReference => NamedValue(name, v.value)
        case v: ObjectReference => NamedValue(name, new Object() {})
        // case v: ThreadGroupReference => NamedValue(name, v.value)
        // case v: ThreadReference => NamedValue(name, v.value)
        case v => throw new Exception(s"unknown value type: $value")
      }
    }
  }

  def createDebugger(mainClass: Class[_]) = {
    val className = classToName(mainClass)
    val classpath = JDITools.jvmClassPath
    val jvmOptions = Seq("-classpath", classpath)

    LaunchingDebugger(
      className = className,
      jvmOptions = jvmOptions,
      suspend = true // Wait to start the main class until after connected
    )
  }

  def launch(mainClass: Class[_]) {
    val debugger = createDebugger(mainClass)

    debugger.start { s =>
      println("Launched and connected to JVM: " + s.uniqueId)

      val p = debugger.process.get
      inputStreamToOutputStream(p.getErrorStream(), System.err);
      inputStreamToOutputStream(p.getInputStream(), System.out);

      // s.onAllExceptions(false, true).foreach(e => {
      //   println(s"Unhandled exception: $e")
      //   DebugRepl.break(NamedParam("e", e.asInstanceOf[Any]))
      //   debugger.stop()
      // })

      val className = classToName(DebugRepl.getClass)
      val fileName = JDITools.scalaClassStringToFileString(className)
      val lineNumber = 42 //TODO: method enter/exit
      s.onUnsafeBreakpoint(fileName, lineNumber).foreach(e => {
        val path = e.location().sourcePath()
        val line = e.location().lineNumber()

        println(s"Reached breakpoint for $path:$line")
        val debugName = scadepl.DebugRepl.Smart.getClass.getName
        val frames = e.thread.frames.asScala.dropWhile(_.thisObject.`type`.name == debugName)
        frames.headOption.foreach { frame =>
          println(s"Current stack frame: $frame")
          val bp = BreakMeta(frame, e)
          val preDefinedParam = NamedValue("_bp", bp)
          val vars = frame.getValues(frame.visibleVariables).asScala.toMap.map { case (k,v) => (k.name,v) }
          val fields = frame.thisObject.getValues(frame.thisObject.referenceType.visibleFields).asScala.toMap.map { case (k,v) => (k.name,v) }
          val params = preDefinedParam :: namedValuesToParams(vars) ++ namedValuesToParams(fields)
          // val params = List(preDefinedParam)

          DebugRepl.break(params : _*)
        }
        // debugger.stop()
      })

      // val className = classToName(DebugRepl.Smart.getClass)
      // val funcName = "break"
      // s.onMethodExit(className, funcName).foreach(e => {
      //   println(s"Break at: $e")
      //   DebugRepl.break(NamedValue("e", e))
      // })
    }

    // Keep the sample program running while our debugger is running
    //TODO: exit?
    while (debugger.isRunning) Thread.sleep(1)
  }
}
