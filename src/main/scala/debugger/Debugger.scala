package scadepl.debugger

import org.scaladebugger.api.debuggers.LaunchingDebugger
import org.scaladebugger.api.utils.JDITools
import com.sun.jdi.event.BreakpointEvent
import collection.JavaConverters._
import scadepl.{Debug, NamedValue}

case class DebugSettings(breakOnException: Boolean = false, breakpoints: Seq[(String,Int)] = Seq.empty)

object Debugger {
  import Helpers._

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

  def launch(mainClass: Class[_], settings: DebugSettings = DebugSettings()) {
    val debugger = createDebugger(mainClass)

    debugger.start { s =>
      println("Launched and connected to JVM: " + s.uniqueId)

      val p = debugger.process.get
      inputStreamToOutputStream(p.getErrorStream(), System.err);
      inputStreamToOutputStream(p.getInputStream(), System.out);

      if (settings.breakOnException) {
        s.onUnsafeAllExceptions(false, true).foreach(e => {
          println(s"Unhandled exception: $e")
          Debug.repl(NamedValue("_exception", e))
        })
      }

      val debugClass = Debuggee.getClass
      val className = classToName(debugClass)
      val breakMethod = JDITools.scalaClassStringToFileString(className) -> 5
      val breakpoints = breakMethod :: settings.breakpoints.toList
      breakpoints.foreach { case (fileName, lineNumber) =>
        s.onUnsafeBreakpoint(fileName, lineNumber).foreach(e => {
          val path = e.location().sourcePath()
          val line = e.location().lineNumber()

          println(s"Reached breakpoint for $path:$line")
          val debugName = debugClass.getName
          val frames = e.thread.frames.asScala.dropWhile(_.thisObject.`type`.name == debugName)
          frames.headOption.foreach { frame =>
            println(s"Current stack frame: $frame")
            val definedValues = List(NamedValue("_bp", e), NamedValue("_frame", frame))
            val vars = frame.getValues(frame.visibleVariables).asScala.toMap.map { case (k,v) => (k.name,v) }
            val fields = frame.thisObject.getValues(frame.thisObject.referenceType.visibleFields).asScala.toMap.map { case (k,v) => (k.name,v) }
            val params = definedValues ++ namedValues(vars) ++ namedValues(fields)

            Debug.repl(params : _*)
          }
        })
      }
    }

    while (debugger.isRunning && debugger.process.isEmpty)
      Thread.sleep(200)

    debugger.process.foreach(_.waitFor())
    if (debugger.isRunning)
      debugger.stop()
  }
}
