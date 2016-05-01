package scadepl.debugger

import java.io.{InputStream, PrintStream}
import scadepl.NamedValue
import com.sun.jdi._

object Utils {
  def classToName(klass: Class[_]) = klass.getName.replaceAllLiterally("$", "")

  def inputStreamToOutputStream(in: InputStream, out: PrintStream) {
    val t = new Thread {
      override def run() {
        val scan = new java.util.Scanner(in);
        while (scan.hasNextLine()) {
          out.println(scan.nextLine());
        }
      }
    }
    t.setDaemon(true)
    t.start()
  }

  def namedValues(vars: Map[String, Value]): List[NamedValue[_]] = {
    vars.toList.flatMap { case (name, value) =>
      value match {
        case v: BooleanValue => Some(NamedValue(name, v.value))
        case v: ByteValue => Some(NamedValue(name, v.value))
        case v: CharValue => Some(NamedValue(name, v.value))
        case v: DoubleValue => Some(NamedValue(name, v.value))
        case v: FloatValue => Some(NamedValue(name, v.value))
        case v: IntegerValue => Some(NamedValue(name, v.value))
        case v: LongValue => Some(NamedValue(name, v.value))
        case v: StringReference => Some(NamedValue(name, v.value))
        case v: ShortValue => Some(NamedValue(name, v.value))
        // case v: VoidValue => ???
        // case v: ArrayReference => ???
        // case v: ClassLoaderReference => ???
        // case v: ClassObjectReference => ???
        // case v: ObjectReference => ???
        // case v: ThreadGroupReference => ???
        // case v: ThreadReference => ???
        case v => None
      }
    }
  }

}
