package scadepl

import java.io.{InputStream, PrintStream}

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
}
