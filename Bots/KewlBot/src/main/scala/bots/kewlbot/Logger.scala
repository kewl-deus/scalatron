package bots.kewlbot

import java.io.{File, FileWriter, PrintWriter}


trait LoggerAware {
  def log(message: String, args: Any*) = Logger.log(message.format(args :_*))
}

object Logger {

  var ENABLED = true

  var logWriter = new PrintWriter(new FileWriter(File.createTempFile("scalatron_bot", ".log")))

  def configure(path: String, round: String) {
    logWriter = new PrintWriter(new FileWriter(path + "/" + System.currentTimeMillis + "_round_" + round + ".log", false))
  }

  def log(message: String) = if (ENABLED) logWriter.println(message)

  def log(error: Throwable) = if (ENABLED) error.printStackTrace(logWriter)

  def flush() = if (ENABLED) logWriter.flush()

  def finish() = if (ENABLED) {
    logWriter.flush()
    logWriter.close()
  }
}
