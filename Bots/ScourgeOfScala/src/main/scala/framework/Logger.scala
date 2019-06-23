package framework

trait LoggerAware {
  def log(message: String, args: Any*) = Logger.log(message.format(args: _*))

  def log(error: Throwable) = Logger.log(error)
}

object Logger {

  import java.io.{PrintWriter, File, FileWriter}

  var ENABLED = true

  var logWriter = new PrintWriter(System.out)

  def configure(path: String, round: String, outputToFile: Boolean = false) {
    logWriter = outputToFile match {
      case true => new PrintWriter(new FileWriter(path + "/" + System.currentTimeMillis + "_round_" + round + ".log", false))
      case _ => new PrintWriter(System.out)
    }
  }

  def log(message: String) = if (ENABLED) logWriter.println(message)

  def log(error: Throwable) = if (ENABLED) error.printStackTrace(logWriter)

  def flush() = if (ENABLED) logWriter.flush()

  def finish() = if (ENABLED) {
    logWriter.flush()
    logWriter.close()
  }
}