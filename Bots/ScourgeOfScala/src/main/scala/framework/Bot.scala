package framework


trait Bot {
  // inputs
  def inputOrElse(key: String, fallback: String): String

  def inputAsIntOrElse(key: String, fallback: Int): Int

  def inputAsXYOrElse(keyPrefix: String, fallback: XY): XY

  def view: View

  def energy: Int

  def time: Int

  def generation: Int

  def position: XY

  // outputs
  def move(delta: XY): Bot

  def say(text: String): Bot

  def status(text: String): Bot

  def spawn(offset: XY, params: (String, Any)*): Bot

  def spawn(offset: XY, name: String, energy: Int): Bot

  def set(params: (String, Any)*): Bot

  def log(text: String): Bot

  def markCell(position: XY, color: String = "#8888ff"): Bot

  def drawLine(from: XY, to: XY, color: String = "#8888ff"): Bot

  // ---------------------- minibot only -----------------
  def offsetToMaster: XY

  def explode(blastRadius: Int): Bot

  def isMiniBot: Boolean

  def isMasterBot: Boolean
}


