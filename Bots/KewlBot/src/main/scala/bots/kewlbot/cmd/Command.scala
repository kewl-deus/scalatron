package bots.kewlbot.cmd

class Command(opCode: String) extends BotInstruction {

  private var params: Map[String, String] = Map()

  def setParam(name: String, value: Any) {
    params = params.updated(name, value.toString)
  }

  def <<(name: String, value: Any) = {
    setParam(name, value)
    this
  }

  override def chain(cmd: BotInstruction) = new CommandChain().chain(this).chain(cmd)

  override def toString = params.map(kv => kv._1 + "=" + kv._2).addString(new StringBuilder, opCode + "(", ",", ")").toString
}
