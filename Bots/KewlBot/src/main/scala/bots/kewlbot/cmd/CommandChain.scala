package bots.kewlbot.cmd

class CommandChain extends BotInstruction {

  var chain: Vector[BotInstruction] = Vector()

  def flatten = chain.flatten(c => c match {
    case ch: CommandChain => ch.chain
    case cmd => List(cmd)
  })

  override def chain(cmd: BotInstruction) = {
    chain :+= cmd
    this
  }

  override def toString = if (chain.isEmpty) "" else this.flatten.addString(new StringBuilder, "|").toString
}
