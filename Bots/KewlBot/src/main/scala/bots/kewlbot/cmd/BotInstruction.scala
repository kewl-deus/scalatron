package bots.kewlbot.cmd

abstract class BotInstruction {

  def chain(cmd: BotInstruction): BotInstruction

  def |(cmd: BotInstruction) = chain(cmd)
}
