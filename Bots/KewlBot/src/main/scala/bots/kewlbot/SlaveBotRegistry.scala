package bots.kewlbot

import bots.kewlbot.mode.SlaveBot

object SlaveBotRegistry {
  private var slaveBots: Map[String, SlaveBot] = Map()

  def register(bot: SlaveBot) = slaveBots = slaveBots.updated(bot.name, bot)

  def getBot(name: String) = slaveBots.get(name)
}
