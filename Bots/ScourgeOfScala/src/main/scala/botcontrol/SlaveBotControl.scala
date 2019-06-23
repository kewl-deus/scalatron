package botcontrol

abstract class SlaveBotControl(val name: String) extends BotControl {
  def timeLeft = ScourgeMaster.roundTime - bot.time
}