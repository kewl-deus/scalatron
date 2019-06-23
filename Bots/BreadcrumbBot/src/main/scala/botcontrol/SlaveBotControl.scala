package botcontrol

abstract class SlaveBotControl(val name: String) extends BotControl {

  def distanceToMaster = bot.offsetToMaster.length
}