package botcontrol

import routing.PongAlgorithm
import framework.XY

object MasterBotControl extends BotControl {

  var breadcrumbs: List[XY] = List()

  def lastDirection = breadcrumbs.lastOption.getOrElse(XY.RightUp)

  def init = breadcrumbs = List()

  protected def run = {

    val direction = PongAlgorithm(bot.view, lastDirection)

    breadcrumbs :+= direction.signum
    bot.move(direction)

    val food = bot.view.offsetToNearest(Fluppet, Zugar).getOrElse(XY(999, 999))

    if (bot.time % 8 == 0 && food.length < 14 && food.signum != direction.signum) {
      val miniBot = new TracingMiniBot("tracer" + SlaveBotRegistry.slaveCount, 390)
      SlaveBotRegistry.put(miniBot)
      bot.spawn(food.signum, ("name", miniBot.name), ("energy", 100),
        ("breadcrumbIndex", breadcrumbs.size - 1), ("spawnOffset", food.signum))

    }
  }

}
