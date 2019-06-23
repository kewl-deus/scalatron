package bots.kewlbot.mode

import bots.kewlbot.kewlbot._
import bots.kewlbot.cmd.{BotInstruction, Move, Status}
import bots.kewlbot.util.{ViewSpace, XY}

abstract class SlaveBot extends BotMode {

  val name: String

  def react(view: ViewSpace, params: CommandParameters): BotInstruction

  def positionOfMaster(params: CommandParameters) = XY(params("dx").toInt, params("dy").toInt)

  def followMaster(params: CommandParameters) = {
    val view = ViewSpace(params("view"))
    val masterPos = positionOfMaster(params)
    val distanceToMaster = view.center.distanceTo(masterPos)
    log(name + ": distance to master = " + distanceToMaster)
    if (distanceToMaster > 16) {
      Move(masterPos.signum)
    } else {
      Status("Waiting for master")
    }
  }
}
