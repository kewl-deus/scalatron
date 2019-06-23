package bots.kewlbot.mode

import bots.kewlbot.kewlbot._
import bots.kewlbot.cmd.Move
import bots.kewlbot.util.{ViewSpace, XY}

import scala.util.Random

case class MoveMode(steps: Int, direction: XY, nextMode: MasterBot) extends MasterBot {

  val rnd = new Random()

  def react(view: ViewSpace, params: CommandParameters) = if (steps > 0) {
    move(view)
  } else {
    nextMode.react(view, params)
  }

  def move(view: ViewSpace) = {
    val wallfreeDirection = calculateMoveDirection(direction, view)
    ModeTransition(Move(wallfreeDirection), MoveMode(steps - 1, wallfreeDirection, nextMode))
  }

}
