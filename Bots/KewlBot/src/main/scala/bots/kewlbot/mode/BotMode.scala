package bots.kewlbot.mode

import bots.kewlbot.LoggerAware
import bots.kewlbot.cmd.{CommandChain, Move, Say}
import bots.kewlbot.util.{ViewSpace, XY}
import bots.kewlbot.kewlbot._

import scala.util.Random

abstract class BotMode extends LoggerAware {
  protected var commandStack: CommandChain = new CommandChain()

  protected var lastMove = XY(0, 0)

  protected val collisionDetection: (Char) => Boolean = (c: Char) => List(Wall, EnemyBot, Snorg, Toxifera).contains(c)


  def surroundingArea = (-1 to 1).flatMap(x => (-1 to 1).map(y => XY(x, y))).filter(p => p != (XY(0, 0)))

  def zoom(area: Seq[XY], factor: Int) = area.map(xy => XY(xy.x * factor, xy.y * factor))

  def isCycle(plannedMove: XY) = plannedMove.x == -lastMove.x && plannedMove.y == -lastMove.y

  /**
   * Collision detection
   */
  def detect(area: Seq[XY], view: ViewSpace, detector: Char => Boolean) = area.filter(pos => detector(view.cellAtRelPos(pos)))

  def move(direction: XY, view: ViewSpace) = {
    var plannedMove = calculateMoveDirection(direction, view)
    if (isCycle(plannedMove)) {
      commandStack | Say("cycle detected")
      plannedMove = XY.randomDirection
    }
    lastMove = plannedMove
    Move(plannedMove)
  }

  def calculateMoveDirection(direction: XY, view: ViewSpace) = {

    val walls = detect(surroundingArea, view, collisionDetection)
    val stuckedInWall = walls.contains(direction.signum)

    if (stuckedInWall) {
      val freeCells = detect(surroundingArea, view, c => !collisionDetection(c))
      val rndIndex = new Random().nextInt(freeCells.size)
      val wallfreeDirection = freeCells(rndIndex)
      log("walls detected: " + walls)
      commandStack | Say("i stuck in a wall")
      wallfreeDirection
    } else {
      direction
    }
  }
}
