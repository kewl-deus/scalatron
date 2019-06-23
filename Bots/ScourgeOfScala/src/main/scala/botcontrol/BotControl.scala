package botcontrol

import scala.util.Random
import framework.{View, XY, Bot, CellCodes, LoggerAware}
import routing.PathFinder

abstract class BotControl extends LoggerAware with CellCodes {

  val rnd = new Random

  private var botOption: Option[Bot] = None

  def bot: Bot = botOption.get

  def hasBot = botOption.isDefined

  def control(bot: Bot) = {
    this.botOption = Some(bot)
    run
  }

  //*********************** abstract operations ************************

  protected def run

  //********************************************************************

  def surroundingArea(i: Int) = (-i to i).flatMap(x => (-i to i).map(y => XY(x, y))).filter(p => p != (XY(0, 0)))

  /**
   *
   * @param origin
   * @param view
   * @return list of coords as moving directions
   */
  def findRoute(origin: XY, destination: XY, view: View) = {
    val pathfinder = new PathFinder(view)
    val path = pathfinder.find(origin, destination)
    log(view.set(path, 'X').toString)
    PathFinder.convertToDirections(origin, path)
  }


  /**
   * Generates a random direction (based on actual position) that has no obstacle in the way
   * @param view
   * @return
   */
  def collisionFreeRandomDirection(view: View) = {
    val collisionDetection: (Char) => Boolean = (c: Char) => List(Wall, EnemyBot, Snorg, Toxifera).contains(c)
    def detect(area: Seq[XY], view: View, detector: Char => Boolean) = area.filter(pos => detector(view.cellAtRelPos(pos)))

    val direction = XY.randomDirection
    val area = surroundingArea(1)

    val walls = detect(area, view, collisionDetection)
    val stuckedInWall = walls.contains(direction.signum)

    if (stuckedInWall) {
      val freeCells = detect(area, view, c => !collisionDetection(c))
      val rndIndex = new Random().nextInt(freeCells.size)
      val wallfreeDirection = freeCells(rndIndex)
      wallfreeDirection
    } else {
      direction
    }
  }

  /**
   * finds absolute position of nearest food if exists
   * @param origin
   * @param view
   * @return
   */
  def findNearestFood(origin: XY, view: View): Option[XY] = {
    val foodPoints = findFoodResources(origin, view)
    if (foodPoints.isEmpty) None else Some(foodPoints.minBy(origin.distanceTo))
  }

  def findNearestEnemy(origin: XY, view: View): Option[XY] = {
    val enemyPoints = findEnemies(origin, view)
    if (enemyPoints.isEmpty) None else Some(enemyPoints.minBy(origin.distanceTo))
  }

  def findFoodResources(origin: XY, view: View): Seq[XY] = view.cells
    .zipWithIndex
    .filter(t => t._1 == Fluppet || t._1 == Zugar)
    .map(t => view.absPosFromIndex(t._2))

  def findEnemies(origin: XY, view: View): Seq[XY] = view.cells
    .zipWithIndex
    .filter(t => t._1 == EnemyBot || t._1 == Snorg)
    .map(t => view.absPosFromIndex(t._2))

  /**
   * Checks if a step in given direction would cause a collision
   * @param start
   * @param direction
   * @param view
   * @return
   */
  def collisionPrediction(start: XY, direction: XY, view: View) = {
    val collision = List(Wall, EnemyBot, Snorg, Toxifera).contains(view.cellAtAbsPos(start + direction))
    if (collision) {
      log("Collision predicted: %s @ %s", view.cellAtAbsPos(start + direction), (start + direction))
    }
    collision
  }

}

object NoopBotControl extends BotControl {
  protected def run = {}
}