package bots.deepthought

import scala.collection.Seq
import scala.collection.immutable.List
import scala.util.Random


object MasterBotControl extends BotControl {

  import PathFinder._

  var route: Seq[XY] = Nil

  var target: XY = XY.Zero


  override protected def run = {
    val botPosition: XY = bot.view.center

    if (route.isEmpty || collision(botPosition, route.head, bot.view)) {
      log("routing...")
      //botcontrol.say("routing...")
      try {
        route = calcFoodPath(botPosition, bot.view)
        bot.markCell(target, "#ff8800")
      } catch {
        case ude: UnreachableDestinationException => {
          log(ude)
          route = List(XY.randomDirection)
        }
      }
    }

    //    if (calcDestination (botPosition, route) != target){
    //      log("routing: target moved, need new route")
    //      botcontrol.say("routing...")
    //      route = calcFoodPath(botPosition, botcontrol.view)
    //    }

    log("route = " + route)
    bot.move(route.head)

    target = target - route.head
    route = route.tail

    //TODO: split view in quadrants and spawn minibots to harvest with threshould
  }

  def collision(start: XY, direction: XY, view: View) = {
    val collision = List(Wall, EnemyBot, Snorg, Toxifera).contains(view.cellAtAbsPos(start + direction))
    if (collision) {
      log("Collision predicted: %s @ %s", view.cellAtAbsPos(start + direction), (start + direction))
    }
    collision
  }

  def calcRoute = {
    val quadrants = (0 to 3).map(bot.view.quadrant)
    val qratios = quadrants.map(q => (q, q.count(Zugar, Fluppet).toFloat / q.count(Wall, Snorg).toFloat))
    val bestQuad = qratios.maxBy(_._2)._1

    log("Selected Quadrant: " + bestQuad.cells + "\n" + bestQuad.toString)

    //TODO: calculate quadrantOrigin
    val quadrantOrigin = XY.Zero
    calcFoodPath(quadrantOrigin, bestQuad)
  }


  def calcFoodPath(origin: XY, view: View) = {
    val foodOption = findFood(origin, view)

    foodOption match {
      case Some(food) => {
        bot.status("harvesting")
        log("food ahead: %s @ %s", view.cellAtAbsPos(food), food)
        this.target = food

        val finder = new PathFinder(view)
        //finder.findDirections(origin, food)
        val path = finder.find(origin, food)
        log(view.set(path, 'X').toString)
        convertToDirections(origin, path)
      }
      case _ => {
        bot.status("randomizing")
        //val randomPath = CellPointer(collisionFreeRandomDirection, 3).toPath
        //this.target = calcDestination(origin, randomPath)
        val rnd = new Random
        this.target = origin

        while (view.cellAtAbsPos(this.target) != EmptyCell) {
          val rndIndex = rnd.nextInt(view.cells.length)
          this.target = view.absPosFromIndex(rndIndex)
        }
        log("random target: " + this.target)

        val finder = new PathFinder(view)
        //finder.findDirections(origin, food)
        val path = finder.find(origin, this.target)
        log(view.set(path, 'X').toString)
        convertToDirections(origin, path)
      }
    }
  }

  def collisionFreeRandomDirection(view: View) = {
    val collisionDetection: (Char) => Boolean = (c: Char) => List(Wall, EnemyBot, Snorg, Toxifera).contains(c)
    val surroundingArea = (-1 to 1).flatMap(x => (-1 to 1).map(y => XY(x, y))).filter(p => p != (XY(0, 0)))
    def detect(area: Seq[XY], view: View, detector: Char => Boolean) = area.filter(pos => detector(view.cellAtRelPos(pos)))

    val direction = XY.randomDirection

    val walls = detect(surroundingArea, view, collisionDetection)
    val stuckedInWall = walls.contains(direction.signum)

    if (stuckedInWall) {
      val freeCells = detect(surroundingArea, view, c => !collisionDetection(c))
      val rndIndex = new Random().nextInt(freeCells.size)
      val wallfreeDirection = freeCells(rndIndex)
      wallfreeDirection
    } else {
      direction
    }
  }

  def findFood(origin: XY, view: View): Option[XY] = {

    val foodPoints = view.cells
      .zipWithIndex
      .filter(t => t._1 == Fluppet || t._1 == Zugar)
      .map(t => view.absPosFromIndex(t._2))

    if (foodPoints.isEmpty) None else Some(foodPoints.minBy(origin.distanceTo))
  }

}

class SlaveBotControl extends BotControl {

  override protected def run = {}
}

// -------------------------------------------------------------------------------------------------
// Framework
// -------------------------------------------------------------------------------------------------

abstract class BotControl extends LoggerAware with CellCodes {

  import util.Random

  var lastPosition = XY.Zero

  private var botOption: Option[Bot] = None

  val rnd = new Random

  def bot: Bot = botOption.get

  def control(bot: Bot) = {
    this.botOption = Some(bot)
    run
  }

  protected def run

}

class ControlFunctionFactory {

  Logger.ENABLED = false

  def create: String => String = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {

      case "Welcome" => {
        val path = params("path")
        val round = params("round")
        if (Logger.ENABLED) Logger.configure(path, round)
        ""
      }

      case "Goodbye" => {
        if (Logger.ENABLED) Logger.finish()
        ""
      }

      case "React" => {

        val bot = new BotImpl(params)

        Logger.log("Input: " + input)
        Logger.log(bot.view.toString)

        try {
          bot.generation match {
            case 0 => MasterBotControl.control(bot)
            case _ => new SlaveBotControl().control(bot)
          }
        }
        catch {
          case t: Throwable => {
            t.printStackTrace
            Logger.log(t)
            bot.status("Error!")
            bot.log("Error!")
          }
        }


        val response = bot.toString
        Logger.log("Response: " + response)
        response
      }

      case _ => "" // OK
    }
  }
}

trait CellCodes {
  val MasterBot = 'M'
  val SlaveBot = 'S'

  val OccludedCell = '?'
  val EmptyCell = '_'
  val Wall = 'W'
  val EnemyBot = 'm'
  val EnemySlave = 's'

  /**good plant  */
  val Zugar = 'P'
  /**poisonous plant */
  val Toxifera = 'p'
  /**friendly creature (eatable) */
  val Fluppet = 'B'
  /**hostile creature */
  val Snorg = 'b'
}

trait Bot {
  // inputs
  def inputOrElse(key: String, fallback: String): String

  def inputAsIntOrElse(key: String, fallback: Int): Int

  def inputAsXYOrElse(keyPrefix: String, fallback: XY): XY

  def view: View

  def energy: Int

  def time: Int

  def generation: Int

  // outputs
  def move(delta: XY): Bot

  def say(text: String): Bot

  def status(text: String): Bot

  def spawn(offset: XY, params: (String, Any)*): Bot

  def set(params: (String, Any)*): Bot

  def log(text: String): Bot

  def markCell(position: XY, color: String = "#8888ff"): Bot

  def drawLine(from: XY, to: XY, color: String = "#8888ff"): Bot

  // ---------------------- minibot only -----------------
  def offsetToMaster: XY

  def explode(blastRadius: Int): Bot

  def isMiniBot: Boolean
}



case class BotImpl(inputParams: Map[String, String]) extends Bot {
  // input
  def inputOrElse(key: String, fallback: String) = inputParams.getOrElse(key, fallback)

  def inputAsIntOrElse(key: String, fallback: Int) = inputParams.get(key).map(_.toInt).getOrElse(fallback)

  def inputAsXYOrElse(key: String, fallback: XY) = inputParams.get(key).map(s => XY(s)).getOrElse(fallback)

  val view = View(inputParams("view"))
  val energy = inputParams("energy").toInt
  val time = inputParams("time").toInt
  val generation = inputParams("generation").toInt


  // output

  private var stateParams = Map.empty[String, Any]
  // holds "Set()" commands
  private var commands = ""
  // holds all other commands
  private var debugOutput = "" // holds all "Log()" output

  /**Appends a new command to the command string; returns 'this' for fluent API. */
  protected def append(s: String): Bot = {
    commands += (if (commands.isEmpty) s else "|" + s);
    this
  }

  /**Renders commands and stateParams into a control function return string. */
  override def toString = {
    var result = commands
    if (!stateParams.isEmpty) {
      if (!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(", ",", ")")
    }
    if (!debugOutput.isEmpty) {
      if (!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def log(text: String) = {
    debugOutput += text + "\n";
    this
  }

  def move(direction: XY) = append("Move(direction=" + direction + ")")

  def say(text: String) = append("Say(text=" + text + ")")

  def status(text: String) = append("Status(text=" + text + ")")


  def spawn(offset: XY, params: (String, Any)*) =
    append("Spawn(direction=" + offset +
      (if (params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")

  def set(params: (String, Any)*) = {
    stateParams ++= params;
    this
  }

  def set(keyPrefix: String, xy: XY) = {
    stateParams ++= List(keyPrefix + "x" -> xy.x, keyPrefix + "y" -> xy.y);
    this
  }

  def markCell(position: XY, color: String): Bot = append("MarkCell(direction=" + position + ",color=" + color + ")")

  def drawLine(from: XY, to: XY, color: String): Bot = append("DrawLine(from=" + from + ",to=" + to + ",color=" + color + ")")

  def explode(blastRadius: Int) = if (isMiniBot) append("Explode(size=" + blastRadius + ")") else this

  def offsetToMaster = if(isMiniBot) inputAsXYOrElse("master", XY.Zero) else view.center

  def isMiniBot: Boolean = generation > 0
}


// -------------------------------------------------------------------------------------------------


/**Utility methods for parsing strings containing a single command of the format
 * "Command(key=value,key=value,...)"
 */
object CommandParser {
  /**"Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
  def apply(command: String): (String, Map[String, String]) = {
    /**"key=value" => ("key","value") */
    def splitParameterIntoKeyValue(param: String): (String, String) = {
      val segments = param.split('=')
      (segments(0), if (segments.length >= 2) segments(1) else "")
    }

    val segments = command.split('(')
    if (segments.length != 2)
      throw new IllegalStateException("invalid command: " + command)
    val opcode = segments(0)
    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
    (opcode, keyValuePairs)
  }
}


// -------------------------------------------------------------------------------------------------


/**Utility class for managing 2D destination coordinates.
 * The coordinate (0,0) corresponds to the top-left corner of the arena on screen.
 * The direction (1,-1) points right and up.
 */
case class XY(x: Int, y: Int) {
  override def toString = x + ":" + y

  def isNonZero = x != 0 || y != 0

  def isZero = x == 0 && y == 0

  def isNonNegative = x >= 0 && y >= 0

  def updateX(newX: Int) = XY(newX, y)

  def updateY(newY: Int) = XY(x, newY)

  def addToX(dx: Int) = XY(x + dx, y)

  def addToY(dy: Int) = XY(x, y + dy)

  def +(pos: XY) = XY(x + pos.x, y + pos.y)

  def -(pos: XY) = XY(x - pos.x, y - pos.y)

  def *(factor: Double) = XY((x * factor).intValue, (y * factor).intValue)

  def distanceTo(pos: XY): Double = (this - pos).length

  // Phythagorean
  def length: Double = math.sqrt(x * x + y * y) // Phythagorean

  def stepsTo(pos: XY): Int = (this - pos).stepCount

  // steps to reach pos: max delta X or Y
  def stepCount: Int = x.abs.max(y.abs) // steps from (0,0) to get here: max X or Y

  def signum = XY(x.signum, y.signum)

  def negate = XY(-x, -y)

  def negateX = XY(-x, y)

  def negateY = XY(x, -y)

  /**Returns the direction index with 'Right' being index 0, then clockwise in 45 degree steps. */
  def toDirection45: Int = {
    val unit = signum
    unit.x match {
      case -1 =>
        unit.y match {
          case -1 =>
            if (x < y * 3) Direction45.Left
            else if (y < x * 3) Direction45.Up
            else Direction45.UpLeft
          case 0 =>
            Direction45.Left
          case 1 =>
            if (-x > y * 3) Direction45.Left
            else if (y > -x * 3) Direction45.Down
            else Direction45.LeftDown
        }
      case 0 =>
        unit.y match {
          case 1 => Direction45.Down
          case 0 => throw new IllegalArgumentException("cannot compute direction index for (0,0)")
          case -1 => Direction45.Up
        }
      case 1 =>
        unit.y match {
          case -1 =>
            if (x > -y * 3) Direction45.Right
            else if (-y > x * 3) Direction45.Up
            else Direction45.RightUp
          case 0 =>
            Direction45.Right
          case 1 =>
            if (x > y * 3) Direction45.Right
            else if (y > x * 3) Direction45.Down
            else Direction45.DownRight
        }
    }
  }

  def rotateCounterClockwise45 = XY.fromDirection45((signum.toDirection45 + 1) % 8)

  def rotateCounterClockwise90 = XY.fromDirection45((signum.toDirection45 + 2) % 8)

  def rotateClockwise45 = XY.fromDirection45((signum.toDirection45 + 7) % 8)

  def rotateClockwise90 = XY.fromDirection45((signum.toDirection45 + 6) % 8)


  def wrap(boardSize: XY) = {
    val fixedX = if (x < 0) boardSize.x + x else if (x >= boardSize.x) x - boardSize.x else x
    val fixedY = if (y < 0) boardSize.y + y else if (y >= boardSize.y) y - boardSize.y else y
    if (fixedX != x || fixedY != y) XY(fixedX, fixedY) else this
  }
}


object XY {
  /**Parse an XY value from XY.toString format, e.g. "2:3". */
  def apply(s: String): XY = {
    val a = s.split(':');
    XY(a(0).toInt, a(1).toInt)
  }

  val Zero = XY(0, 0)
  val One = XY(1, 1)

  val Right = XY(1, 0)
  val RightUp = XY(1, -1)
  val Up = XY(0, -1)
  val UpLeft = XY(-1, -1)
  val Left = XY(-1, 0)
  val LeftDown = XY(-1, 1)
  val Down = XY(0, 1)
  val DownRight = XY(1, 1)

  def fromDirection45(index: Int): XY = index match {
    case Direction45.Right => Right
    case Direction45.RightUp => RightUp
    case Direction45.Up => Up
    case Direction45.UpLeft => UpLeft
    case Direction45.Left => Left
    case Direction45.LeftDown => LeftDown
    case Direction45.Down => Down
    case Direction45.DownRight => DownRight
  }

  def fromDirection90(index: Int): XY = index match {
    case Direction90.Right => Right
    case Direction90.Up => Up
    case Direction90.Left => Left
    case Direction90.Down => Down
  }

  def randomDirection = {
    val rnd = new util.Random
    var xy = Zero
    while (xy == Zero) {
      xy = XY(rnd.nextInt(3) - 1, rnd.nextInt(3) - 1)
    }
    xy
  }

  def apply(array: Array[Int]): XY = XY(array(0), array(1))
}


object Direction45 {
  val Right = 0
  val RightUp = 1
  val Up = 2
  val UpLeft = 3
  val Left = 4
  val LeftDown = 5
  val Down = 6
  val DownRight = 7
}


object Direction90 {
  val Right = 0
  val Up = 1
  val Left = 2
  val Down = 3
}

case class CellPointer(destination: XY, distance: Int) {
  def direction = destination.signum

  def toPath: Seq[XY] = (0 to distance).map(i => destination)

  override def toString = "%d -> %s".format(distance, destination)
}

// -------------------------------------------------------------------------------------------------

/**
 * @see http://www.policyalmanac.org/games/aStarTutorial_de.html
 * @param view
 */
class PathFinder(private var view: View) extends CellCodes {

  import PathFinder._

  val obstacles = List(Wall, Toxifera, Snorg)

  lazy val ambienceVector = (-1 to 1).flatMap(x => (-1 to 1).map(y => XY(x, y))).filter(p => p.isNonZero)

  private var closedList: List[PathNode] = List()

  private var openList: List[PathNode] = List()

  /**
   * absolute coords
   * @param point
   * @return
   */
  def ambience(point: XY) = ambienceVector
    .map(p => point + p)
    .filter(p => p.isNonNegative && p.x < view.size && p.y < view.size) //point is in viewbounds

  /**
   * nonblocked cells
   * @param point
   * @return
   */
  def reachableAmbience(point: XY) = ambience(point).filterNot(p => obstacles.contains(view.cellAtAbsPos(p)))

  /**
   * Returns path as absolute coordinates
   * @param start
   * @param destination
   * @return
   */
  def find(start: XY, destination: XY): Seq[XY] = {
    val root = PathNode(start, None, destination)
    openList = List(root)
    closedList = List()

    while (closedList.lastOption.getOrElse(root).point != destination) {
      searchPath
    }

    var backtrace: Option[PathNode] = closedList.lastOption
    var path: List[XY] = List()
    while (!backtrace.getOrElse(root).samePoint(root)) {
      val bt = backtrace.get
      path = bt.point :: path
      backtrace = bt.parent
    }
    path
  }

  /**
   * Returns path as list of directions
   * @param start
   * @param destination
   * @return
   */
  def findDirections(start: XY, destination: XY): Seq[XY] = {
    val path = find(start, destination)
    convertToDirections(start, path)
  }


  private def searchPath {
    //    println("searchPath ------------------------------------")
    val currentNode = nodeWithLowestCost
    closedList :+= currentNode //append node
    openList = openList.filterNot(pn => pn.cost == currentNode.cost) //remove all nodes with same cost

    val neighbours = reachableNeighbours(currentNode)
    val existingNeighbours = openList.filter(p => neighbours.exists(n => p.samePoint(n)))

    val cheaperPaths = existingNeighbours.map(nb => {
      val testNb = PathNode(nb.point, Some(currentNode), currentNode.destination)
      //      println("cheaper test: " + nb + " vs. " + testNb)
      //is new neighbour a cheaper path?
      if (testNb.costFromRoot < nb.costFromRoot) {
        Some(testNb)
      } else {
        None
      }
    }).flatten

    //    println("currentNode: " + currentNode)
    //    printNodeList("closedList", closedList)
    //    printNodeList("openList", openList)
    //    printNodeList("neighbours", neighbours)
    //    printNodeList("existingNeighbours", existingNeighbours)
    //    printNodeList("cheaperPaths", cheaperPaths)

    //replace expensive nodes with cheaper nodes if such exists
    openList = openList.filterNot(p => cheaperPaths.exists(n => p.samePoint(n)))
    openList ++= cheaperPaths

    //add only neighbours that not already exist
    openList ++= neighbours.filterNot(n => openList.exists(p => n.samePoint(p)))

    //    printNodeList("new openList", openList)
  }

  //  private def printNodeList(label: String, list: Seq[PathNode]){
  //    println(label)
  //    println("----------")
  //    println(list.mkString("\n"))
  //    println(view.set(list.map(_.point), 'X'))
  //  }

  private def nodeWithLowestCost = if (openList.isEmpty) {
    throw new UnreachableDestinationException
  } else {
    openList.minBy(_.cost)
  }

  private def reachableNeighbours(start: PathNode) = reachableAmbience(start.point)
    .filterNot(p => closedList.exists(node => node.point == p))
    .map(p => PathNode(p, Some(start), start.destination))

}

class UnreachableDestinationException extends RuntimeException

object PathFinder {
  def calcDestination(start: XY, directions: Seq[XY]) = directions.foldLeft(start)((point, delta) => point + delta)

  def convertToDirections(startPoint: XY, path: Seq[XY]) = {
    var points = path
    var previousPoint = startPoint
    var directions = List[XY]()
    while (!points.isEmpty) {
      val point = points.head
      directions :+= point - previousPoint
      previousPoint = point
      points = points.tail
    }
    directions
  }
}

case class PathNode(point: XY, parent: Option[PathNode], destination: XY) {

  val linearStepCost = 10

  val diagonalStepCost = 14

  val diagonalDirections = List(XY.DownRight, XY.LeftDown, XY.RightUp, XY.UpLeft)

  def manhattenDistance(start: XY, end: XY) = (start.x - end.x).abs + (start.y - end.y).abs

  def costFromParent: Int = parent match {
    case Some(pNode) => {
      diagonalDirections.exists(xy => this.point + xy == pNode.point) match {
        case true => 14 //parent is diagonal reachable
        case false => 10
      }
    }
    case None => 0
  }


  def costFromRoot: Int = {
    var node = this
    var cost = 0
    while (!node.isRoot) {
      cost += node.costFromParent
      node = node.parent.get
    }
    cost
  }

  def costToDestination: Int = manhattenDistance(point, destination) * linearStepCost

  def cost = costFromRoot + costToDestination

  def isRoot = parent.isEmpty

  def samePoint(other: PathNode) = this.point == other.point

  override def toString = "F(%d:%d) = G:%d + H:%d = %d".format(point.x, point.y, costFromRoot, costToDestination, cost)
}


// -------------------------------------------------------------------------------------------------


case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size / 2, size / 2)

  def apply(relPos: XY) = cellAtRelPos(relPos)

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size

  def absPosFromIndex(index: Int) = XY(index % size, index / size)

  def absPosFromRelPos(relPos: XY) = relPos + center

  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))

  def relPosFromAbsPos(absPos: XY) = absPos - center

  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))

  def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

  def offsetToNearest(c: Char) = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if (matchingXY.isEmpty)
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      Some(nearest)
    }
  }

  def contains(c: Char): Boolean = cells.contains(c)

  def count(ch: Char*): Int = cells.count(cell => ch.contains(cell))

  def quadrant(q: Int): View = {
    val q0 = q % 4 //quadrant counting with 0-3
    val offset = q0 % 2 match {
        case 0 => 0
        case 1 => cells.length / 2
      }
    val slice = cells.slice(offset, offset + cells.length / 2)
    val qCells = slice.sliding(size / 2, size)
    View(qCells.mkString)
  }

  def set(absPos: XY, value: Char): View = new View(cells.updated(indexFromAbsPos(absPos), value).mkString)

  def set(path: Seq[XY], value: Char): View = path.foldLeft(this)((v, point) => v.set(point, value))

  override def toString = {
    new String(cells.map(x =>
      if (x < 10) " " + x else x).sliding(size, size).flatMap(l =>
      l.addString(new StringBuilder, "", "\t", "\n")).toArray)
  }
}


// -------------------------------------------------------------------------------------------------

trait LoggerAware {
  def log(message: String, args: Any*) = Logger.log(message.format(args: _*))

  def log(error: Throwable) = Logger.log(error)
}

object Logger {

  import java.io.{File, FileWriter, PrintWriter}

  var ENABLED = true

  var logWriter = new PrintWriter(new FileWriter(File.createTempFile("scalatron_bot", ".log")))

  def configure(path: String, round: String) {
    logWriter = new PrintWriter(new FileWriter(path + "/" + System.currentTimeMillis + "_round_" + round + ".log", false))
  }

  def log(message: String) = if (ENABLED) logWriter.println(message)

  def log(error: Throwable) = if (ENABLED) error.printStackTrace(logWriter)

  def flush() = if (ENABLED) logWriter.flush()

  def finish() = if (ENABLED) {
    logWriter.flush()
    logWriter.close()
  }
}