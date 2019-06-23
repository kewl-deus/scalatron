import scala.util.Random

abstract class BotControl extends LoggerAware with CellCodes {

  val rnd = new Random

  private var botOption: Option[Bot] = None

  protected def bot: Bot = botOption.get

  protected var lastPosition = XY.Zero

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

object Overmind extends BotControl {

  var roundTime: Int = Int.MaxValue

  protected var route: Seq[XY] = Nil

  protected var lastBombSpawn = 0

  def welcomeMessage = if (bot.time < 2) bot.say("I sense fear!")

  def timeLeft = roundTime - bot.time

  override protected def run = {

    welcomeMessage

    //spawning
    if (timeLeft > 150) {
      //spawnZergling drone
      if (bot.time % 2 == 0) {
        spawnZergling
      } else {
        if (bot.energy > 9000
          && bot.time - lastBombSpawn > 25
          && bot.view.offsetToNearest(EnemyBot).getOrElse(XY(99, 99)).length < 14
        ) {
          val bomb = new Baneling("bomb" + System.currentTimeMillis)
          HiveCluster.enslave(bomb)
          bot.spawn(bot.position, bomb.name, ((bot.energy % 10) / 2).max(1000))
          lastBombSpawn = bot.time
        }
      }
    }

    if (route.isEmpty || collisionPrediction(bot.position, route.head, bot.view)) {
      log("routing...")
      try {
        route = calcFoodPath(bot.position, bot.view)
      } catch {
        case ude: UnreachableDestinationException => {
          log(ude)
          route = List(XY.randomDirection)
        }
      }
    }

    //move master
    log("route = " + route)
    bot.move(route.head)
    route = route.tail

  }

  def spawnZergling = {
    val foods = findFoodResources(bot.position, bot.view).filter(f => f.length < 21)
    log("detected %d foodresources in minibot range", foods.size)
    if (foods.size > 1) {
      log("spawning zergling")
      val randomFood = foods(rnd.nextInt(foods.size))
      val zergRoute = findRoute(bot.position, randomFood, bot.view)
      val zergling = new Zergling("zergling" + System.currentTimeMillis(), 100, zergRoute.tail)
      HiveCluster.enslave(zergling)
      bot.spawn(zergRoute.head, zergling.name, zergling.startEnergy)
    }
  }

  def calcFoodPath(origin: XY, view: View) = {
    val foodOption = findNearestFood(origin, view)

    val destination = foodOption match {
      case Some(food) => {
        bot.status("harvesting")
        food
      }
      case _ => {
        bot.status("randomizing")
        var target = origin

        while (view.cellAtAbsPos(target) != EmptyCell && origin.distanceTo(target) < 14) {
          val rndIndex = rnd.nextInt(view.cells.length)
          target = view.absPosFromIndex(rndIndex)
        }
        target
      }
    }
    findRoute(origin, destination, view)
  }

}

abstract class SlaveBotControl(val name: String) extends BotControl {
  def timeLeft = Overmind.roundTime - bot.time
}

/**
 * Zergling bomb that explodes in shower of acid
 */
class Baneling(override val name: String) extends SlaveBotControl(name) {
  override protected def run = {
    bot.view.offsetToNearest(EnemyBot) match {
      case Some(enemyOffset) => if (enemyOffset.length > 10) {
        bot.move(findRoute(bot.position, bot.position + enemyOffset, bot.view).head)
      } else {
        bot.say("Nuuuuuuuuke!")
        bot.explode(enemyOffset.length.toInt.max(9))
      }
      case _ => bot.say("Tick, Tack!")
    }
  }
}

class Zergling(override val name: String, val startEnergy: Int, var route: Seq[XY]) extends SlaveBotControl(name) {

  private var energy = startEnergy

  def welcomeMessage = if (startEnergy == bot.energy) bot.status("For the sworm!")

  override protected def run = {

    welcomeMessage

    energy = bot.energy

    if (timeLeft < 50){
      route = homeRoute
      bot.move(route.head)
      route = route.tail
    } else {
      if (shouldAttack) {
        findNearestEnemy(bot.position, bot.view) match {
          case Some(enemyOffset) => attack(enemyOffset)
          case _ => harvest
        }
      } else {
        harvest
      }
    }

  }


  def shouldAttack = {
    val cells = surroundingArea(3).map(xy => bot.view.cellAtRelPos(xy))
    (cells.contains(EnemyBot) && energy < 1000) || (cells.count(c => c == Snorg) > 3 && energy < 300)
  }

  def attack(enemyOffset: XY) = {
    bot.explode(enemyOffset.length.toInt.max(6))
    bot.say("aaaaargh!")
  }

  def harvest = {
    if (route.isEmpty || collisionPrediction(bot.position, route.head, bot.view)) {
      log("zergling: routing...")
      try {
        val foodOption = findNearestFood(bot.position, bot.view)
        route = foodOption match {
          case Some(food) => {
            bot.status("eat")
            log("zergling: footroute")
            findRoute(bot.position, food, bot.view)
          }
          case _ => homeRoute
        }

      } catch {
        case ude: UnreachableDestinationException => {
          log(ude)
          route = List(XY.randomDirection)
        }
      }
    }

    log("zergling: route = " + route)
    bot.move(route.head)

    route = route.tail
  }

  def homeRoute: Seq[XY] = {
    try {
      bot.status("home")

      def bounded(x: Int) = if (x.abs > bot.view.size) {
        x.signum * (bot.view.size - 1)
      } else {
        x
      }

      //    val masterPos = botcontrol.offsetToMaster
      //    val gradient = masterPos.y.toFloat / masterPos.x.toFloat
      //    val boundedMasterPos = XY(bounded(masterPos.x), (gradient * botcontrol.view.size).toInt * masterPos.y.signum)
      //    //val boundedMasterPos = XY(bounded(masterPos.x), bounded(masterPos.y))
      //
      //    log("zergling: homeRoute (V = virtual master)")
      //    log("masteroffset = " + botcontrol.offsetToMaster)
      //    log("calculated master pos = " + boundedMasterPos)
      //    log(botcontrol.view.set(boundedMasterPos, 'V').toString)
      //    val path = findRoute(botcontrol.position, boundedMasterPos, botcontrol.view)
      //    path

      val offsetToMaster = bot.position + bot.offsetToMaster
      val boundedMasterPos = XY(bounded(offsetToMaster.x), bounded(offsetToMaster.y))
      log("zergling: homeRoute (V = virtual master)")
      log("masteroffset = " + bot.offsetToMaster)
      log("calculated master pos = " + boundedMasterPos)
      findRoute(bot.position, boundedMasterPos, bot.view)
    } catch {
      case ex: Throwable => List(XY.randomDirection)
    }
  }

  def distanceToMaster = bot.offsetToMaster.length

  /**
   * checks if master gets out of sight
   * @return
   */
  def shouldReturnHome = bot.energy > 900 || distanceToMaster + 2 > bot.view.centerToCornerDistance
}

object NoopBotControl extends BotControl {
  protected def run = {}
}

object HiveCluster {
  private var barracks: Map[String, SlaveBotControl] = Map()

  def clear = barracks = Map()

  def enslave(slave: SlaveBotControl) = barracks = barracks.updated(slave.name, slave)

  def getSlave(name: String) = barracks.get(name)

  def slaveCount = barracks.size

  def slaves = barracks.values
}

// -------------------------------------------------------------------------------------------------
// Framework
// -------------------------------------------------------------------------------------------------


class ControlFunctionFactory {

  Logger.ENABLED = false

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)

    opcode match {

      case "Welcome" => {
        val path = params("path")
        val round = params("round")
        Overmind.roundTime = params("apocalypse").toInt
        HiveCluster.clear
        if (Logger.ENABLED) Logger.configure(path, round)
      }

      case "Goodbye" => {
        if (Logger.ENABLED) Logger.finish()
      }

      case "React" => {

        val bot = BotImpl(params)

        Logger.log("Input: " + input)
        Logger.log(bot.view.toString);

        try {
          bot.generation match {
            case 0 => Overmind.control(bot)
            case _ => HiveCluster
              .getSlave(bot.inputOrElse("name", "unknown"))
              .getOrElse(NoopBotControl)
              .control(bot)
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

  def position: XY

  // outputs
  def move(delta: XY): Bot

  def say(text: String): Bot

  def status(text: String): Bot

  def spawn(offset: XY, params: (String, Any)*): Bot

  def spawn(offset: XY, name: String, energy: Int): Bot

  def set(params: (String, Any)*): Bot

  def log(text: String): Bot

  def markCell(position: XY, color: String = "#8888ff"): Bot

  def drawLine(from: XY, to: XY, color: String = "#8888ff"): Bot

  // ---------------------- minibot only -----------------
  def offsetToMaster: XY

  def explode(blastRadius: Int): Bot

  def isMiniBot: Boolean

  def isMasterBot: Boolean
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

  val position = view.center

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


  def spawn(offset: XY, name: String, energy: Int) = spawn(offset, ("name", name), ("energy", energy))

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

  def offsetToMaster = if (isMiniBot) inputAsXYOrElse("master", XY.Zero) else view.center

  def isMiniBot: Boolean = generation > 0

  def isMasterBot: Boolean = !isMiniBot
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

  def centerToCornerDistance = center.length.toInt

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

  import java.io.{PrintWriter, File, FileWriter}

  var ENABLED = true

  var logWriter = new PrintWriter(System.out)

  def configure(path: String, round: String, outputToFile: Boolean = false) {
    logWriter = outputToFile match {
      case true => new PrintWriter(new FileWriter(path + "/" + System.currentTimeMillis + "_round_" + round + ".log", false))
      case _ => new PrintWriter(System.out)
    }
  }

  def log(message: String) = if (ENABLED) logWriter.println(message)

  def log(error: Throwable) = if (ENABLED) error.printStackTrace(logWriter)

  def flush() = if (ENABLED) logWriter.flush()

  def finish() = if (ENABLED) {
    logWriter.flush()
    logWriter.close()
  }
}

