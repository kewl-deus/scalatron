package routing

import framework.{View, XY, CellCodes}

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



