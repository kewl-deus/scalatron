package routing

import framework.XY

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