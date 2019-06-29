package bots.aibot

import bots.framework.{CellCodes, Direction45, View, XY}

class ViewAnalyzer(view: View) extends CellCodes {


  def analyze = {

    val positions = Globals.directions.map(dir => XY.fromDirection45(dir))

    val pathsToEdges: Seq[Path] = positions.map(pos => Path(pos, 15))

    pathsToEdges.map(findFirstObstacle).zipWithIndex.map{case (optObs, direction) => ObstacleSuspicion(direction, optObs)}
  }

  def findFirstObstacle(path: Path) = path(view).zip(path.positions)
    .map{case (cell, pos) => Obstacle(cell, pos)}
    .find(ob => ob.cell != EmptyCell)

}

case class Obstacle(cell: Char, position: XY)

case class ObstacleSuspicion(direction45: Int, obstacle: Option[Obstacle]) {

  override def toString: String = {
    val dirName = Globals.nameDirection45(direction45)
    s"ObstacleSuspicion($dirName, $obstacle)"
  }
}