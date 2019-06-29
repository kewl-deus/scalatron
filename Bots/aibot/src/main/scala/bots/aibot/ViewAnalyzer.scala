package bots.aibot

import bots.framework.{CellCodes, View, XY}

class ViewAnalyzer(view: View) extends CellCodes {

  def analyze = {

    val positions = Globals.directions.map(dir => XY.fromDirection45(dir))

    val pathsToEdges: Seq[Path] = positions.map(pos => Path(pos, 15))

    pathsToEdges.map(findFirstObstacle).zipWithIndex.map{case (optObs, direction) => ObstacleSuspicion(direction, optObs)}
  }

  def listObstacles(path: Path) = path(view)
    .zip(path.positions)
    .map{case (cell, pos) => Obstacle(cell, pos)}

  def findFirstObstacle(path: Path) = listObstacles(path).find(ob => ob.cell != EmptyCell).toSeq

  def filterObstacles(path: Path) = listObstacles(path).filter(ob => ob.cell != EmptyCell)
}
