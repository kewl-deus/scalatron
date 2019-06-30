package bots.aibot

import bots.framework.{CellCodes, Direction45, View, XY}

class ViewAnalyzer(view: View) extends CellCodes {

  /** max amount of steps from bot position (0,0) to edge of view */
  val maxSteps = (view.size - 1) / 2

  /**
    * @return CellVector for each of the 45-degree directions
    */
  def analyze = {

    val positions = Direction45.ALL.map(dir => XY.fromDirection45(dir))

    val pathsToEdges: Seq[Path] = positions.map(pos => Path(pos, maxSteps))

    pathsToEdges.map(readCells).zipWithIndex.map{case (cells, direction) => ViewAxis(direction, cells)}
  }

  def readCells(path: Path) = path(view)
    .zip(path.positions)
    .map{case (cell, pos) => Cell(cell, pos)}

}
