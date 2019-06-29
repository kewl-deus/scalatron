package bots.aibot

import bots.framework.{CellCodes, View, XY}

class ViewAnalyzer(view: View) extends CellCodes {

  /**
    * @return CellVector for each of the 45-degree directions
    */
  def analyze = {

    val positions = Globals.directions.map(dir => XY.fromDirection45(dir))

    val pathsToEdges: Seq[Path] = positions.map(pos => Path(pos, Globals.maxSteps))

    pathsToEdges.map(readCells).zipWithIndex.map{case (cells, direction) => CellVector(direction, cells)}
  }

  def readCells(path: Path) = path(view)
    .zip(path.positions)
    .map{case (cell, pos) => Cell(cell, pos)}

}
