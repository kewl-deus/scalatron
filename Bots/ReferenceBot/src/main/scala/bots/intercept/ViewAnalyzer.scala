package bots.intercept

import bots.intercept.DataStructureUtils.State
import bots.intercept.EnvironmentInterpreters.ObstacleStateMapper
import bots.reference.{Direction45, View, XY}

class ViewAnalyzer(val obstacleCodes: List[Char], val envInterpreter: ObstacleStateMapper) {

  private val positions = Direction45.Right.to(Direction45.DownRight).map(dir => XY.fromDirection45(dir))


  /**
    *
    * @param view
    * @return max amount of steps from bot position (0,0) to edge of view
    */
  def getMaxSteps(view: View) = (view.size - 1) / 2

  /**
    * @param view
    * @return ViewAxis for each of the 45-degree directions
    */
  def getAxes(view: View) = {

    val maxSteps = getMaxSteps(view)
    val pathsToEdges: Seq[Path] = positions.map(pos => Path(pos, maxSteps))

    pathsToEdges
      .map(p => readCells(p, view))
      .zipWithIndex.map { case (cells, direction) => ViewAxis(direction, cells) }
  }

  def getState(view: View): State = {
    val viewAxes = getAxes(view)

    val obstacleMatrix = viewAxes.map(axis => axis.cells match {
      case cells: Seq[Cell] if (!cells.isEmpty) => this.envInterpreter(cells, obstacleCodes)
      case _ => obstacleCodes.map(_ => 0d)
    })

    obstacleMatrix.flatten
  }

  def readCells(path: Path, view: View) = path(view)
    .zip(path.positions)
    .map { case (cell, pos) => Cell(cell, pos) }

}
