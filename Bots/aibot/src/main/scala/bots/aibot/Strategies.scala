package bots.aibot

import bots.aibot.Globals.State

object Strategies {

  type ObstacleStateMapper = (Seq[Cell], List[Char]) => State

  /**
    * Converts cells into bitmap vector of obstacleCodes.
    * Bitmap vector length = obstacleCodes count.
    * If cell with obstacleCode exists, bitmap index for obstacleCode is marked with 1.
    *
    * @param cells
    * @param obstacleCodes
    * @return
    */
  def obstacleBitmap(cells: Seq[Cell], obstacleCodes: List[Char]): State = {
    obstacleCodes.map(code => if (cells.exists(o => o.cellCode == code)) 1d else 0d)
  }

  /**
    * Converts cells to vector with relative distances.
    * State vector length = obstacleCodes count.
    * State value represents the relative distance to the first cell containing the given obstacle code.
    * Relative distance is number of steps to cell in relation to length of view-axis (= number of cells)
    *
    * @param cells
    * @param obstacleCodes
    * @return
    */
  def relativeDistances(cells: Seq[Cell], obstacleCodes: List[Char]): State = {
    val maxSteps = cells.size.doubleValue()
    obstacleCodes.map{code =>
      val firstObstacle = cells.find(ob => ob.cellCode == code)
      firstObstacle match {
        case Some(obstacle) => obstacle.position.stepCount.doubleValue() / maxSteps
        case _ => 0d
      }
    }
  }

  def relativeDensity(cells: Seq[Cell], obstacleCodes: List[Char]): State = {
    val cellCount = cells.size.doubleValue()
    obstacleCodes.map{code =>
      val obstacleCells = cells.filter(ob => ob.cellCode == code)
      obstacleCells.size.doubleValue() / cellCount.doubleValue()
    }
  }

  //TODO relativeDensityWithWeightedDistance
}
