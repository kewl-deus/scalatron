package bots.intercept

import bots.intercept.DataStructureUtils.State


/**
  * Functions for interpreting the observed environment
  */
object EnvironmentInterpreters {

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
    obstacleCodes.map(code => if (cells.exists(o => o.code == code)) 1d else 0d)
  }

  /**
    * Converts cells to vector with shortest distance to obstacle.
    * State vector length = obstacleCodes count.
    * State value represents most valuable distance to the first cell containing the given obstacle code.
    * Valuable distance = (maxSteps - stepCount) / maxSteps
    * with maxSteps = cellcount
    * --> this calculation makes short distances more valuable and make them relative to axis length
    *
    * @param cells
    * @param obstacleCodes
    * @return
    */
  def shortestDistance(cells: Seq[Cell], obstacleCodes: List[Char]): State = {
    val maxSteps = cells.size.doubleValue()
    obstacleCodes.map { code =>
      //shortest distance = find first cell with obstacleCode
      val firstObstacle = cells.find(ob => ob.code == code)
      firstObstacle match {
        case Some(obstacle) => (maxSteps - obstacle.position.stepCount.doubleValue()) / maxSteps
        case _ => 0d
      }
    }
  }

  def relativeDensity(cells: Seq[Cell], obstacleCodes: List[Char]): State = {
    val cellCount = cells.size.doubleValue()
    obstacleCodes.map { code =>
      val obstacleCells = cells.filter(ob => ob.code == code)
      obstacleCells.size.doubleValue() / cellCount.doubleValue()
    }
  }

}
