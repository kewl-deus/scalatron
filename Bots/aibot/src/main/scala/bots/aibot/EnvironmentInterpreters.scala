package bots.aibot

import bots.aibot.DataStructureUtils.State
import bots.framework.CellCodes

/**
  * Functions for interpreting the observed environment
  */
object EnvironmentInterpreters extends CellCodes {

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

  /**
    * Converts cells to vector with expected reward for closest obstacle
    * weighted by distance to it.
    *
    * @param cells
    * @param obstacleCodes
    * @return
    */
  def distanceWeightedReward(cells: Seq[Cell], obstacleCodes: List[Char]): State = {
    def calcReward(cellCode: Char, stepDistance: Int) = {
      cellCode match {
        case Fluppet => // good beast: valuable, but runs away
          if (stepDistance == 1) 600d
          else if (stepDistance == 2) 300d
          else (150d - stepDistance * 15).max(10)

        case Zugar => // good plant: less valuable, but does not run
          if (stepDistance == 1) 500d
          else if (stepDistance == 2) 300d
          else (150d - stepDistance * 10).max(10)

        case Snorg => // bad beast: dangerous, but only if very close
          if (stepDistance < 4) -400d / stepDistance else -50d / stepDistance

        case Toxifera => // bad plant: bad, but only if I step on it
          if (stepDistance < 2) -1000d else 0d

        case Wall => // wall: harmless, just don't walk into it
          if (stepDistance < 2) -1000d else 0d

        case OccludedCell => 1d

        case _ => 0d
      }
    }

    obstacleCodes.map ( code =>
      cells.find(ob => ob.code == code).map(cell => calcReward(cell.code, cell.position.stepCount)).getOrElse(0d)
    )
  }
}
