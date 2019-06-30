package bots.aibot

import bots.framework.{CellCodes, Direction45, View, XY}
import org.specs2.mutable.Specification
import TestUtils._

class ShortestDistanceSpec extends Specification with CellCodes {

  "Obstacle distance converter" should {
    "return vector of distances to obstacle" in {
      val view: View = viewFromMultiline(
        """
                      a______________________________
                      _______________________________
                      __b____________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      ________a______________________
                      _______________________________
                      __________c____________________
                      ___________b___________________
                      ____________a__________________
                      _____________a_________________
                      _______________________________
                      _______________M_a_____a_______
                      _______________b_______________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________a_______________
                      _______________________________
                      _______________________________
                      _______________b_______________
                      _______________b_______________
                      _______________b_______________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                """)

      view.cells.length mustEqual (31 * 31)

      val botPos = XY(0, 0)
      view(botPos) mustEqual (MasterBot)

      val inputParams = Map("view" -> view.cells)
      val obstacleCodes = List('a', 'b', 'c')
      val networkModel =DRLModels.createNetwork(Direction45.ALL.size, obstacleCodes.size)
      val agent = new DRLAgent(networkModel, new DirectTransfer(40), new PredictionRewardAdjustmentDataConverter(1000))
      val bot = new DeepLearningBot(inputParams, obstacleCodes, EnvironmentInterpreters.shortestDistance, agent)

      val distances = bot.getState

      distances.size mustEqual (Direction45.ALL.size * obstacleCodes.size)

      val dirIndex = Direction45.UpLeft
      val obstacleIndex = obstacleCodes.size * dirIndex

      val stepsToObstacle = 2
      val maxSteps = (view.size - 1) / 2
      val valuableObstacleDistance = (maxSteps.doubleValue() - stepsToObstacle.doubleValue()) / maxSteps.doubleValue()
      distances(obstacleIndex) mustEqual valuableObstacleDistance

    }
  }
}
