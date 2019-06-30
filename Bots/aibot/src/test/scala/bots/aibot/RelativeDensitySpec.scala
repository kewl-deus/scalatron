package bots.aibot

import bots.framework.{CellCodes, Direction45, View, XY}
import org.specs2.mutable.Specification
import TestUtils._

class RelativeDensitySpec extends Specification with CellCodes{


  "Obstacle density converter" should {
    "return vector of relative obstacle densities" in {
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
      val agent = new DRLAgent(networkModel, new DirectTransfer(40), new SimpleTrainDataConverter(1000))
      val bot = new DeepLearningBot(inputParams, obstacleCodes, EnvironmentInterpreters.relativeDensity, agent)

      val relDensityVector = bot.getState

      relDensityVector.size mustEqual (Direction45.ALL.size * obstacleCodes.size)

      val upleftA = obstacleCodes.size * Direction45.UpLeft
      val upleftB = obstacleCodes.size * Direction45.UpLeft + 1
      val upleftC = obstacleCodes.size * Direction45.UpLeft + 2

      val maxSteps = (view.size - 1) / 2
      relDensityVector(upleftA) mustEqual (4d / maxSteps.doubleValue())
      relDensityVector(upleftB) mustEqual (2d / maxSteps.doubleValue())
      relDensityVector(upleftC) mustEqual (1d / maxSteps.doubleValue())

    }
  }
}
