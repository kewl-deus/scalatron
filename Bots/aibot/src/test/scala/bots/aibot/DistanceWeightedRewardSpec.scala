package bots.aibot

import bots.framework.{CellCodes, Direction45, View, XY}
import org.specs2.mutable.Specification
import TestUtils._

class DistanceWeightedRewardSpec extends Specification with CellCodes {

  "DistanceWeightedReward converter" should {
    "return a state vector with rewards" in {
      val view: View = viewFromMultiline(
        """
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
                      ____________?__________________
                      _____________W___p_____________
                      _______________PB______________
                      _______???WWWWWMp_B____B__W____
                      ______________p_b______________
                      _______________b_______________
                      ____________b__________________
                      ___________?___________________
                      _______________________________
                      _______________B_______________
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________________________
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
      val obstacleCodes = List(OccludedCell, Wall, Zugar, Toxifera, Fluppet, Snorg)
      val networkModel =DRLModels.createNetwork(Direction45.ALL.size, obstacleCodes.size)
      val agent = new DRLAgent(networkModel, new DirectTransfer(40), new SimpleTrainDataConverter(1000))
      val bot = new DeepLearningBot(inputParams, obstacleCodes, EnvironmentInterpreters.distanceWeightedReward, agent)

      val state = bot.getState

      state.size mustEqual (Direction45.ALL.size * obstacleCodes.size)

    }
  }

}
