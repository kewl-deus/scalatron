package bots.aibot

import bots.aibot.TestUtils._
import bots.framework.{Direction45, View, XY}
import org.specs2.mutable.Specification
import bots.framework.CellCodes._

class DistanceWeightedRewardSpec extends Specification  {

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

      val obstacleCodes = List(OccludedCell, Wall, Zugar, Toxifera, Fluppet, Snorg)

      val viewAnalyzer = new ViewAnalyzer(obstacleCodes, EnvironmentInterpreters.distanceWeightedReward)
      val state = viewAnalyzer.getState(view)

      state.size mustEqual (Direction45.ALL.size * obstacleCodes.size)

    }
  }

}
