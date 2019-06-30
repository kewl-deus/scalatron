package bots.aibot

import bots.aibot.TestUtils._
import bots.framework.CellCodes._
import bots.framework.{Direction45, View, XY}
import org.specs2.mutable.Specification

class ShortestDistanceSpec extends Specification {

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

      val obstacleCodes = List('a', 'b', 'c')

      val viewAnalyzer = new ViewAnalyzer(obstacleCodes, EnvironmentInterpreters.shortestDistance)
      val distances = viewAnalyzer.getState(view)


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
