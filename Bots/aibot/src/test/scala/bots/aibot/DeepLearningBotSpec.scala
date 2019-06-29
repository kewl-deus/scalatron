package bots.aibot

import bots.aibot.Globals.State
import bots.framework._
import org.deeplearning4j.scalnet.models.Sequential
import org.specs2.mutable.Specification

import scala.collection.immutable

class DeepLearningBotSpec extends Specification with CellCodes {

  lazy val sampleView: View = View(
    """
                      ?________________??????????????
                      ??_______________??????????????
                      ???______________??????????????
                      ????_____________??????????????
                      ?????___________???????????????
                      _?WWWW__________???????????????
                      ___p____P_______???????????????
                      ________________????????????WW?
                      ________________WWWWWWWWWWWWWWW
                      ________________WWWWWWWWWWWWWWW
                      ____________________________WW?
                      ____________________________WW?
                      ____p_______________________WW?
                      ____________________________WW?
                      ____________________________WW?
                      _______________M____________WW?
                      ____________________________WW?
                      ____________________________WW?
                      ____________________________WW?
                      ____________________________WW?
                      ____________________P_______WW?
                      __P_________________________WW?
                      ____________________________WW?
                      ____________________________WW?
                      ____________________________WW?
                      ______________________________?
                      _______________________________
                      _______________________________
                      _______________________________
                      ________________WWWWWW_________
                      ________________?????W_________
                """.replaceAll("\\s", ""))

  lazy val emptyView: View = View(
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
                      _______________________________
                      _______________________________
                      _______________________________
                      _______________M_______________
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
                      _______________________________
                      _______________________________
                      _______________________________
                """.replaceAll("\\s", ""))

  "View analysis" should {
    "discover all entities in all 8 directions" in {
      val view: View = View(
        """
                      n______________________________
                      _m_____________________________
                      __l____________________________
                      ___k___________________________
                      ____j__________________________
                      _____i_________________________
                      ______h________________________
                      _______g_______________________
                      ________f______________________
                      _________e_____________________
                      __________d____________________
                      ___________c___________________
                      ____________b__________________
                      _____________a_________________
                      ______________012______________
                      ______________7M3______________
                      ______________654______________
                      _______________A_______________
                      _______________B_______________
                      _______________C_______________
                      _______________D_______________
                      _______________E_______________
                      _______________F_______________
                      _______________G_______________
                      _______________H_______________
                      _______________I_______________
                      _______________J_______________
                      _______________K_______________
                      _______________L_______________
                      _______________M_______________
                      _______________N_______________
                """.replaceAll("\\s", ""))

      view.cells.length mustEqual (31 * 31)

      val botPos = XY(0, 0)
      view(botPos) mustEqual (MasterBot)

      val directions = Direction45.Right.to(Direction45.DownRight)
      val positions = directions.map(dir => XY.fromDirection45(dir))
      //val positions = List(XY.UpLeft, XY.Up, XY.RightUp, XY.Right, XY.DownRight, XY.Down, XY.LeftDown, XY.Left)

      val pathsToEdges: Seq[Path] = positions.map(pos => Path(pos, 15))

      //pathsToEdges.foreach(println)
      val pathCells = pathsToEdges.map(path => path.cells(view)).map(s => s.mkString(",")).toList
      //pathCells.foreach(println)

      pathCells(Direction45.UpLeft) mustEqual "0,a,b,c,d,e,f,g,h,i,j,k,l,m,n"
      pathCells(Direction45.Down) mustEqual "5,A,B,C,D,E,F,G,H,I,J,K,L,M,N"
      pathCells(Direction45.Right) mustEqual "3,_,_,_,_,_,_,_,_,_,_,_,_,_,_"

    }
  }

  "Obstacle bitmap converter" should {
    "return a bitmap as state" in {
      val view: View = View(
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
                """.replaceAll("\\s", ""))

      view.cells.length mustEqual (31 * 31)

      val botPos = XY(0, 0)
      view(botPos) mustEqual (MasterBot)

      val inputParams = Map("view" -> view.cells)
      val obstacleCodes = List('a', 'b', 'c')
      val bot = new DeepLearningBot(inputParams, obstacleCodes, Strategies.obstacleBitmap, new DQNAgent(Globals.directions.size, obstacleCodes.size))

      val bitmap = bot.getState

      bitmap.size mustEqual (Globals.directions.size * obstacleCodes.size)

      val dirIndex = Direction45.UpLeft
      val obstacleIndex = obstacleCodes.size * dirIndex

      bitmap(obstacleIndex) mustEqual (1.0d)

    }
  }

  "Obstacle distance converter" should {
    "return vector of relative distances to obstacle" in {
      val view: View = View(
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
                """.replaceAll("\\s", ""))

      view.cells.length mustEqual (31 * 31)

      val botPos = XY(0, 0)
      view(botPos) mustEqual (MasterBot)

      val inputParams = Map("view" -> view.cells)
      val obstacleCodes = List('a', 'b', 'c')
      val bot = new DeepLearningBot(inputParams, obstacleCodes, Strategies.relativeDistances, new DQNAgent(Globals.directions.size, obstacleCodes.size))

      val relDistanceVector = bot.getState

      relDistanceVector.size mustEqual (Globals.directions.size * obstacleCodes.size)

      val dirIndex = Direction45.UpLeft
      val obstacleIndex = obstacleCodes.size * dirIndex

      val stepsToObstacle = 2
      relDistanceVector(obstacleIndex) mustEqual (stepsToObstacle.doubleValue() / Globals.maxSteps.doubleValue())

    }
  }

  "Obstacle density converter" should {
    "return vector of relative obstacle densities" in {
      val view: View = View(
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
                """.replaceAll("\\s", ""))

      view.cells.length mustEqual (31 * 31)

      val botPos = XY(0, 0)
      view(botPos) mustEqual (MasterBot)

      val inputParams = Map("view" -> view.cells)
      val obstacleCodes = List('a', 'b', 'c')
      val bot = new DeepLearningBot(inputParams, obstacleCodes, Strategies.relativeDensity, new DQNAgent(Globals.directions.size, obstacleCodes.size))

      val relDensityVector = bot.getState

      relDensityVector.size mustEqual (Globals.directions.size * obstacleCodes.size)

      val upleftA = obstacleCodes.size * Direction45.UpLeft
      val upleftB = obstacleCodes.size * Direction45.UpLeft + 1
      val upleftC = obstacleCodes.size * Direction45.UpLeft + 2

      relDensityVector(upleftA) mustEqual (4d / Globals.maxSteps.doubleValue())
      relDensityVector(upleftB) mustEqual (2d / Globals.maxSteps.doubleValue())
      relDensityVector(upleftC) mustEqual (1d / Globals.maxSteps.doubleValue())

    }
  }
}