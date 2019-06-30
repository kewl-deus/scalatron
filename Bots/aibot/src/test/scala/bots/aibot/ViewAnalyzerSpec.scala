package bots.aibot

import bots.framework._
import org.specs2.mutable.Specification
import TestUtils._

class ViewAnalyzerSpec extends Specification with CellCodes {


  "View analysis" should {
    "discover all entities in all 8 directions" in {
      val view: View = viewFromMultiline(
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
                """)

      view.cells.length mustEqual (31 * 31)

      println(view.size)

      val botPos = XY(0, 0)
      view(botPos) mustEqual (MasterBot)

      val viewAnalyzer = new ViewAnalyzer(view)
      val viewAxes = viewAnalyzer.analyze

      //viewAxes.map(axis => axis.cells).map()
      /*
      val directions = Direction45.Right.to(Direction45.DownRight)
      val positions = directions.map(dir => XY.fromDirection45(dir))
      val pathsToEdges: Seq[Path] = positions.map(pos => Path(pos, 15))
      val pathCells = pathsToEdges.map(path => path.cells(view)).map(s => s.mkString(",")).toList
      */

      val pathCells = viewAxes.map(ax => (ax.direction45, ax.cells.map(_.code).mkString(","))).toMap

      pathCells(Direction45.UpLeft) mustEqual "0,a,b,c,d,e,f,g,h,i,j,k,l,m,n"
      pathCells(Direction45.Down) mustEqual "5,A,B,C,D,E,F,G,H,I,J,K,L,M,N"
      pathCells(Direction45.Right) mustEqual "3,_,_,_,_,_,_,_,_,_,_,_,_,_,_"

    }
  }
}