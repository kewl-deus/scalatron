package bots.intercept

import bots.reference.{CommandParser, XY}
import org.specs2.mutable.Specification

class CommandMapSpec extends Specification {

  "Welcome command string" should {
    "be parsed into Command" in {

      val cmdString = "Welcome(name=BotName,apocalypse=5000,round=42,maxslaves=100)"

      val cmdMap = CommandMap(cmdString)

      val cmd = cmdMap("Welcome")

      cmd.opcode mustEqual "Welcome"
      cmd.paramAsInt("apocalypse").get mustEqual 5000
      cmd.paramAsInt("round").get mustEqual 42
      cmd.paramAsInt("maxslaves").get mustEqual 100
    }
  }

  //val reactCmd = "React(generation=0,time=14,view=____________???????????????????____________???????????????????____________???????????????????_____________??????????????????__P__________??????????????????_____________????????????????W?_____________??????????????_WW?_____________?????????????__WW_______________???????????____________________??????????_____________________?W??????_______________________?W?????________________________?W????P_________________________WWW?__________________________________________________________M_____p___________b________________________________________________________________________________________________P______________________________________________________________________________________________________________________________________________________________________________________________________________________________________________WW_____________________________WW?____________________________WW?__B_____________p___________WW?____________________B_p_____WW?,energy=1100,slaves=0,name=ReferenceBot,lastDirection=1)"

  "Multiple commands" should {
    "be parse into CommandMap" in {
      val multiCmdStr = "Move(direction=1:-1)|Set(lastDirection=1)"

      val cmdMap = CommandMap(multiCmdStr)

      cmdMap.contains("Move") must beTrue

      cmdMap("Move").paramAsXY("direction").get mustEqual XY(1, -1)
    }
  }

  "Empty string" should {
    "be parsed as empty CommandMap" in {
      val cmdMap = CommandMap("")

      cmdMap.isEmpty must beTrue
    }
  }
}
