package bots.aibot

import bots.framework.CommandParser
import ch.qos.logback.classic.Level
import org.slf4j.LoggerFactory

class ControlFunctionFactory {

  val botBackend = DeepLearningBotBackend

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "React" => new DeepLearningBot(params, botBackend).react
      case "Goodbye" => new GoodbyeBot(params.get("energy").map(_.toInt).getOrElse(0)).goodbye
      case _ => Globals.Noop
    }
  }
}

class GoodbyeBot(energy: Int){
//TODO save roundNo & record score

  def goodbye: String = {
    println(s"Final energy: $energy")
    "Goodbye"
  }


}


object DeepLearningBotBackend extends DQNAgent(Globals.directions.size, Globals.obstacleCodes.size){
  //System.setProperty("org.slf4j.simpleLogger.log.org.deeplearning4j.scalnet.models.Sequential", "warn")

  LoggerFactory
    .getLogger("org.deeplearning4j.scalnet.models.Sequential")
    .asInstanceOf[ch.qos.logback.classic.Logger]
    .setLevel(Level.WARN)
}