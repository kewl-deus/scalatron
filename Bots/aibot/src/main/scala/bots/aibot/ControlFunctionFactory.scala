package bots.aibot

import bots.framework.{BotImpl, CommandParser}
import ch.qos.logback.classic.Level
import org.slf4j.LoggerFactory

class ControlFunctionFactory {

  val botBackend = DeepLearningBotBackend

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "Welcome" => {
        Stats.welcome(params)
        new DeepLearningBot(params, botBackend).welcome
      }
      case "React" => new DeepLearningBot(params, botBackend).react
      case "Goodbye" => {
        Stats.goodbye(params)
        new DeepLearningBot(params, botBackend).goodbye
      }
      case _ => Globals.Noop
    }
  }
}

object Stats {
  private var roundNo = 0
  private var scoreRecord = Int.MinValue

  def inputAsIntOrElse(inputParams: Map[String, String], key: String, fallback: Int) = inputParams.get(key).map(_.toInt).getOrElse(fallback)

  def welcome(params: Map[String, String]) = {
    roundNo = inputAsIntOrElse(params, "round", 0)
  }

  def goodbye(params: Map[String, String]) = {
    val finalEnergy = inputAsIntOrElse(params, "energy", 0)
    if (finalEnergy > scoreRecord) scoreRecord = finalEnergy

    println(s"Round($roundNo) Final energy: $finalEnergy | Record: $scoreRecord")
  }


}


object DeepLearningBotBackend extends DQNAgent(Globals.directions.size, Globals.obstacleCodes.size) {
  //System.setProperty("org.slf4j.simpleLogger.log.org.deeplearning4j.scalnet.models.Sequential", "warn")

  LoggerFactory
    .getLogger("org.deeplearning4j.scalnet.models.Sequential")
    .asInstanceOf[ch.qos.logback.classic.Logger]
    .setLevel(Level.WARN)
}