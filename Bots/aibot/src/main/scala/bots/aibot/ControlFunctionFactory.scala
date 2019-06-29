package bots.aibot

import bots.aibot.Strategies.ObstacleStateMapper
import bots.framework.{BotImpl, CommandParser}
import ch.qos.logback.classic.Level
import org.slf4j.LoggerFactory

class ControlFunctionFactory {

  val botBackend = DeepLearningBotBackend
  val strategy: ObstacleStateMapper = Strategies.relativeDensity

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "Welcome" => {
        Stats.welcome(params)
        new DeepLearningBot(params, Globals.obstacleCodes, strategy, botBackend).welcome
      }
      case "React" => new DeepLearningBot(params, Globals.obstacleCodes, strategy, botBackend).react
      case "Goodbye" => {
        Stats.goodbye(params)
        new DeepLearningBot(params, Globals.obstacleCodes, strategy, botBackend).goodbye
      }
      case _ => Globals.Noop
    }
  }
}

object Stats {
  private var roundNo = 0
  private var roundBegin = 0L
  private var scoreRecord = Int.MinValue

  def inputAsIntOrElse(inputParams: Map[String, String], key: String, fallback: Int) = inputParams.get(key).map(_.toInt).getOrElse(fallback)

  def welcome(params: Map[String, String]) = {
    roundNo = inputAsIntOrElse(params, "round", 0) + 1
    roundBegin = System.currentTimeMillis
  }

  def goodbye(params: Map[String, String]) = {
    val finalEnergy = inputAsIntOrElse(params, "energy", 0)
    if (finalEnergy > scoreRecord) scoreRecord = finalEnergy

    val elapsedTime = (System.currentTimeMillis - roundBegin) / 1000

    println(s"Round($roundNo,${elapsedTime}s) Final energy: $finalEnergy | Record: $scoreRecord")
  }


}


object DeepLearningBotBackend extends DQNAgent(Globals.directions.size, Globals.obstacleCodes.size) {
  //System.setProperty("org.slf4j.simpleLogger.log.org.deeplearning4j.scalnet.models.Sequential", "warn")

  LoggerFactory
    .getLogger("org.deeplearning4j.scalnet.models.Sequential")
    .asInstanceOf[ch.qos.logback.classic.Logger]
    .setLevel(Level.WARN)
}