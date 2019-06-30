package bots.aibot

import bots.aibot.EnvironmentInterpreters.ObstacleStateMapper
import bots.framework.{CommandParser, Direction45}
import ch.qos.logback.classic.Level
import org.slf4j.LoggerFactory
import bots.framework.CellCodes


class ControlFunctionFactory  {

  import DeepLearningBotConfig._

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "Welcome" => {
        Stats.welcome(params)
        new DeepLearningBot(params, obstacleCodes, envInterpreter, DeepLearningBotBackend).welcome
      }
      case "React" => new DeepLearningBot(params, obstacleCodes, envInterpreter, DeepLearningBotBackend).react
      case "Goodbye" => {
        Stats.goodbye(params)
        new DeepLearningBot(params, obstacleCodes, envInterpreter, DeepLearningBotBackend).goodbye
      }
      case _ => ""
    }
  }
}


object DeepLearningBotConfig extends CellCodes {
  val obstacleCodes = List(OccludedCell, Wall, Zugar, Toxifera, Fluppet, Snorg)
  val envInterpreter: ObstacleStateMapper = EnvironmentInterpreters.obstacleBitmap
}


object DeepLearningBotBackend extends DRLAgent(
  model = DRLModels.createNetwork(Direction45.ALL.size, DeepLearningBotConfig.obstacleCodes.size),
  replayMemoryManager = new DirectTransfer(20), //new ShortTermMemory(20),
  trainDataConverter = new PredictionRewardAdjustmentDataConverter(400), //new DirectRewardLastMoveDataConverter(1)
  collisionCost = 40) {

  //System.setProperty("org.slf4j.simpleLogger.log.org.deeplearning4j.scalnet.models.Sequential", "warn")

  LoggerFactory
    .getLogger("org.deeplearning4j.scalnet.models.Sequential")
    .asInstanceOf[ch.qos.logback.classic.Logger]
    .setLevel(Level.WARN)
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