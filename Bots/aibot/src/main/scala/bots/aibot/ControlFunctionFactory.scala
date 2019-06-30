package bots.aibot

import bots.framework.CellCodes._
import bots.framework.{CommandParser, Direction45}
import ch.qos.logback.classic.Level
import org.slf4j.LoggerFactory


class ControlFunctionFactory {

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "Welcome" => {
        Stats.welcome(params)
        new DeepLearningBot(params, DeepLearningBotViewAnalyzer, DeepLearningBotBackend).welcome
      }
      case "React" => new DeepLearningBot(params, DeepLearningBotViewAnalyzer, DeepLearningBotBackend).react
      case "Goodbye" => {
        Stats.goodbye(params)
        new DeepLearningBot(params, DeepLearningBotViewAnalyzer, DeepLearningBotBackend).goodbye
      }
      case _ => ""
    }
  }
}


object DeepLearningBotViewAnalyzer extends ViewAnalyzer(
  obstacleCodes = List(OccludedCell, Wall, Zugar, Toxifera, Fluppet, Snorg),
  envInterpreter = EnvironmentInterpreters.obstacleBitmap
)


object DeepLearningBotBackend extends DRLAgent(
  model = DRLModels.createCustomNetwork(Direction45.ALL.size, DeepLearningBotViewAnalyzer.obstacleCodes.size),
  replayMemoryManager = new DirectTransfer(20), // new ShortTermMemory(20)
  trainDataConverter = new PredictionRewardAdjustmentDataConverter(200), //new DirectRewardLastMoveDataConverter(1), //new MarkovTrainDataConverter(500),
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