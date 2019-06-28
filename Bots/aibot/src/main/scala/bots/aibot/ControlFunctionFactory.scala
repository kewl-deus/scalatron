package bots.aibot

import bots.framework.CommandParser

class ControlFunctionFactory {

  lazy val Noop = ""

  val botBackend = DeepLearningBotBackend

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "React" =>
        new DeepLearningBot(params, botBackend).react
      case _ => Noop // OK
    }
  }
}


object DeepLearningBotBackend extends DQNAgent(Globals.directions.size, Globals.obstacleCodes.size)