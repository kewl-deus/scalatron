package bots.aibot

import bots.framework.CommandParser

class ControlFunctionFactory {

  lazy val Noop = ""

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "React" =>
        new DeepLearningBot(params).react
      case _ => Noop // OK
    }
  }
}