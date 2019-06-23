package bots.kewlbot

import bots.kewlbot.cmd.{BotInstruction, Say, Status}
import bots.kewlbot.kewlbot.CommandParameters
import bots.kewlbot.mode.{MasterBot, StrategyMode}
import bots.kewlbot.util.{CommandParser, ViewSpace, _}


class Bot{

  var masterBot: MasterBot = StrategyMode

  def respond(input: String): String = {

    Logger.ENABLED = false

    val (opcode, params) = CommandParser(input)

    val instruction = opcode match {

      case "Welcome" => {
        val path = params("path")
        val round = params("round")
        if (Logger.ENABLED) Logger.configure(path, round)

        Say("time to kick ass and chew bubble gum")
      }

      case "Goodbye" => {
        if (Logger.ENABLED) Logger.finish()
        Say("tonight we dine in hell")
      }

      case "React" => {
        Logger.log("Input: " + input)
        Logger.log(SquareView(params("view")).toString);

        val stepIndex = params("time").toInt
        if (stepIndex % 10 == 0) Logger.flush()

        try {
          params("generation").toInt match {
            case 0 => reactMaster(params)
            case _ => reactSlave(params)
          }
        } catch {
          case error: Throwable => {
            Logger.log(error)
            Status("Error!")
          }
        }
      }
    }

    val response = instruction.toString
    Logger.log("Response: " + response)
    response
  }

  def reactMaster(params: CommandParameters): BotInstruction = {
    val view = ViewSpace(params("view"))
    val transition = masterBot.react(view, params)
    Logger.log("Mode: " + masterBot.getClass.getSimpleName)
    masterBot = transition.nextMode
    transition.instruction
  }


  def reactSlave(params: CommandParameters): BotInstruction = {
    val view = ViewSpace(params("view"))
    val slaveName = params("name")
    val slave = SlaveBotRegistry.getBot(slaveName)
    slave.isDefined match {
      case true => slave.get.react(view, params)
      case _ => {
        Logger.log("SlaveBot %s not found".format(slaveName))
        Status("SlaveBot %s not found".format(slaveName))
      }
    }

  }
}

