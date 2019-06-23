package scalatron.botwar.botPlugin

import framework._
import botcontrol.{SlaveBotRegistry, TracingMiniBot, MasterBotControl, NoopBotControl}

class ControlFunctionFactory {

  Logger.ENABLED = false

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)

    opcode match {

      case "Welcome" => {
        val path = params("path")
        val round = params("round")
        if (Logger.ENABLED) Logger.configure(path, round)
        MasterBotControl.init
      }

      case "Goodbye" => {
        if (Logger.ENABLED) Logger.finish()
      }

      case "React" => {

        val bot = BotImpl(params)

        Logger.log("Input: " + input)
        Logger.log(bot.view.toString);

        try {
          bot.generation match {
            case 0 => MasterBotControl.control(bot)
            case _ => SlaveBotRegistry
              .get(bot.inputOrElse("name", "unknown"))
              .getOrElse(NoopBotControl)
              .control(bot)
          }
        }
        catch {
          case t: Throwable => {
            t.printStackTrace
            Logger.log(t)
            bot.status("Error!")
            bot.log("Error!")
          }
        }


        val response = bot.toString
        Logger.log("Response: " + response)
        response
      }

      case _ => "" // OK
    }
  }
}