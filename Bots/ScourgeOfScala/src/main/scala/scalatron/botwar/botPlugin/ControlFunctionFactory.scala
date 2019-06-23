package scalatron.botwar.botPlugin

import framework.{BotImpl, Logger, CommandParser}
import botcontrol.{NoopBotControl, Slavery, ScourgeMaster}


class ControlFunctionFactory {

  Logger.ENABLED = false

  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)

    opcode match {

      case "Welcome" => {
        val path = params("path")
        val round = params("round")
        ScourgeMaster.roundTime = params("apocalypse").toInt
        Slavery.clear
        if (Logger.ENABLED) Logger.configure(path, round)
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
            case 0 => ScourgeMaster.control(bot)
            case _ => Slavery
              .getSlave(bot.inputOrElse("name", "unknown"))
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