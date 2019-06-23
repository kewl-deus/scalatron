package bots.kewlbot.mode

import bots.kewlbot.kewlbot._
import bots.kewlbot.util.ViewSpace

abstract class MasterBot extends BotMode {
  def react(view: ViewSpace, params: CommandParameters): ModeTransition

}
