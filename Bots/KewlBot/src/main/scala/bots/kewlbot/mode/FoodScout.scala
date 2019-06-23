package bots.kewlbot.mode

import bots.kewlbot.kewlbot._
import bots.kewlbot.cmd.Status
import bots.kewlbot.util.ViewSpace

/**
 * Scans area for food
 */
case class FoodScout(name: String) extends SlaveBot{
  def react(view: ViewSpace, params: CommandParameters) = {
    Status("FoodScout is alive")
  }
}
