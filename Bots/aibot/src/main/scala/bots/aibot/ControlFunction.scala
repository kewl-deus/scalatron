package bots.aibot

import bots.framework._


class DeepLearningBot(inputParams: Map[String, String]) extends BotImpl(inputParams){

  def react = {
    this.buildReaction
    this.toString
  }

  private def buildReaction {
    // demo: log the view of the master bot into the debug output (if running in the browser sandbox)
    this.log(this.view.cells.grouped(31).mkString("\n"))

    this.say(this.energy.toString)
  }

}
