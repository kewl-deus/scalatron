package bots.aibot

import bots.aibot.DataStructureUtils.State
import bots.framework._

import scala.util.Random

/**
  * Bot Frontend: Input parsing and command chaining
  */
class DeepLearningBot(inputParams: Map[String, String], viewAnalyzer: ViewAnalyzer, agent: DRLAgent) extends BotImpl(inputParams) {

  /**
    * Welcome(name=String,apocalypse=int,round=int,maxslaves=int)
    */
  def welcome: String = {
    val roundNo = inputAsIntOrElse("round", 0) + 1
    val maxStepCount = inputAsIntOrElse("apocalypse", 0)
    agent.newRound(roundNo, maxStepCount)
    toString
  }

  /**
    * Goodbye(energy=int)
    */
  def goodbye: String = {
    //val finalEnergy = inputAsIntOrElse("energy", 0)
    agent.endRound
    toString
  }


  /**
    * React(generation=int,name=string,time=int,view=string,energy=string,master=int:int,collision=int:int,slaves=int)
    */
  def react: String = {
    this.performReaction
    this.toString
  }

  private def performReaction = {
    //debugPrint(view)

    val currentState = viewAnalyzer.getState(view)
    val nextMove = calcNextMove(currentState)
    this.move(nextMove)

    val collisionDetected = this.collision
    collisionDetected.foreach(_ => this.say("Bonk!"))

    //save state
    agent.remember(currentState, nextMove, this.time, this.energy, collisionDetected)
  }

  private def calcNextMove(state: State) = {
    val epsilon = 80 - this.time

    //optimize: check for collision
    //repeat until nextMove != last collision Move

    if (Random.nextInt(200) < epsilon) {
      //random move
      this.say("Random move")
      XY.fromDirection45(Random.nextInt(Direction45.ALL.size))
    } else {
      //predicted move
      //this.say("Predicted move")
      agent.predictMove(state)
    }
  }


  private def debugPrint(view: View): Unit = {
    println(view.cells.grouped(31).mkString("\n"))
  }

  def collision: Option[XY] = inputParams.get("collision").map(s => XY(s))

  def drawLine(from: XY, to: XY, color: String) = append(s"DrawLine(from=$from,to=$to,color=$color)")
}
