package bots.aibot

import bots.framework._
import Globals._

import scala.util.Random

/**
  * Bot Frontend: Input parsing and command chaining
  *
  * @param inputParams
  * @param agent
  */
class DeepLearningBot(inputParams: Map[String, String], val agent: DQNAgent) extends BotImpl(inputParams) {

  /**
    * Welcome(name=String,apocalypse=int,round=int,maxslaves=int)
    */
  def welcome: String = {
    val round = inputAsIntOrElse("round", 0)
    val maxStepCount = inputAsIntOrElse("apocalypse", 0)
    agent.newRound(round, maxStepCount)
    Globals.Noop
  }

  /**
    * Goodbye(energy=int)
    */
  def goodbye: String = {
    val finalEnergy = inputAsIntOrElse("energy", 0)
    agent.endRound
    Globals.Noop
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

    val currentState = getState
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

    if (Random.nextInt(200) < epsilon){
      //random move
      this.say("Random move")
      XY.fromDirection45(Random.nextInt(8))
    } else {
      //predicted move
      //this.say("Predicted move")
      agent.predictMove(state)
    }
  }

  private def getState: State = {
    val viewAnalyzer = new ViewAnalyzer(view)
    val obstacleSuspicions = viewAnalyzer.analyze

    val obstacleMatrix = obstacleSuspicions.map(obs => obs.obstacle match {
      case Some(obstacle) => relativeDistances(obstacle, obstacleCodes, maxSteps) //obstacleBitmap(obstacle, obstacleCodes)
      case _ => obstacleCodes.map(_ => 0.doubleValue())
    })

    obstacleMatrix.flatten
  }

  private def obstacleBitmap(obstacle: Obstacle, cellCodes: List[Char]) = {
    cellCodes.map(code => if (obstacle.cell == code) 1 else 0).map(_.doubleValue())
  }

  private def relativeDistances(obstacle: Obstacle, cellCodes: List[Char], maxSteps: Int) = {
    cellCodes.map(code => if (obstacle.cell == code) obstacle.position.stepCount.doubleValue() / maxSteps.doubleValue() else 0.doubleValue())
  }

  private def debugPrint(view: View): Unit = {
    println(view.cells.grouped(31).mkString("\n"))
  }

  def collision: Option[XY] = inputParams.get("collision").map(s => XY(s))

  def drawLine(from: XY,to: XY,color: String) = append(s"DrawLine(from=$from,to=$to,color=$color)")
}
