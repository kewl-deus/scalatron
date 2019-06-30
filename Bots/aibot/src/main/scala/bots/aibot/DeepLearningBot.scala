package bots.aibot

import bots.framework._
import DataStructureUtils.State
import bots.aibot.EnvironmentInterpreters.ObstacleStateMapper

import scala.util.Random

/**
  * Bot Frontend: Input parsing and command chaining
  *
  * @param inputParams
  * @param agent
  */
class DeepLearningBot(inputParams: Map[String, String], obstacleCodes: List[Char], envInterpreter: ObstacleStateMapper, agent: DRLAgent) extends BotImpl(inputParams) {

  val Noop = ""

  /**
    * Welcome(name=String,apocalypse=int,round=int,maxslaves=int)
    */
  def welcome: String = {
    val roundNo = inputAsIntOrElse("round", 0) + 1
    val maxStepCount = inputAsIntOrElse("apocalypse", 0)
    agent.newRound(roundNo, maxStepCount)
    Noop
  }

  /**
    * Goodbye(energy=int)
    */
  def goodbye: String = {
    //val finalEnergy = inputAsIntOrElse("energy", 0)
    agent.endRound
    Noop
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

  def getState: State = {
    val viewAnalyzer = new ViewAnalyzer(view)
    val viewAxes = viewAnalyzer.analyze

    val obstacleMatrix = viewAxes.map(axis => axis.cells match {
      case cells: Seq[Cell] if (!cells.isEmpty) => this.envInterpreter(cells, obstacleCodes)
      case _ => obstacleCodes.map(_ => 0d)
    })

    obstacleMatrix.flatten
  }


  private def debugPrint(view: View): Unit = {
    println(view.cells.grouped(31).mkString("\n"))
  }

  def collision: Option[XY] = inputParams.get("collision").map(s => XY(s))

  def drawLine(from: XY, to: XY, color: String) = append(s"DrawLine(from=$from,to=$to,color=$color)")
}
