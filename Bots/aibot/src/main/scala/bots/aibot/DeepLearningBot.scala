package bots.aibot

import bots.framework._
import Globals.State

import scala.util.Random

/**
  * Bot Frontend: Input parsing and command chaining
  *
  * @param inputParams
  * @param agent
  */
class DeepLearningBot(inputParams: Map[String, String], obstacleCodes: List[Char], agent: DQNAgent) extends BotImpl(inputParams) {

  /**
    * Welcome(name=String,apocalypse=int,round=int,maxslaves=int)
    */
  def welcome: String = {
    val roundNo = inputAsIntOrElse("round", 0) + 1
    val maxStepCount = inputAsIntOrElse("apocalypse", 0)
    agent.newRound(roundNo, maxStepCount)
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

    val currentState = getState(relativeDistances)
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
      XY.fromDirection45(Random.nextInt(8))
    } else {
      //predicted move
      //this.say("Predicted move")
      agent.predictMove(state)
    }
  }


  /**
    *
    * @param obstacleCodes       chars defining obstacles
    * @param obstacleStateMapper converts obstacle to state
    * @return
    */
  def getState(obstacleStateMapper: (Seq[Cell], List[Char]) => State): State = {
    val viewAnalyzer = new ViewAnalyzer(view)
    val viewAxes = viewAnalyzer.analyze

    val obstacleMatrix = viewAxes.map(axis => axis.cells match {
      case cells: Seq[Cell] if (!cells.isEmpty) => obstacleStateMapper(cells, obstacleCodes)
      case _ => obstacleCodes.map(_ => 0d)
    })

    obstacleMatrix.flatten
  }

  /**
    * Converts cells into bitmap vector of obstacleCodes.
    * Bitmap vector length = obstacleCodes count.
    * If cell with obstacleCode exists, bitmap index for obstacleCode is marked with 1.
    *
    * @param cells
    * @param obstacleCodes
    * @return
    */
  def obstacleBitmap(cells: Seq[Cell], obstacleCodes: List[Char]): State = {
    obstacleCodes.map(code => if (cells.exists(o => o.cellCode == code)) 1d else 0d)
  }

  /**
    * Converts cells to vector with relative distances.
    * State vector length = obstacleCodes count.
    * State value represents the relative distance to the first cell containing the given obstacle code.
    * Relative distance is number of steps to cell in relation to length of view-axis (= number of cells)
    *
    * @param cells
    * @param obstacleCodes
    * @return
    */
  def relativeDistances(cells: Seq[Cell], obstacleCodes: List[Char]): State = {
    val maxSteps = cells.size.doubleValue()
    obstacleCodes.map{code =>
      val firstObstacle = cells.find(ob => ob.cellCode == code)
      firstObstacle match {
        case Some(obstacle) => obstacle.position.stepCount.doubleValue() / maxSteps
        case _ => 0d
      }
    }
  }

  def relativeDensity(cells: Seq[Cell], obstacleCodes: List[Char]): State = {
    val cellCount = cells.size.doubleValue()
    obstacleCodes.map{code =>
      val obstacleCells = cells.filter(ob => ob.cellCode == code)
      obstacleCells.size.doubleValue() / cellCount.doubleValue()
    }
  }

  //TODO relativeDensityWithWeightedDistance

  private def debugPrint(view: View): Unit = {
    println(view.cells.grouped(31).mkString("\n"))
  }

  def collision: Option[XY] = inputParams.get("collision").map(s => XY(s))

  def drawLine(from: XY, to: XY, color: String) = append(s"DrawLine(from=$from,to=$to,color=$color)")
}
