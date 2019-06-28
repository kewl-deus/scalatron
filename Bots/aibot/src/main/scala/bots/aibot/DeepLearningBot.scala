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

  def react = {
    this.performReaction
    this.toString
  }

  private def performReaction = {
    //debugPrint(view)

    //new round?
    //if (this.time <= 1) agent.reset(this.energy)
    agent.update(this.time, this.energy)

    val currentState = getState
    val nextMove = calcNextMove(currentState)
    this.move(nextMove)

    //save state
    agent.remember(currentState, nextMove, this.energy)
  }

  private def calcNextMove(state: State) = {
    val epsilon = 80 - this.time

    if (Random.nextInt(200) < epsilon){
      //random move
      //val x = random.nextInt(3) - 1
      //val y = random.nextInt(3) - 1
      //this.move(XY(x, y))
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
      case Some(obstacle) => obstacleBitmap(obstacle, obstacleCodes) //relativeDistances(obstacle, obstacleCodes, maxSteps)
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
}
