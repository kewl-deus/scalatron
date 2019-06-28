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

  private def performReaction: Unit = {
    debugPrint(view)

    val currentState = getState

    val epsilon = 80 - this.time
    val random = Random

    if (random.nextInt(200) < epsilon){
      //random move
      //val x = random.nextInt(3) - 1
      //val y = random.nextInt(3) - 1
      //this.move(XY(x, y))
      val randomDirection = XY.fromDirection45(random.nextInt(8))
      this.move(randomDirection)

    } else {
      //predicted move
      this.agent.predictMove(currentState)
    }

    /*
    if randint(0, 200) < agent.epsilon:
                final_move = to_categorical(randint(0, 2), num_classes=3)
            else:
                # predict action based on the old state
                prediction = agent.model.predict(state_old.reshape((1,11)))
                final_move = to_categorical(np.argmax(prediction[0]), num_classes=3)

            #perform new move and get new state
            player1.do_move(final_move, player1.x, player1.y, game, food1, agent)
            state_new = agent.get_state(game, player1, food1)

            #set treward for the new state
            reward = agent.set_reward(player1, game.crash)

            #train short memory base on the new action and state
            agent.train_short_memory(state_old, final_move, reward, state_new, game.crash)

            # store the new data into a long term memory
            agent.remember(state_old, final_move, reward, state_new, game.crash)
     */


  }

  private def getState = {
    val viewAnalyzer = new ViewAnalyzer(view)
    val obstacleSuspicions = viewAnalyzer.analyze

    val relDistVectors: Seq[List[Int]] = obstacleSuspicions.map(obs => obs.obstacle match {
      case Some(obstacle) => relativeDistances(obstacle, obstacleCodes, maxSteps)
      case _ => obstacleCodes.map(_ => 0)
    })

    relDistVectors.flatten
  }

  private def relativeDistances(obstacle: Obstacle, cellCodes: List[Char], maxSteps: Int) = {
    cellCodes.map(code => if (obstacle.cell == code) obstacle.position.stepCount / maxSteps else 0)
  }

  private def debugPrint(view: View): Unit = {
    println(view.cells.grouped(31).mkString("\n"))
  }
}
