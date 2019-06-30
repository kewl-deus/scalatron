package bots.aibot

import java.io.File

import bots.aibot.DataStructureUtils.{State, reshape}
import bots.framework.XY
import org.deeplearning4j.datasets.iterator.impl.ListDataSetIterator
import org.deeplearning4j.optimize.listeners.PerformanceListener
import org.deeplearning4j.scalnet.models.Model
import org.nd4j.linalg.dataset.DataSet

import scala.collection.mutable
import scala.util.Random

/**
  * Deep Reinforced Learning Agent (= Bot backend)
  * @param model neural network
  * @param replayMemoryManager strategy for filling replay memory
  * @param trainDataConverter converter for creating training data from performed steps
  * @param collisionCost amount of energy loss for a collision
  */
class DRLAgent(model: Model,
               replayMemoryManager: ReplayMemoryManager,
               trainDataConverter: TrainDataConverter,
               collisionCost: Int = 40) {

  private val trainListeners = List(new PerformanceListener(1000))

  private val replayMemory = new mutable.Queue[StateTransition]

  private var lastMove: Option[XY] = None

  private var lastState: Option[State] = None

  private var botEnergy: Int = 0

  private var collisionCount: Int = 0

  private var round: Int = 0

  private def reset = {
    this.botEnergy = 1000
    this.collisionCount = 0
    this.lastMove = None
    this.lastState = None
  }


  def newRound(roundNo: Int, maxStepCount: Int): Unit = {
    reset
    this.round = roundNo
  }

  def endRound = {
    println(s"Collision count: $collisionCount")
    println(s"Replay memory size: ${replayMemory.size}")
    trainReplay

    if (round % 50 == 0){
      val timestamp = System.currentTimeMillis
      val modelFile = new File(System.getProperty("java.io.tmpdir"), s"scalatron-dqn-network-$timestamp-$round.zip")
      println(s"Saving model: $modelFile")
      model.getNetwork.save(modelFile, true)
    }

  }



  def predictMove(state: State): XY = {
    val input = reshape(state)
    try {
      val prediction = model.predict(input)
      //val amax = prediction.amax(1)
      val maxElem = prediction.toDoubleVector.zipWithIndex.maxBy(_._1)
      XY.fromDirection45(maxElem._2)
    } catch {
      case ex: Exception => {
        ex.printStackTrace()
        throw ex
      }
    }
  }

  private def calcReward(botEnergy: Int): Int = {
    val reward = botEnergy - this.botEnergy
    this.botEnergy = botEnergy
    reward
  }


  def remember(state: State, move: XY, stepCount: Int, botEnergy: Int, collision: Option[XY]) {
    val reward = calcReward(botEnergy) - collision.map(_ => collisionCost).getOrElse(0)

    if (collision.isDefined) {
      collisionCount += 1
      //println(s"Collision: $collision")
    }
    if (reward > 0) {
      println(s"Step($stepCount) REWARD: $reward")
    }

    if (lastState.isDefined && lastMove.isDefined) {
      val transition = StateTransition(lastState.get, lastMove.get, state, reward)
      replayMemoryManager.transfer(transition, t => {
        replayMemory.enqueue(t)
        train(t)
      })
    }

    lastState = Some(state)
    lastMove = Some(move)
  }

  private def toTrainData(stateTransition: StateTransition) = trainDataConverter.toTrainData(stateTransition, model.predict)

  private def train(stateTransition: StateTransition) {
    val trainData = toTrainData(stateTransition)
    try {
      model.fit(trainData, 1, trainListeners)
    } catch {
      case ex: Exception => {
        ex.printStackTrace()
        throw ex
      }
    }
  }

  private def trainReplay {
    if (replayMemory.size < 1) return

    val minibatch = replayMemory.size match {
      case x: Int if (x > 1000) => Random.shuffle(replayMemory).take(1000)
      case _ => replayMemory
    }

    //minibatch.foreach(train)

    import scala.collection.JavaConverters._

    val trainData = minibatch.map(toTrainData).asJava
    val trainDataIter = new ListDataSetIterator[DataSet](trainData, 10)
    model.fit(trainDataIter, 1, trainListeners)

    //println(model.toJson)
  }
}
