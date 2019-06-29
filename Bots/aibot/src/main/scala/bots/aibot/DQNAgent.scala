package bots.aibot

import bots.aibot.Globals.State
import bots.framework.XY
import org.deeplearning4j.datasets.iterator.impl.ListDataSetIterator
import org.deeplearning4j.nn.conf.Updater
import org.deeplearning4j.optimize.listeners.PerformanceListener
import org.deeplearning4j.scalnet.layers.core.Dense
import org.deeplearning4j.scalnet.models.Sequential
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.dataset.DataSet
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction

import scala.collection.{immutable, mutable}
import scala.util.Random

/**
  * Bot backend
  */
class DQNAgent(val directions: Int, val obstacleTypes: Int) {

  val learningRate = 0.0005

  /** gamma */
  val discountRate = 0.9

  val collisionCost = -40


  private val model = createNetwork(directions, obstacleTypes)

  private val trainListeners = List(new PerformanceListener(1000))

  private val replayMemory = new mutable.Queue[StateTransition]

  private var lastMove: Option[XY] = None

  private var lastState: Option[State] = None

  private var botEnergy: Int = 0

  private def reset = {
    this.botEnergy = 1000
    this.lastMove = None
    this.lastState = None
  }

  def newRound(roundNo: Int, maxStepCount: Int): Unit = {
    reset
  }

  def endRound = {
    trainReplay
  }

  private def createNetwork(directions: Int, obstacleTypes: Int) = {

    val numInputs = directions * obstacleTypes
    val numHidden = 120
    val numOutputs = directions
    val dropoutRate = 0.15

    val model = Sequential()
    model.add(Dense(name = "inputlayer", nIn = numInputs, nOut = numHidden, activation = Activation.RELU, dropOut = dropoutRate))
    //model.add(Dropout(nOut = 120, rate = 0.15))
    model.add(Dense(name = "hidden-1", nOut = numHidden, activation = Activation.RELU, dropOut = dropoutRate))
    //model.add(Dropout(nOut = 120, rate = 0.15))
    model.add(Dense(name = "hidden-2", nOut = numHidden, activation = Activation.RELU, dropOut = dropoutRate))
    //model.add(Dropout(nOut = 120, rate = 0.15))
    model.add(Dense(name = "outputlayer", nOut = numOutputs, activation = Activation.SOFTMAX))
    model.compile(lossFunction = LossFunction.MSE, updater = Updater.ADAM)

    //println(model.toJson)

    //return value
    model
  }

  private def reshape(state: State) = Nd4j.create(state.toArray, Array(1, state.length)) //rows, columns

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

  private def remember(stateTransition: StateTransition) {
    replayMemory.enqueue(stateTransition)
  }

  def remember(state: State, move: XY, stepCount: Int, botEnergy: Int, collision: Option[XY]) {

    val reward = calcReward(botEnergy) + collision.map(_ => collisionCost).getOrElse(0)

    //if (collision.isDefined) println(s"Collision: $collision")
    if (reward > 0) println(s"Step($stepCount) REWARD: $reward")

    if (reward != 0 && lastState.isDefined) {
      val transition = StateTransition(lastState.get, move, state, reward)
      remember(transition)
      train(transition)
    }
    lastState = Some(state)
    lastMove = Some(move)
  }

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

  private def toTrainData(stateTransition: StateTransition) = toTrainDataMarkov(stateTransition)

  private def toTrainDataMarkov(stateTransition: StateTransition) = {
    val amax = model.predict(reshape(stateTransition.newState)).amaxNumber().doubleValue()
    val targetValue = (stateTransition.reward.doubleValue() / 1000d) + discountRate * amax

    val target = model.predict(reshape(stateTransition.state))
    val index = stateTransition.action.toDirection45
    target.put(0, index, targetValue)

    new DataSet(reshape(stateTransition.state), target)
  }


  private def toTrainDataSimple(stateTransition: StateTransition) = {
    val target = model.predict(reshape(stateTransition.state))

    if (stateTransition.reward != 0) {
      val index = stateTransition.action.toDirection45

      val targetValue = target.getDouble(0L, index.longValue())
      val reward: Double = stateTransition.reward.doubleValue() / 1000d

      target.put(0, index, targetValue + reward)
    }

    new DataSet(reshape(stateTransition.state), target)

  }

  /*
    private def toTrainDataSimple(stateTransition: StateTransition) = {
    //val predictedTarget = model.predict(reshape(stateTransition.state))

    val reward: Double = if (stateTransition.reward == 0) 1d else stateTransition.reward.doubleValue()
    val index = stateTransition.action.toDirection45
    val target = Globals.directions.map(dir => if (dir == index) reward else 0)
    val labels = Nd4j.create(target.toArray, Array(1, target.length))
    new DataSet(reshape(stateTransition.state), labels)

  }
   */

  private def trainReplay {
    if (replayMemory.size <= 0) return

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
