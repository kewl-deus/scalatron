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

import scala.collection.mutable
import scala.util.Random

/**
  * Bot backend
  */
class DQNAgent(val directions: Int, val obstacleTypes: Int) {

  private val learningRate = 0.0005

  private val discountRate = 0.9

  private val model = createNetwork(directions, obstacleTypes)

  private val trainListeners = List(new PerformanceListener(1000))

  private var replayMemory = new mutable.Queue[StateTransition]

  private var lastMove: XY = XY.Zero

  private var lastState: State = Nil

  private var stepCounter: Int = 0

  private var botEnergy: Int = 0

  def reset(botEnergy: Int) = {
    this.botEnergy = botEnergy
    this.lastMove = XY.Zero
    this.lastState = Nil
  }

  def update(stepCount: Int, botEnergy: Int): Boolean = {
    val newRound = stepCount <= stepCounter
    if (newRound) {
      reset(botEnergy)
      trainReplay
    }
    stepCounter = stepCount
    newRound
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

    println(model.toJson)

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

  def remember(state: State, move: XY, botEnergy: Int) {
    val reward = calcReward(botEnergy)

    if (reward != 0) println(s"REWARD: $reward")

    if (!lastState.isEmpty) {
      val transition = StateTransition(lastState, move, state, reward)
      remember(transition)
      train(transition)
    }
    lastState = state
    lastMove = move
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

  private def toTrainData(stateTransition: StateTransition) = {
    val amax = model.predict(reshape(stateTransition.newState)).amaxNumber().doubleValue()
    val targetValue = stateTransition.reward + discountRate * amax

    val target = model.predict(reshape(stateTransition.state))
    val index = stateTransition.move.toDirection45
    target.put(0, index, targetValue)

    new DataSet(reshape(stateTransition.state), target)
  }

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
    model.fit(trainDataIter, 10, trainListeners)

    println(model.toJson)

  }
}
