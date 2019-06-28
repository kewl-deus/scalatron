package bots.aibot

import bots.framework.XY
import org.deeplearning4j.nn.conf.Updater
import org.deeplearning4j.scalnet.layers.core.{Dense, Dropout}
import org.deeplearning4j.scalnet.models.Sequential
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.learning.config.Adam
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction

/**
  * Bot backend
  */
class DQNAgent(val directions: Int, val obstacleTypes: Int) {

  private lazy val model = createNetwork(directions, obstacleTypes)

  private def createNetwork(directions: Int, obstacleTypes: Int) = {

    val numInputs = directions * obstacleTypes
    val numHidden = 120
    val numOutputs = directions
    val learningRate = 0.0005
    val dropoutRate = 0.15

    val model = Sequential()
    model.add(Dense(name = "inputlayer", nIn = numInputs, nOut = numHidden, activation = Activation.RELU, dropOut = dropoutRate))
    //model.add(Dropout(nOut = 120, rate = 0.15))
    model.add(Dense(name = "hidden-1", nOut = numHidden, activation = Activation.RELU, dropOut = dropoutRate))
    //model.add(Dropout(nOut = 120, rate = 0.15))
    model.add(Dense(name = "hidden-2",nOut = numHidden, activation = Activation.RELU, dropOut = dropoutRate))
    //model.add(Dropout(nOut = 120, rate = 0.15))
    model.add(Dense(name = "outputlayer", nOut = numOutputs, activation = Activation.SOFTMAX))
    model.compile(lossFunction = LossFunction.MSE, updater = Updater.ADAM)

    //return value
    model
  }

  def predictMove(state: Seq[Int]): XY = {
    XY.Zero
  }

}