package bots.aibot

import org.deeplearning4j.nn.conf.Updater
import org.deeplearning4j.scalnet.layers.core.Dense
import org.deeplearning4j.scalnet.models.Sequential
import org.nd4j.linalg.activations.Activation
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction

object DRLModels {

  def createNetwork(directions: Int, obstacleTypes: Int) = {

    val learningRate = 0.0005

    val numInputs = directions * obstacleTypes
    val numHidden = 120
    val numOutputs = directions
    val dropoutRate = 0.15
    val hiddenActivation = Activation.RELU //Activation.LEAKYRELU
    val hiddenWeightInit = WeightInit.NORMAL //WeightInit.VAR_SCALING_NORMAL_FAN_AVG

    val model = Sequential()
    model.add(Dense(name = "inputlayer", nIn = numInputs, nOut = numHidden, activation = hiddenActivation, weightInit = hiddenWeightInit, dropOut = dropoutRate))
    //model.add(Dropout(nOut = 120, rate = 0.15))
    model.add(Dense(name = "hidden-1", nOut = numHidden, activation = hiddenActivation, weightInit = hiddenWeightInit, dropOut = dropoutRate))
    //model.add(Dropout(nOut = 120, rate = 0.15))
    model.add(Dense(name = "hidden-2", nOut = numHidden, activation = hiddenActivation, weightInit = hiddenWeightInit, dropOut = dropoutRate))
    //model.add(Dropout(nOut = 120, rate = 0.15))
    model.add(Dense(name = "outputlayer", nOut = numOutputs, activation = Activation.SOFTMAX))
    model.compile(lossFunction = LossFunction.MSE, updater = Updater.ADAM)

    //println(model.toJson)

    //return value
    model
  }
}
