package bots.aibot

import ch.qos.logback.classic.Level
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.conf.{NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.deeplearning4j.scalnet.layers.core.{Dense, Layer}
import org.deeplearning4j.scalnet.models.Sequential
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.learning.config.{Adam, IUpdater}
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction
import org.slf4j.LoggerFactory

object DRLModels {

  def createNetwork(directions: Int, obstacleTypes: Int) = {

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

  def createCustomNetwork(directions: Int, obstacleTypes: Int) = {

    val learningRate = 0.0005

    val numInputs = directions * obstacleTypes
    val numHidden = 120
    val numOutputs = directions
    val dropoutRate = 0.15
    val hiddenActivation = Activation.RELU //Activation.LEAKYRELU
    val hiddenWeightInit = WeightInit.NORMAL //WeightInit.VAR_SCALING_NORMAL_FAN_AVG

    val model = new LRASequential()
    model.add(Dense(name = "inputlayer", nIn = numInputs, nOut = numHidden, activation = hiddenActivation, weightInit = hiddenWeightInit, dropOut = dropoutRate))
    model.add(Dense(name = "hidden-1", nOut = numHidden, activation = hiddenActivation, weightInit = hiddenWeightInit, dropOut = dropoutRate))
    model.add(Dense(name = "hidden-2", nOut = numHidden, activation = hiddenActivation, weightInit = hiddenWeightInit, dropOut = dropoutRate))
    model.add(Dense(name = "outputlayer", nOut = numOutputs, activation = Activation.SOFTMAX))
    model.compile(lossFunction = LossFunction.MSE, optimizer = OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT, updater = new Adam(learningRate))

    //println(model.toJson)

    //return value
    model
  }
}

/**
  * Learning rate adjustable Sequential-Network-Model
  * @param miniBatch
  * @param biasInit
  * @param rngSeed
  */
class LRASequential(miniBatch: Boolean = true, biasInit: Double = 0.0, rngSeed: Long = 0) extends Sequential(miniBatch, biasInit, rngSeed) {

  LoggerFactory
    .getLogger(classOf[LRASequential])
    .asInstanceOf[ch.qos.logback.classic.Logger]
    .setLevel(Level.WARN)

  def compile(lossFunction: LossFunction,
              optimizer: OptimizationAlgorithm,
              updater: IUpdater): Unit = {
    val builder = buildModelConfig(optimizer, updater, miniBatch, biasInit, rngSeed)
    buildOutput(lossFunction)

    var listBuilder: NeuralNetConfiguration.ListBuilder = builder.list()
    for ((layer, layerIndex) <- layers.zipWithIndex) {
      logger.info("Layer " + layerIndex + ": " + layer.getClass.getSimpleName)
      logger.info(" size: " + layer.describe())
      listBuilder.layer(layerIndex, layer.asInstanceOf[Layer].compile)
    }

    model = new MultiLayerNetwork(listBuilder.build())
    model.init()
  }

  def buildModelConfig(optimizer: OptimizationAlgorithm,
                       updater: IUpdater,
                       miniBatch: Boolean,
                       biasInit: Double,
                       seed: Long): NeuralNetConfiguration.Builder = {
    var builder: NeuralNetConfiguration.Builder = new NeuralNetConfiguration.Builder()
    if (seed != 0) {
      builder = builder.seed(seed)
    }
    builder
      .optimizationAlgo(optimizer)
      .updater(updater)
      .miniBatch(miniBatch)
      .biasInit(biasInit)
  }
}