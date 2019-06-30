package bots.aibot

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.dataset.DataSet
import org.nd4j.linalg.factory.Nd4j
import DataStructureUtils.reshape
import bots.framework.Direction45

import scala.util.Random

abstract class TrainDataConverter(rewardAdjustmentBase: Int) {

  def toTrainData(stateTransition: StateTransition, predict: INDArray => INDArray): DataSet
}

/**
  * Markov Decision
  *
  * @param rewardAdjustmentBase
  * @param discountRate gamma
  */
class MarkovTrainDataConverter(rewardAdjustmentBase: Int, val discountRate: Double = 0.9) extends TrainDataConverter(rewardAdjustmentBase) {

  override def toTrainData(stateTransition: StateTransition, predict: INDArray => INDArray): DataSet = {
    val amax = predict(reshape(stateTransition.newState)).amaxNumber().doubleValue()
    val targetValue = (stateTransition.reward.doubleValue() / rewardAdjustmentBase.doubleValue()) + discountRate * amax

    val target = predict(reshape(stateTransition.state))
    val index = stateTransition.action.toDirection45
    target.put(0, index, targetValue.max(0))

    new DataSet(reshape(stateTransition.state), target)
  }
}

class PredictionRewardAdjustmentDataConverter(rewardAdjustmentBase: Int) extends TrainDataConverter(rewardAdjustmentBase) {

  override def toTrainData(stateTransition: StateTransition, predict: INDArray => INDArray): DataSet = {
    val target = predict(reshape(stateTransition.state))

    if (stateTransition.reward != 0) {
      val index = stateTransition.action.toDirection45

      /*
      val targetValue = target.getDouble(0L, index.longValue())
      val reward: Double = stateTransition.reward.doubleValue() / rewardAdjustmentBase.doubleValue()
      target.put(0, index, targetValue + reward)
      */

      target.put(0, index, if (stateTransition.reward > 0) 1d else 0d)
    }

    new DataSet(reshape(stateTransition.state), target)
  }

}

class DirectRewardLastMoveDataConverter(rewardAdjustmentBase: Int) extends TrainDataConverter(rewardAdjustmentBase) {

  private def zero(): Double = {
    0d
  }

  override def toTrainData(stateTransition: StateTransition, predict: INDArray => INDArray): DataSet = {

    def createTarget(direction: Int, valueMatching: Double, valueOthers: () => Double) = {
      Direction45.ALL.map(dir => if (dir == direction) valueMatching else valueOthers())
    }

    def createRandomTargetButNotDirection(direction: Int) = {
      var rand = direction
      while (rand == direction) {
        rand = Random.nextInt(Direction45.ALL.size)
      }
      Direction45.ALL.map(dir => if (dir == rand) Random.nextDouble() else 0d)
    }

    val dirIndex = stateTransition.action.toDirection45


    val target = stateTransition.reward match {
      case 0 => createTarget(dirIndex, Random.nextDouble(), zero)
      case reward: Int if (reward > 0) => createTarget(dirIndex, 1d, zero) //reward.doubleValue() / rewardAdjustmentBase.doubleValue()
      case reward: Int if (reward < 0) => createRandomTargetButNotDirection(dirIndex) //createTarget(dirIndex, 0d, Random.nextDouble)
    }

    val labels = Nd4j.create(target.toArray, Array(1, target.length))
    new DataSet(reshape(stateTransition.state), labels)
  }
}
