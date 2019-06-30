package bots.aibot

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.dataset.DataSet
import org.nd4j.linalg.factory.Nd4j
import DataStructureUtils.reshape
import bots.framework.Direction45

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
    target.put(0, index, targetValue)

    new DataSet(reshape(stateTransition.state), target)
  }
}

class SimpleTrainDataConverter(rewardAdjustmentBase: Int) extends TrainDataConverter(rewardAdjustmentBase) {

  override def toTrainData(stateTransition: StateTransition, predict: INDArray => INDArray): DataSet = {
    val target = predict(reshape(stateTransition.state))

    if (stateTransition.reward != 0) {
      val index = stateTransition.action.toDirection45

      val targetValue = target.getDouble(0L, index.longValue())
      val reward: Double = stateTransition.reward.doubleValue() / rewardAdjustmentBase.doubleValue()

      target.put(0, index, targetValue + reward)
    }

    new DataSet(reshape(stateTransition.state), target)
  }

}

class VerySimpleTrainDataConverter(rewardAdjustmentBase: Int) extends TrainDataConverter(rewardAdjustmentBase) {
  override def toTrainData(stateTransition: StateTransition, predict: INDArray => INDArray): DataSet = {
    val index = stateTransition.action.toDirection45
    val target = Direction45.ALL.map(dir => if (dir == index) stateTransition.reward.doubleValue() / rewardAdjustmentBase.doubleValue() else 0d)
    val labels = Nd4j.create(target.toArray, Array(1, target.length))
    new DataSet(reshape(stateTransition.state), labels)
  }
}
