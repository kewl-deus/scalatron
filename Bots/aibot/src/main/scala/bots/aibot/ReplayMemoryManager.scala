package bots.aibot

import scala.collection.mutable

/**
  * Strategy for storing replay data into agents memory.
  *
  * @param memoryRewardThreshold minimum absolute reward for storing transition in replayMemory
  */
abstract class ReplayMemoryManager(memoryRewardThreshold: Int) {

  /**
    *
    * @param transition input to be stored
    * @param remember   store function to be called in order to save transition
    */
  def transfer(transition: StateTransition, remember: StateTransition => Unit)

  def accept(stateTransition: StateTransition): Boolean = Math.abs(stateTransition.reward) >= memoryRewardThreshold
}

class ShortTermMemory(memoryRewardThreshold: Int) extends ReplayMemoryManager(memoryRewardThreshold) {

  private val shortTermMemory = new mutable.Queue[StateTransition]

  override def transfer(transition: StateTransition, remember: StateTransition => Unit) {
    //remember all transitions
    shortTermMemory.enqueue(transition)

    //wait until we got one with a reward
    if (transition.reward != 0) {
      //distribute reward of last captured transition over all previous steps
      transferShortTermMemory(transition.reward, remember)
    }
  }


  private def transferShortTermMemory(reward: Int, remember: StateTransition => Unit) {
    val stepReward: Float = reward.floatValue() / shortTermMemory.size

    var partialReward: Int = 0
    while (!shortTermMemory.isEmpty) {
      val t = shortTermMemory.dequeue()
      partialReward += Math.round(stepReward)
      val rewardedTransition = new StateTransition(t.state, t.action, t.newState, partialReward)
      if (accept(rewardedTransition)) remember(rewardedTransition)
    }
  }

}

class DirectTransfer(memoryRewardThreshold: Int) extends ReplayMemoryManager(memoryRewardThreshold) {
  override def transfer(transition: StateTransition, remember: StateTransition => Unit) {
    if (accept(transition)) remember(transition)
  }
}
