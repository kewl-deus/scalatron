package bots.aibot

import bots.aibot.DataStructureUtils.State
import bots.framework.XY

case class StateTransition(state: State, action: XY, newState: State, reward: Int) {

}
