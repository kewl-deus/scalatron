package bots.aibot

import bots.aibot.Globals.State
import bots.framework.XY

case class StateTransition(state: State, move: XY, newState: State, reward: Int) {

}
