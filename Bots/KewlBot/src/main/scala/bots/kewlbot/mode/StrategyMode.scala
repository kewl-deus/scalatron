package bots.kewlbot.mode

import bots.kewlbot.SlaveBotRegistry
import bots.kewlbot.cmd.{BotInstruction, CommandChain, Spawn, Status}
import bots.kewlbot.util.{Pointer, ViewSpace}
import bots.kewlbot.kewlbot._

object StrategyMode extends MasterBot {

  private val harvestingMode = new HarvestingMode()

  private var lastMineReleased = 0


  def react(view: ViewSpace, params: CommandParameters) = {

    commandStack = new CommandChain()

    //    val energy = params("energy").toInt
    //    for (i <- 1 to energy % 200) {
    //      instructions |= spawnBodyguard(200)
    //    }

    commandStack | spawnSmartMine(view, params)

    val route = harvestingMode.calculateRouteToFood(view)

    commandStack | move(route.direction, view)

    if (route.distance == 31) {
      ModeTransition(commandStack, new MoveMode(route.distance, route.destination, this))
    } else {
      ModeTransition(commandStack, this)
    }

  }


  def spawnSmartMine(view: ViewSpace, params: CommandParameters) = {
    val instructions: BotInstruction = new CommandChain()

    val time = params("time").toInt
    val masterEnergy = params("energy").toInt

    val enemies = List(Snorg, EnemyBot, EnemySlave)

    val enemyOffsets = enemies.map(view.offsetToNearest).flatten

    if (! enemyOffsets.isEmpty){
      val enemyPointers = enemyOffsets.map(e => Pointer(e,  view.center.distanceTo(e).toInt))

      val nearestEnemy = enemyPointers.minBy(_.distance)
      val enemyCount = view.cells.count(c => enemies.contains(c))
      val enemyDistance = nearestEnemy.distance
      val timeSinceLastMine = time - lastMineReleased

      if (
        (enemyCount > 2 && enemyDistance < 30 && timeSinceLastMine > 40) ||
          (enemyCount > 4 && enemyDistance < 15 && timeSinceLastMine > 15) ||
          (enemyCount > 2 && enemyDistance < 15 && timeSinceLastMine > 10 && masterEnergy > 2000)
      ) {
        lastMineReleased = time
        val smartMine = SmartMine("SmartMine" + System.currentTimeMillis)
        SlaveBotRegistry.register(smartMine)
        instructions | Status("I smell foes!") | Spawn(-1, -1, smartMine.name, 200)
      }
    }

    instructions
  }

  def spawnBodyguard(energy: Int) = {
    val guard = Bodyguard("Bodyguard" + System.currentTimeMillis)
    SlaveBotRegistry.register(guard)
    Spawn(-1, -1, guard.name, energy)
  }
}
