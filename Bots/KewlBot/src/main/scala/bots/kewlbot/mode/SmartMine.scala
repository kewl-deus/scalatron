package bots.kewlbot.mode

import bots.kewlbot.util.{Pointer, ViewSpace}
import bots.kewlbot.kewlbot._
import bots.kewlbot.cmd._

/**
 * Look for Snorts and destroy them by explosion
 */
case class SmartMine(name: String) extends SlaveBot {

  def react(view: ViewSpace, params: CommandParameters) = {

    commandStack = new CommandChain()

    val enemies = List(Snorg, EnemyBot, EnemySlave)

    val enemyOffsets = enemies.map(view.offsetToNearest).flatten

    if (!enemyOffsets.isEmpty) {

      val enemyPointers = enemyOffsets.map(e => Pointer(e, view.center.distanceTo(e).toInt))

      val nearestEnemy = enemyPointers.minBy(_.distance)

      if (nearestEnemy.distance > 20) {
        commandStack | Status("Beep! Beep!") | Move(nearestEnemy.direction)
        //move(nearestEnemy.direction, view)
      } else {
        commandStack | Say("Boooooooooom!") | Explode(8)
      }
    }

    commandStack
    /*
    nearestEnemy match {
      case Some(xy) => {
        Say("Boooooooooom!") | Explode(8)
      }
      case _ => Status("Beep! Beep!")
    }
    */
  }
}
